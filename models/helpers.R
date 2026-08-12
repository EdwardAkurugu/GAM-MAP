# ============================================================================
# SHARED HELPERS
# Sourced by: gam.R, gamm.R, gam_seasonal_smooth.R, gamm_seasonal_smooth.R,
# and server.R. Edit here only — every script picks up the change.
# ============================================================================

library(mgcv)

# ---- Region metadata lookups ----
pfx_lookup <- c(
  "Upper East" = "UE", "Upper West" = "UW", "Northern" = "NO", "Brong Ahafo" = "BA",
  "Ashanti" = "AS", "Eastern" = "EA", "Volta" = "VO", "Greater Accra" = "GA",
  "Central" = "CE", "Western" = "WE"
)

k_time_lookup <- c(
  "Upper East" = 13, "Upper West" = 34, "Northern" = 16, "Brong Ahafo" = 17,
  "Ashanti" = 18, "Eastern" = 37, "Volta" = 18, "Greater Accra" = 18,
  "Central" = 18, "Western" = 19
)

pq_max_lookup <- c(
  "Upper East" = 2, "Upper West" = 2, "Northern" = 2, "Brong Ahafo" = 2,
  "Ashanti" = 2, "Eastern" = 2, "Volta" = 3, "Greater Accra" = 2,
  "Central" = 2, "Western" = 2
)

gam_regions  <- c("Northern")

gamm_regions <- c("Upper East", "Upper West", "Brong Ahafo", "Ashanti",
                  "Eastern", "Volta", "Greater Accra", "Central", "Western")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# ---- Term / smooth-parsing helpers ----
get_smooth_term_labels <- function(model) {
  tl <- attr(terms(formula(model)), "term.labels")
  tl[grepl("^s\\(|^ti\\(|^te\\(", tl)]
}

get_smooth_vars <- function(term_str) {
  e <- tryCatch(str2lang(term_str), error = function(e) NULL)
  if (is.null(e) || !is.call(e)) return(character(0))
  args <- as.list(e)[-1]
  nm <- names(args)
  if (is.null(nm)) nm <- rep("", length(args))
  pos_args <- args[nm == ""]
  vapply(pos_args, function(a) deparse(a), character(1))
}

get_term_units <- function(model) {
  smooth_terms <- get_smooth_term_labels(model)
  ti_terms <- smooth_terms[grepl("^ti\\(|^te\\(", smooth_terms)]
  s_terms  <- smooth_terms[grepl("^s\\(", smooth_terms)]
  
  ti_units <- list()
  absorbed <- character(0)
  
  for (ti_term in ti_terms) {
    vars <- get_smooth_vars(ti_term)
    if (length(vars) == 0) {
      ti_units[[length(ti_units) + 1]] <- ti_term
      next
    }
    marginals <- s_terms[sapply(s_terms, function(s) {
      s_vars <- get_smooth_vars(s)
      any(vars %in% s_vars)
    })]
    ti_units[[length(ti_units) + 1]] <- unique(c(marginals, ti_term))
    absorbed <- c(absorbed, marginals)
  }
  
  standalone <- s_terms[!s_terms %in% absorbed]
  c(as.list(standalone), ti_units)
}

drop_seasonal_unit <- function(unit_list) {
  is_seasonal <- sapply(unit_list, function(u) any(grepl("^s\\(months", u)))
  idx <- which(is_seasonal)
  if (length(idx) == 0) return(NULL)
  list(reduced_units = unit_list[-idx], dropped_idx = idx[1])
}

fit_units_model <- function(unit_list, reg_data) {
  terms_flat <- unlist(unit_list)
  f <- as.formula(paste("uncom ~ offset(log_pop_offset) +", paste(terms_flat, collapse = " + ")))
  tryCatch(gam(f, data = reg_data, family = "nb", method = "REML"), error = function(e) NULL)
}

worst_concurvity_excluding_self_interactions <- function(m, unit_list) {
  cc <- tryCatch(concurvity(m, full = FALSE)$worst, error = function(e) NULL)
  if (is.null(cc)) return(list(value = NA, culprit_unit_idx = NA))
  
  term_names <- rownames(cc)
  unit_of_term <- function(term_name) {
    idx <- which(sapply(unit_list, function(u) any(sapply(u, function(raw) {
      grepl(gsub("([()])", "\\\\\\1", raw), term_name) ||
        grepl(gsub("([()])", "\\\\\\1", term_name), raw)
    }))))
    if (length(idx) == 0) NA else idx[1]
  }
  term_unit_idx <- sapply(term_names, unit_of_term)
  
  worst_val <- -Inf
  culprit_units <- c(NA, NA)
  for (i in seq_len(nrow(cc) - 1)) {
    for (j in (i + 1):ncol(cc)) {
      ui <- term_unit_idx[i]; uj <- term_unit_idx[j]
      if (!is.na(ui) && !is.na(uj) && ui == uj) next
      val <- cc[i, j]
      if (!is.na(val) && val > worst_val) {
        worst_val <- val
        culprit_units <- c(ui, uj)
      }
    }
  }
  if (!is.finite(worst_val)) return(list(value = NA, culprit_unit_idx = NA))
  list(value = worst_val, culprit_unit_idx = culprit_units)
}

# ---- Model selection / diagnostics helpers ----
get_best_model <- function(models) {
  models <- Filter(Negate(is.null), models)
  if (length(models) == 0) return(NULL)
  idx <- which.max(sapply(models, function(m) summary(m)$r.sq * summary(m)$dev.expl))
  models[[idx]]
}

lb_p_at_lag <- function(model, lag = 12) {
  if (is.null(model)) return(NA_real_)
  resid_dev <- tryCatch(residuals(model, type = "deviance"), error = function(e) NULL)
  if (is.null(resid_dev)) return(NA_real_)
  bt <- tryCatch(Box.test(resid_dev, lag = lag, type = "Ljung-Box"), error = function(e) NULL)
  if (is.null(bt)) NA_real_ else bt$p.value
}

format_corarma_label <- function(raw_label) {
  nums <- as.integer(regmatches(raw_label, gregexpr("[0-9]+", raw_label))[[1]])
  if (length(nums) != 2 || any(is.na(nums))) return(raw_label)
  sprintf("corARMA(%d,%d)", nums[1], nums[2])
}

# ---- Model-status table builder (used by Model Validity tab) ----
build_model_status_table <- function(regions, gam_status_fn, gamm_status_fn,
                                     models_fn, best_model_fn) {
  
  rows <- lapply(regions, function(region) {
    
    gam_status  <- tryCatch(gam_status_fn(region),  error = function(e) NULL)
    gamm_status <- tryCatch(gamm_status_fn(region), error = function(e) NULL)
    
    gam_ok   <- !is.null(gam_status)  && length(gam_status$significant_lags)  == 0
    has_gamm <- !is.null(gamm_status)
    gamm_ok  <- has_gamm && length(gamm_status$significant_lags) == 0
    
    gam_lag_str  <- if (is.null(gam_status))  "N/A"
    else if (gam_ok)  "None" else paste(gam_status$significant_lags, collapse = ", ")
    gamm_lag_str <- if (!has_gamm) "No GAMM fitted"
    else if (gamm_ok) "None" else paste(gamm_status$significant_lags, collapse = ", ")
    
    if (gam_ok) {
      recommended <- "GAM"
      rationale <- "No significant residual autocorrelation in the standalone GAM at any tested lag (12\u201360 months); the simpler model is fully valid for inference and forecasting."
      status_level <- "clean"
    } else if (has_gamm && gamm_ok) {
      recommended <- "GAMM"
      rationale <- paste0("Standalone GAM showed residual autocorrelation (lag(s) ", gam_lag_str,
                          "); the corARMA-corrected GAMM (", format_corarma_label(gamm_status$structure_label %||% ""),
                          ") resolves it \u2014 use the GAMM for inference/forecasting in this region.")
      status_level <- "corrected"
    } else if (has_gamm && !gamm_ok) {
      recommended <- "GAMM (caution)"
      rationale <- paste0("Residual autocorrelation remains in BOTH the GAM (lag(s) ", gam_lag_str,
                          ") and the best-available GAMM (lag(s) ", gamm_lag_str,
                          "). Report the GAMM as the best-available model but interpret coefficient ",
                          "standard errors and forecasts with explicit caution.")
      status_level <- "unresolved"
    } else {
      recommended <- "GAM (caution)"
      rationale <- paste0("Residual autocorrelation remains in the GAM (lag(s) ", gam_lag_str,
                          ") and no GAMM has been fitted for this region ",
                          "(check that model_*_gamm.rds was generated). Interpret with caution.")
      status_level <- "unresolved"
    }
    
    data.frame(
      region                 = region,
      gam_ljung_box          = if (gam_ok) "Pass \u2713" else "Fail \u2717",
      gam_sig_lags           = gam_lag_str,
      gamm_available         = ifelse(has_gamm, "Yes", "No"),
      gamm_ljung_box         = if (!has_gamm) "\u2014" else if (gamm_ok) "Pass \u2713" else "Fail \u2717",
      gamm_sig_lags          = gamm_lag_str,
      gamm_structure         = if (has_gamm) format_corarma_label(gamm_status$structure_label %||% "") else "\u2014",
      recommended_model      = recommended,
      status_level           = status_level,
      rationale              = rationale,
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ---- Misc app-side helpers used by server.R (kept here for a single home) ----
reset_tab_filters <- function(session, data,
                              region_input_id = NULL,
                              default_region = "Upper East",
                              year_input_id = NULL,
                              year_range_input_id = NULL,
                              date_range_input_id = NULL) {
  if (!is.null(region_input_id))
    shiny::updateSelectInput(session, region_input_id, selected = default_region)
  if (!is.null(year_input_id))
    shiny::updateSelectInput(session, year_input_id, selected = min(data$year, na.rm = TRUE))
  if (!is.null(year_range_input_id))
    shiny::updateSliderInput(session, year_range_input_id,
                             value = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)))
  if (!is.null(date_range_input_id))
    shiny::updateDateRangeInput(session, date_range_input_id,
                                start = min(data$date, na.rm = TRUE),
                                end = max(data$date, na.rm = TRUE))
}

get_population_series <- function(rd) {
  if ("log_pop_offset" %in% names(rd)) return(exp(rd$log_pop_offset))
  pop_col <- intersect(c("pop_join", "population", "pop", "pop_offset", "Population"), names(rd))
  if (length(pop_col) > 0) return(rd[[pop_col[1]]])
  rep(NA_real_, nrow(rd))
}

