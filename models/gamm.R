
#=================================================================================================
# GENERALISED ADDITIVE MIXED MODEL (GAMM) 
#=================================================================================================

# ============================================================================
# GAMM.R — OFFLINE GAMM fitting (corARMA grid search per region)
# Run standalone: source("models/helpers.R"); then this script.
# Produces model_<PFX>_gamm.rds, model_<PFX>_gamm_meta.rds,
# gamm_corarma_grid_comparison_<PFX>.csv files.
# ============================================================================

source("models/helpers.R")

if (!exists("data")) stop("`data` not found. Source your data-prep pipeline first.")

region_config <- list(
  "Upper East"    = list(pfx = "UE", k_time = 13, terms = c("rainfall","avgtemp"), pq_max = 2,
                         niterPQL = 50, niterEM = 50, maxIter = 100, msMaxIter = 100),
  "Upper West"    = list(pfx = "UW", k_time = 34, terms = c("rainfall", "avgtemp"), pq_max = 2,
                         niterPQL = 50, niterEM = 50, maxIter = 100, msMaxIter = 100),
  "Northern"      = list(pfx = "NO", k_time = 16, terms = c("rainfall"), pq_max = 2,
                         niterPQL = 50, niterEM = 50, maxIter = 100, msMaxIter = 100),
  "Brong Ahafo"   = list(pfx = "BA", k_time = 17, terms = c("avgtemp"), pq_max = 2,
                         niterPQL = 50, niterEM = 50, maxIter = 100, msMaxIter = 100),
  "Ashanti"       = list(pfx = "AS", k_time = 18, terms = c("rainfall","avgtemp"), pq_max = 2,
                         niterPQL = 50, niterEM = 50, maxIter = 100, msMaxIter = 100),
  "Eastern"       = list(pfx = "EA", k_time = 37, terms = c("rainfall","avgtemp"), pq_max = 2,
                         niterPQL = 50, niterEM = 50, maxIter = 100, msMaxIter = 100),
  "Volta"         = list(pfx = "VO", k_time = 18, terms = c("avgtemp"), pq_max = 3,
                         niterPQL = 150, niterEM = 150, maxIter = 100, msMaxIter = 100),
  "Greater Accra" = list(pfx = "GA", k_time = 18, terms = c("rainfall"), pq_max = 2,
                         niterPQL = 100, niterEM = 100, maxIter = 100, msMaxIter = 100),
  "Central"       = list(pfx = "CE", k_time = 18, terms = c("rainfall","avgtemp"), pq_max = 2,
                         niterPQL = 50, niterEM = 50, maxIter = 100, msMaxIter = 100),
  "Western"       = list(pfx = "WE", k_time = 19, terms = c("rainfall","avgtemp"), pq_max = 2,
                         niterPQL = 50, niterEM = 50, maxIter = 100, msMaxIter = 100)
)

build_nb_formula <- function(k_time, terms) {
  smooth_terms <- c(
    sprintf("s(time, k = %d)", k_time), "s(months)",
    if ("rainfall" %in% terms) "s(rainfall)",
    if ("avgtemp"  %in% terms) "s(avgtemp)",
    "ti(time, months)", "ti(avgtemp, rainfall)"
  )
  as.formula(paste("uncom ~", paste(smooth_terms, collapse = " + ")))
}

build_gamm_formula <- function(k_time, terms) {
  smooth_terms <- c(
    "offset(log_pop_offset)",
    sprintf("s(time, k = %d, bs = 'cr')", k_time),
    "s(months, bs = 'cc')",
    if ("rainfall" %in% terms) "s(rainfall, bs = 'cr')",
    if ("avgtemp"  %in% terms) "s(avgtemp, bs = 'cr')",
    "ti(time, months, bs = c('cr','cc'))",
    "ti(avgtemp, rainfall, bs = c('cr','cr'))"
  )
  as.formula(paste("uncom ~", paste(smooth_terms, collapse = " + ")))
}

fit_region_gamm <- function(region, cfg, data) {
  cat(sprintf("\n==================== %s ====================\n", region))
  
  dat <- data[data$region == region, ]
  
  nb_formula <- build_nb_formula(cfg$k_time, cfg$terms)
  model_nb   <- gam(nb_formula, data = dat, family = nb(), offset = log_pop_offset, method = "REML")
  theta_hat  <- model_nb$family$getTheta(TRUE)
  cat(sprintf("Estimated theta from gam() NB fit: %.4f\n", theta_hat))
  
  dat <- dat %>% arrange(time)
  dat$g <- factor(1)
  gamm_formula <- build_gamm_formula(cfg$k_time, cfg$terms)
  
  pq_grid      <- expand.grid(p = 0:cfg$pq_max, q = 0:cfg$pq_max)
  gamm_fits    <- list()
  pql_warnings <- list()
  
  for (i in seq_len(nrow(pq_grid))) {
    p <- pq_grid$p[i]; q <- pq_grid$q[i]
    label <- paste0("p", p, "_q", q)
    cat(sprintf("  Fitting corARMA(p=%d, q=%d) ...\n", p, q))
    
    pql_warned <- FALSE
    fit_call <- function() {
      if (p == 0 && q == 0) {
        gamm(gamm_formula, data = dat, family = negative.binomial(theta = theta_hat), niterPQL = cfg$niterPQL)
      } else {
        gamm(gamm_formula, data = dat, family = negative.binomial(theta = theta_hat),
             correlation = corARMA(form = ~ time | g, p = p, q = q),
             control = lmeControl(maxIter = cfg$maxIter, msMaxIter = cfg$msMaxIter, niterEM = cfg$niterEM),
             niterPQL = cfg$niterPQL)
      }
    }
    
    fit <- tryCatch({
      withCallingHandlers(
        fit_call(),
        warning = function(w) {
          if (grepl("niterPQL|not converged", conditionMessage(w))) pql_warned <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
    }, error = function(e) { cat(sprintf("    FAILED: %s\n", conditionMessage(e))); NULL })
    
    gamm_fits[[label]]    <- fit
    pql_warnings[[label]] <- pql_warned
  }
  
  converged    <- !sapply(gamm_fits, is.null)
  gamm_fits    <- gamm_fits[converged]
  pql_warnings <- pql_warnings[converged]
  pql_flag     <- unlist(pql_warnings[names(gamm_fits)])
  
  if (length(gamm_fits) == 0) {
    warning(sprintf("No corARMA structure converged for %s — skipping.", region))
    return(invisible(NULL))
  }
  
  lme_list    <- lapply(gamm_fits, function(x) x$lme)
  adj_r2_list <- sapply(gamm_fits, function(x) tryCatch(summary(x$gam)$r.sq, error = function(e) NA_real_))
  
  parsed_pq <- do.call(rbind, lapply(names(gamm_fits), function(nm) {
    nums <- as.integer(regmatches(nm, gregexpr("[0-9]+", nm))[[1]])
    data.frame(p = nums[1], q = nums[2])
  }))
  
  lme_comparison <- data.frame(
    model = names(gamm_fits), p = parsed_pq$p, q = parsed_pq$q,
    npar = sapply(lme_list, function(x) attr(logLik(x), "df")),
    logLik = sapply(lme_list, function(x) as.numeric(logLik(x))),
    AIC = sapply(lme_list, AIC), BIC = sapply(lme_list, BIC),
    Adj_R2 = adj_r2_list, pql_converged = !pql_flag
  ) %>% arrange(AIC)
  rownames(lme_comparison) <- NULL
  
  best_candidates <- lme_comparison %>% dplyr::filter(pql_converged)
  if (nrow(best_candidates) == 0) {
    cat("  WARNING: no structure fully converged under PQL; falling back to lowest AIC overall.\n")
    best_candidates <- lme_comparison
  }
  best_label <- (best_candidates %>% arrange(AIC) %>% slice(1))$model
  best_gamm  <- gamm_fits[[best_label]]
  cat(sprintf("  Best model by AIC among fully PQL-converged fits: %s\n", best_label))
  
  phi_est <- tryCatch(coef(best_gamm$lme$modelStruct$corStruct, unconstrained = FALSE), error = function(e) NA)
  resid_norm <- residuals(best_gamm$lme, type = "normalized")
  ljung_box <- lapply(c(12, 24, 36, 48, 60), function(lag) {
    bt <- Box.test(resid_norm, lag = lag, type = "Ljung-Box")
    data.frame(lag = lag, statistic = unname(bt$statistic), df = unname(bt$parameter), p_value = bt$p.value)
  }) %>% bind_rows()
  
  saveRDS(best_gamm, file = paste0("model_", cfg$pfx, "_gamm.rds"))
  write.csv(lme_comparison, paste0("gamm_corarma_grid_comparison_", cfg$pfx, ".csv"), row.names = FALSE)
  saveRDS(list(best_label = best_label, phi = phi_est, ljung_box = ljung_box, theta = theta_hat),
          file = paste0("model_", cfg$pfx, "_gamm_meta.rds"))
  
  cat(sprintf("  Saved model_%s_gamm.rds (AIC = %.1f)\n", cfg$pfx,
              lme_comparison$AIC[lme_comparison$model == best_label]))
  invisible(list(best_gamm = best_gamm, grid = lme_comparison, meta = list(phi = phi_est, ljung_box = ljung_box)))
}

for (region in names(region_config)) {
  fit_region_gamm(region, region_config[[region]], data)
}

cat("\nAll regions processed. Copy the model_*_gamm.rds, model_*_gamm_meta.rds,\n",
    "and gamm_corarma_grid_comparison_*.csv files into your Shiny app's working\n",
    "directory alongside the existing model_*_1..8.rds files.\n")


