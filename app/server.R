# ================================================================================================
# GAM(M)-MAP SHINY SERVER
# ================================================================================================

# -------------------------------------------------------------------------------------------------
# OFFLINE: GENERALISED ADDITIVE MODEL (GAM) and GENERALISED ADDITIVE MIXED MODEL
# UNCOMMENT THE SOURCED R FILES TO OBTAIN RDS TO FIT MODELS WHEN RUNNING THE APP FOR THE FIRST TIME
# -------------------------------------------------------------------------------------------------
# source("models/helpers.R")
# source("models/gam.R")
# source("models/gamm.R")
# source("models/gam_seasonal_smooth.R")
# source("models/gamm_seasonal_smooth.R")

# ================================================================================================
# SECTION 1 — SERVER
# ================================================================================================
server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------------
  # INITIALIZATION — loading overlay, model stores, data provenance banner
  # ----------------------------------------------------------------------------------
  session$onFlushed(function() { shinyjs::hide("loading-overlay") }, once = TRUE)
  
  output$data_provenance <- renderText({
    paste0("Data: ",
           format(min(data$date, na.rm = TRUE), "%b %Y"), " \u2013 ",
           format(max(data$date, na.rm = TRUE), "%b %Y"),
           "  |  ", length(unique(data$region)), " regions")
  })
  
  safe_readRDS <- function(path) {
    tryCatch(readRDS(path), error = function(e) {
      showNotification(paste0("Model file missing: ", basename(path)), type = "error")
      NULL
    })
  }
  
  gamm_prefixes <- c("Upper East" = "UE", "Upper West" = "UW", "Northern" = "NO", "Brong Ahafo" = "BA",
                     "Ashanti" = "AS", "Eastern" = "EA", "Volta" = "VO", "Greater Accra" = "GA",
                     "Central" = "CE", "Western" = "WE")
  
  gamm_store <- lapply(gamm_prefixes, function(pfx) safe_readRDS(paste0("model_", pfx, "_gamm.rds")))
  names(gamm_store) <- names(gamm_prefixes)
  
  gamm_meta_store <- lapply(gamm_prefixes, function(pfx) safe_readRDS(paste0("model_", pfx, "_gamm_meta.rds")))
  names(gamm_meta_store) <- names(gamm_prefixes)
  
  gamm_grid_store <- lapply(gamm_prefixes, function(pfx) {
    path <- paste0("gamm_corarma_grid_comparison_", pfx, ".csv")
    if (file.exists(path)) read.csv(path) else NULL
  })
  names(gamm_grid_store) <- names(gamm_prefixes)
  
  get_gamm_model <- function(region) gamm_store[[region]]
  get_gamm_meta  <- function(region) gamm_meta_store[[region]]
  get_gamm_grid  <- function(region) gamm_grid_store[[region]]
  
  model_store <- local({
    regions <- c("Upper East", "Upper West", "Northern", "Brong Ahafo", "Ashanti",
                 "Eastern", "Volta", "Greater Accra", "Central", "Western")
    prefixes <- c("UE", "UW", "NO", "BA", "AS", "EA", "VO", "GA", "CE", "WE")
    store <- setNames(vector("list", length(regions)), regions)
    for (i in seq_along(regions)) {
      store[[regions[i]]] <- lapply(1:8, function(j)
        safe_readRDS(paste0("model_", prefixes[i], "_", j, ".rds")))
    }
    store
  })
  
  get_models <- function(region) model_store[[region]]
  
  get_best_model <- function(models) {
    models <- Filter(Negate(is.null), models)
    if (length(models) == 0) return(NULL)
    idx <- which.max(sapply(models, function(m) summary(m)$r.sq * summary(m)$dev.expl))
    models[[idx]]
  }
  
  get_models_and_best_idx <- function(region) {
    models <- get_models(region)
    models <- Filter(Negate(is.null), models)
    best_idx <- which.max(sapply(models, function(m) summary(m)$r.sq * summary(m)$dev.expl))
    list(models = models, best_idx = best_idx)
  }
  
  get_model_equation <- function(model) {
    paste(trimws(format(formula(model))), collapse = " ")
  }
  
  get_model_estimates <- function(model) {
    pt <- as.data.frame(summary(model)$p.table)
    pt <- data.frame(Term = rownames(pt), round(pt, 4), row.names = NULL)
    colnames(pt) <- c("Term", "Estimate", "Std_Error", "z_value", "p_value")
    
    st <- as.data.frame(summary(model)$s.table)
    st <- data.frame(Term = rownames(st), round(st, 4), row.names = NULL)
    colnames(st) <- c("Term", "edf", "Ref_df", "Chi_sq", "p_value")
    
    list(parametric = pt, smooth = st)
  }
  
  get_model_concurvity <- function(model) {
    overall <- as.data.frame(round(concurvity(model, full = TRUE), 3))
    overall <- data.frame(Statistic = rownames(overall), overall, row.names = NULL)
    
    pw <- concurvity(model, full = FALSE)$estimate
    pairwise <- as.data.frame(round(pw, 3))
    pairwise <- data.frame(Term = rownames(pairwise), pairwise, row.names = NULL)
    
    list(overall = overall, pairwise = pairwise)
  }
  
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
  
  
  # ----------------------------------------------------------------------------------
  # NAVIGATION — "next step" buttons, header help link, step-strip clicks
  # ----------------------------------------------------------------------------------
  observeEvent(input$go_descriptives, { updateTabItems(session, "tabs", "descriptives") })
  observeEvent(input$go_ts,           { updateTabItems(session, "tabs", "timeseries") })
  observeEvent(input$go_season,       { updateTabItems(session, "tabs", "seasonality") })
  observeEvent(input$go_est,          { updateTabItems(session, "tabs", "estimates") })
  observeEvent(input$go_diag,         { updateTabItems(session, "tabs", "diagnostics") })
  observeEvent(input$go_plots,        { updateTabItems(session, "tabs", "plots") })
  observeEvent(input$go_summary,      { updateTabItems(session, "tabs", "regional_summary") })
  observeEvent(input$header_help,     { updateTabItems(session, "tabs", "help") })
  observeEvent(input$go_corr,         { updateTabItems(session, "tabs", "correlation") })
  observeEvent(input$go_forecast,     { updateTabItems(session, "tabs", "forecast") })
  observeEvent(input$go_map,          { updateTabItems(session, "tabs", "map_view") })
  observeEvent(input$go_gdiag,        { updateTabItems(session, "tabs", "gamm_diagnostics") })
  observeEvent(input$go_gplots,       { updateTabItems(session, "tabs", "gamm_plots") })
  observeEvent(input$go_gamm_est,     { updateTabItems(session, "tabs", "gamm_estimates") })
  
  
  # Step progress strip 
  observeEvent(input$step_click, {
    step_map <- c("1" = "descriptives", "2" = "timeseries",  "3" = "seasonality",
                  "4" = "estimates",    "5" = "diagnostics", "6" = "plots", "7" = "regional_summary")
    target <- step_map[input$step_click]
    if (!is.na(target)) updateTabItems(session, "tabs", target)
  })
  
  
  # ==================================================================================
  #  MODEL VALIDITY  (sidebar: "Model Validity")
  #   Tabs: Validity Summary | Concurvity | GAM Seasonal Smooth Effect |
  #         GAMM Seasonal Smooth Effect
  # ==================================================================================
  
  # ---- Validity Summary: GAM vs GAMM Ljung-Box recommendation table ----
  gam_autocorrelation_status <- function(region) {
    bm <- get_best_model(get_models(region))
    if (is.null(bm)) return(NULL)
    
    resid_dev <- residuals(bm, type = "deviance")
    lb <- lapply(c(12, 24, 36, 48, 60), function(lag) {
      bt <- tryCatch(Box.test(resid_dev, lag = lag, type = "Ljung-Box"), error = function(e) NULL)
      if (is.null(bt)) return(data.frame(lag = lag, p_value = NA_real_))
      data.frame(lag = lag, p_value = bt$p.value)
    }) %>% bind_rows()
    
    list(
      significant_lags = lb$lag[!is.na(lb$p_value) & lb$p_value < 0.05],
      table = lb,
      has_gamm = !is.null(get_gamm_model(region))
    )
  }
  
  gamm_autocorrelation_status <- function(region) {
    bg   <- get_gamm_model(region)
    meta <- get_gamm_meta(region)
    if (is.null(bg) || is.null(meta)) return(NULL)
    
    lb <- meta$ljung_box
    list(
      significant_lags = lb$lag[!is.na(lb$p_value) & lb$p_value < 0.05],
      table = lb,
      structure_label = format_corarma_label(meta$best_label),
      gam_status = gam_autocorrelation_status(region)
    )
  }
  
  model_status_table <- reactive({
    build_model_status_table(
      regions        = names(gamm_prefixes),
      gam_status_fn  = gam_autocorrelation_status,
      gamm_status_fn = gamm_autocorrelation_status,
      models_fn      = get_models,
      best_model_fn  = get_best_model
    )
  })
  
  output$model_status_overview <- DT::renderDataTable({
    df <- model_status_table()
    show_df <- df[, c("region", "gam_ljung_box", "gam_sig_lags", "gamm_available",
                      "gamm_ljung_box", "gamm_sig_lags", "gamm_structure",
                      "recommended_model")]
    DT::datatable(
      show_df,
      options = list(
        pageLength = 10, dom = "Bfrtip", scrollX = TRUE,
        buttons = list(list(extend = "csv"), list(extend = "excel"),
                       list(extend = "pdf"), list(extend = "print")),
        columnDefs = list(list(className = "dt-center", targets = 1:7))
      ),
      extensions = "Buttons", rownames = FALSE,
      colnames = c("Region", "GAM Ljung-Box", "GAM Sig. Lags", "GAMM Fitted?",
                   "GAMM Ljung-Box", "GAMM Sig. Lags", "GAMM Structure", "Recommended Model"),
      caption = tags$caption(
        style = "caption-side:bottom;text-align:left;",
        tags$span(style = "color:black;font-weight:bold;font-style:italic;",
                  "Recommended model = GAM where the standalone negative-binomial GAM already clears the Ljung-Box test ",
                  "(p \u2265 0.05) at every tested lag (12\u201360 months); GAMM where a corARMA structure was needed and ",
                  "succeeds in clearing it; 'caution' flags where autocorrelation persists regardless of model choice, ",
                  "in which case coefficient standard errors and any forecasts should be reported with that caveat.")),
      callback = JS(
        "table.on('draw.dt', function(){",
        "  table.rows().every(function(){",
        "    var d = this.data();",
        "    if(d[1] && d[1].indexOf('Fail') !== -1){",
        "      $(this.node()).find('td').eq(1).css({'background-color':'#FCEBEB','font-weight':'bold'});",
        "    } else if(d[1] && d[1].indexOf('Pass') !== -1){",
        "      $(this.node()).find('td').eq(1).css({'background-color':'#EAF3DE','font-weight':'bold'});",
        "    }",
        "    if(d[4] && d[4].indexOf('Fail') !== -1){",
        "      $(this.node()).find('td').eq(4).css({'background-color':'#FCEBEB','font-weight':'bold'});",
        "    } else if(d[4] && d[4].indexOf('Pass') !== -1){",
        "      $(this.node()).find('td').eq(4).css({'background-color':'#EAF3DE','font-weight':'bold'});",
        "    }",
        "    if(d[7] && d[7].indexOf('caution') !== -1){",
        "      $(this.node()).css({'background-color':'#FFF6E5'});",
        "    } else if(d[7] === 'GAM' || d[7] === 'GAMM'){",
        "      $(this.node()).find('td').eq(7).css({'background-color':'#ffd966','font-weight':'bold'});",
        "    }",
        "  });",
        "});"
      )
    )
  })
  
  # ---- Concurvity tab (overall + pairwise) ----
  observeEvent(input$reset_diag5, {
    reset_tab_filters(session, data, region_input_id = "concurvity_region", default_region = "Northern")
    updateSelectInput(session, "concurvity_type", selected = "estimate")
  })
  
  concurvity_model <- reactive({
    req(input$concurvity_region)
    get_best_model(get_models(input$concurvity_region))
  })
  
  output$concurvity_overall_table <- DT::renderDataTable({
    bm <- concurvity_model()
    req(!is.null(bm))
    
    cc <- concurvity(bm, full = TRUE)
    df <- as.data.frame(round(cc, 3))
    df <- cbind(Statistic = rownames(df), df)
    rownames(df) <- NULL
    
    DT::datatable(df,
                  options = list(
                    dom = "Bfrtip", pageLength = 5, scrollX = TRUE,
                    buttons = list(list(extend = "csv"), list(extend = "excel"),
                                   list(extend = "pdf"), list(extend = "print")),
                    columnDefs = list(list(className = "dt-center", targets = 1:(ncol(df) - 1)))
                  ),
                  extensions = "Buttons", rownames = FALSE,
                  caption = tags$caption(
                    style = "caption-side:bottom;text-align:left;",
                    tags$span(style = "color:black;font-weight:bold;font-style:italic;",
                              "Each column is a model term (parametric intercept + smooths); each row is a concurvity statistic (0 = no concurvity, 1 = complete concurvity).")),
                  callback = JS(
                    "table.on('draw.dt', function(){",
                    "  table.rows().every(function(){",
                    "    var d = this.data();",
                    "    for (var i = 1; i < d.length; i++){",
                    "      if (parseFloat(d[i]) >= 0.8){",
                    "        $(this.node()).find('td').eq(i).css({'background-color':'#FCEBEB','font-weight':'bold','color':'#A32D2D'});",
                    "      }",
                    "    }",
                    "  });",
                    "});"
                  )
    )
  })
  
  output$concurvity_pairwise_table <- DT::renderDataTable({
    bm <- concurvity_model()
    req(!is.null(bm))
    req(input$concurvity_type)
    
    cc_list <- concurvity(bm, full = FALSE)
    mat <- cc_list[[input$concurvity_type]]
    
    df <- as.data.frame(round(mat, 3))
    df <- cbind(Term = rownames(df), df)
    rownames(df) <- NULL
    
    DT::datatable(df,
                  options = list(
                    dom = "Bfrtip", pageLength = 10, scrollX = TRUE,
                    buttons = list(list(extend = "csv"), list(extend = "excel"),
                                   list(extend = "pdf"), list(extend = "print")),
                    columnDefs = list(list(className = "dt-center", targets = 1:(ncol(df) - 1)))
                  ),
                  extensions = "Buttons", rownames = FALSE,
                  caption = tags$caption(
                    style = "caption-side:bottom;text-align:left;",
                    tags$span(style = "color:black;font-weight:bold;font-style:italic;",
                              paste0(
                                "Footnote: Pairwise ", input$concurvity_type,
                                " concurvity between each pair of terms (diagonal = 1 by definition). ",
                                "Off-diagonal values ≥ 0.8 flag term pairs that may be confounded with each other. ",
                                "Worst-case is a conservative upper bound; Observed uses the actual fit; ",
                                "Estimate is a computationally cheaper approximation. Values ≥ 0.8 indicate high concurvity."
                              ))),
                  callback = JS(
                    "table.on('draw.dt', function(){",
                    "  table.rows().every(function(){",
                    "    var d = this.data();",
                    "    var rowTerm = d[0];",
                    "    for (var i = 1; i < d.length; i++){",
                    "      var colTerm = table.column(i).header().textContent;",
                    "      var val = parseFloat(d[i]);",
                    "      if (val >= 0.8 && rowTerm !== colTerm){",
                    "        $(this.node()).find('td').eq(i).css({'background-color':'#FCEBEB','font-weight':'bold','color':'#A32D2D'});",
                    "      } else if (rowTerm === colTerm){",
                    "        $(this.node()).find('td').eq(i).css({'background-color':'#F1EFE8','color':'#5F5E5A'});",
                    "      }",
                    "    }",
                    "  });",
                    "});"
                  )
    )
  })
  
  # ---- GAM Seasonal Smooth Effect ----
  observeEvent(input$reset_gam_smooth_effect_region, {
    updateSelectInput(session, "gam_smooth_effect_region", selected = "all")
  })
  
  seasonal_smooth <- function(region_name) {
    pfx   <- pfx_lookup[[region_name]]
    stats <- safe_readRDS(paste0("model_", pfx, "_gam_noseason_stats.rds"))
    
    if (is.null(stats)) {
      return(data.frame(
        Region = region_name, AIC_Full = NA, AIC_NoSeason = NA, Delta_AIC = NA,
        Dev_Full = NA, Dev_NoSeason = NA, Delta_Dev = NA,
        Max_Concurvity = NA, LB_p_Full = NA, LB_p_NoSeason = NA,
        Note = "Precomputed stats unavailable (run precompute_gam_noseason.R)",
        stringsAsFactors = FALSE
      ))
    }
    
    data.frame(
      Region = region_name,
      AIC_Full = stats$AIC_Full, AIC_NoSeason = stats$AIC_NoSeason, Delta_AIC = stats$Delta_AIC,
      Dev_Full = stats$Dev_Full, Dev_NoSeason = stats$Dev_NoSeason, Delta_Dev = stats$Delta_Dev,
      Max_Concurvity = stats$Max_Concurvity,
      LB_p_Full = stats$LB_p_Full, LB_p_NoSeason = stats$LB_p_NoSeason,
      Note = stats$Note,
      stringsAsFactors = FALSE
    )
  }
  
  # GAM Concurvity
  concurvity_smooth_data <- reactive({
    regions <- gam_regions
    out <- do.call(rbind, lapply(regions, seasonal_smooth))
    rownames(out) <- NULL
    out
  })
  
  output$concurvity_smooth_table <- DT::renderDataTable({
    df <- concurvity_smooth_data()
    
    if (!is.null(input$gam_smooth_effect_region) && input$gam_smooth_effect_region != "all") {
      df <- df[df$Region == input$gam_smooth_effect_region, ]
    }
    
    show_df <- df[, c("Region","AIC_Full","AIC_NoSeason","Delta_AIC",
                      "Dev_Full","Dev_NoSeason","Delta_Dev",
                      "Max_Concurvity","LB_p_Full","LB_p_NoSeason")]
    show_df$LB_p_Full     <- ifelse(is.na(show_df$LB_p_Full), "\u2014", sprintf("%.4f", show_df$LB_p_Full))
    show_df$LB_p_NoSeason <- ifelse(is.na(show_df$LB_p_NoSeason), "\u2014", sprintf("%.4f", show_df$LB_p_NoSeason))
    
    dt <- DT::datatable(
      show_df,
      options = list(
        dom = "Bfrtip", pageLength = 10, scrollX = TRUE,
        buttons = list(list(extend = "csv"), list(extend = "excel"),
                       list(extend = "pdf"), list(extend = "print")),
        columnDefs = list(list(className = "dt-center", targets = 1:9))
      ),
      extensions = "Buttons", rownames = FALSE,
      colnames = c("Region", "AIC (Full)", "AIC (No Season)", "\u0394AIC",
                   "Dev. Explained (%)", "Dev. Explained \u2014 No Season (%)", "\u0394Dev. Explained",
                   "Max Concurvity", "Ljung\u2013Box p (Full, lag 12)", "Ljung\u2013Box p (No Season, lag 12)"),
      caption = tags$caption(
        style = "caption-side:bottom;text-align:left;",
        tags$span(style = "color:black;font-weight:bold;font-style:italic;",
                  paste0(
                    "Each region's best negative-binomial GAM (as selected on the GAM Estimates tab) is refit ",
                    "with the s(months) term-bundle removed, holding all other smooth terms fixed. \u0394AIC = AIC(No Season) \u2212 ",
                    "AIC(Full); a large positive value indicates worse fit without seasonality. Dev. Explained columns show ",
                    "percentage deviance explained with and without the seasonal term. Max Concurvity is the region's worst-case ",
                    "pairwise concurvity among s(time), s(months), ti(time,months), and the climate smooths in the full model. ",
                    "Ljung\u2013Box p-values test residual autocorrelation at lag 12 for both models to show whether removing 
                    seasonality reintroduces residual autocorrelation. s(months) is retained if removal increases AIC by more ",
                    "than 2, reduces deviance explained by more than 1 percentage point, or reintroduces significant residual ",
                    "autocorrelation absent in the full model.")))
    )
    
    DT::formatStyle(dt, "Max_Concurvity",
                    backgroundColor = DT::styleInterval(0.8, c("#EAF3DE", "#FCEBEB")),
                    fontWeight = "bold") %>%
      DT::formatStyle("Delta_AIC",
                      backgroundColor = DT::styleInterval(2, c("#EAF3DE", "#FCEBEB")),
                      fontWeight = "bold") %>%
      DT::formatStyle("Delta_Dev",
                      backgroundColor = DT::styleInterval(1, c("#FCEBEB", "#EAF3DE")),
                      fontWeight = "bold") %>%
      DT::formatStyle("LB_p_NoSeason",
                      color = DT::styleInterval(0.05, c("#A32D2D", "#3B6D11")),
                      fontWeight = "bold")
  })
  
  output$seasonal_smooth_banner <- renderUI({
    df <- concurvity_smooth_data()
    req(nrow(df) > 0)
    
    region_sel <- input$gam_smooth_effect_region
    
    if (is.null(region_sel) || region_sel == "all") {
      return(div(class = "threshold-alert alert-normal",
                 tags$i(class = "fa fa-circle-info"),
                 " Select a single region above for a region-specific seasonal-term effect \u2014 the table below shows all the regions fitted with GAMs."))
    }
    
    row <- df[df$Region == region_sel, ]
    req(nrow(row) > 0)
    row <- row[1, ]
    
    if (!is.null(row$Note) && !is.na(row$Note) && row$Note == "Best GAM unavailable") {
      return(div(class = "threshold-alert alert-high",
                 tags$i(class = "fa fa-triangle-exclamation"),
                 sprintf(" Best GAM unavailable for %s \u2014 the seasonal-term comparison could not be computed.",
                         region_sel)))
    }
    if (!is.null(row$Note) && !is.na(row$Note) && row$Note == "No s(months) term in best model") {
      return(div(class = "threshold-alert alert-normal",
                 tags$i(class = "fa fa-circle-info"),
                 sprintf(" The best-fitting GAM for %s does not include a seasonal smooth s(months), so this justification check does not apply here.",
                         region_sel)))
    }
    if (!is.null(row$Note) && !is.na(row$Note) && row$Note == "Reduced model failed to fit") {
      return(div(class = "threshold-alert alert-high",
                 tags$i(class = "fa fa-triangle-exclamation"),
                 sprintf(" The reduced (no-season) model for %s failed to fit \u2014 the seasonal-term justification check could not be completed.",
                         region_sel)))
    }
    
    high_concurvity <- !is.na(row$Max_Concurvity) && row$Max_Concurvity >= 0.8
    worse_aic       <- !is.na(row$Delta_AIC) && row$Delta_AIC > 2
    worse_dev       <- !is.na(row$Delta_Dev) && row$Delta_Dev > 1
    reintroduces_ac <- !is.na(row$LB_p_Full) && !is.na(row$LB_p_NoSeason) &&
      row$LB_p_Full >= 0.05 && row$LB_p_NoSeason < 0.05
    justified       <- worse_aic || worse_dev || reintroduces_ac
    
    fired <- c(
      if (worse_aic) sprintf("AIC increased by %.1f (> 2)", row$Delta_AIC),
      if (worse_dev) sprintf("deviance explained dropped by %.1f pts (> 1)", row$Delta_Dev),
      if (reintroduces_ac) sprintf("residual autocorrelation was reintroduced (Ljung\u2013Box p moved from %.4f to %.4f)",
                                   row$LB_p_Full, row$LB_p_NoSeason)
    )
    fired_str <- if (length(fired) > 0) paste(fired, collapse = "; ") else "no criterion was met"
    
    if (high_concurvity && justified) {
      div(class = "threshold-alert alert-high",
          tags$i(class = "fa fa-triangle-exclamation"),
          sprintf(
            " %s: worst-case concurvity among seasonal/climate smooths is high (%.2f), but s(months) is statistically justified \u2014 removing it: %s. Consistent with Wood (2008), high concurvity here does not imply redundancy: s(months) is retained.",
            region_sel, row$Max_Concurvity, fired_str
          ))
    } else if (high_concurvity && !justified) {
      div(class = "threshold-alert alert-low",
          tags$i(class = "fa fa-circle-info"),
          sprintf(
            " %s: worst-case concurvity is high (%.2f), and removing s(months) does not meaningfully worsen fit by AIC (\u0394AIC = %.1f) or deviance explained (\u0394Dev = %.1f pts), nor reintroduce residual autocorrelation \u2014 s(months) may be largely redundant with the other seasonal/climate smooths in this region and its individual effect estimate should be interpreted cautiously.",
            region_sel, row$Max_Concurvity, row$Delta_AIC, row$Delta_Dev
          ))
    } else {
      div(class = "threshold-alert alert-normal",
          tags$i(class = "fa fa-circle-check"),
          sprintf(
            " %s: worst-case concurvity among seasonal/climate smooths is %.2f (below the 0.80 flag threshold) \u2014 s(months) is well-identified here and its estimate can be interpreted with standard confidence.",
            region_sel, row$Max_Concurvity
          ))
    }
  })
  
  output$concurvity_smooth_effect_writeup <- renderUI({
    df <- concurvity_smooth_data()
    req(nrow(df) > 0)
    
    valid <- df[!is.na(df$Delta_AIC), ]
    if (nrow(valid) == 0) {
      return(div(class = "threshold-alert alert-normal", tags$i(class = "fa fa-circle-info"),
                 " Seasonal-term comparison could not be computed for any region \u2014 check that best models are available."))
    }
    
    ex <- if (!is.null(input$gam_smooth_effect_region) && input$gam_smooth_effect_region != "all") {
      row <- valid[valid$Region == input$gam_smooth_effect_region, ]
      if (nrow(row) == 0) {
        return(div(class = "threshold-alert alert-normal", tags$i(class = "fa fa-circle-info"),
                   sprintf(" Seasonal-term comparison could not be computed for %s.", input$gam_smooth_effect_region)))
      }
      row[1, ]
    } else {
      valid[which.max(valid$Delta_AIC), ]
    }
    
    div(class = "summary-narrative-card",
        h4(tags$i(class = "fa fa-file-lines"), "Narrative"),
        tags$hr(),
        p(class = "summary-narrative-text",
          "To evaluate whether the seasonal smooth represented redundant information given its ",
          "concurvity with s(time), ti(time, months), and the climate smooths, we compared each ",
          "region's best-fitting negative-binomial GAM with and without s(months). For ",
          tags$span(class = "narrative-highlight", ex$Region), ", removal of the seasonal smooth ",
          "resulted in a marked deterioration in model fit (AIC = ",
          tags$span(class = "narrative-highlight", sprintf("%.1f", ex$AIC_Full)), " versus ",
          tags$span(class = "narrative-highlight", sprintf("%.1f", ex$AIC_NoSeason)),
          "; \u0394AIC = ", tags$span(class = "narrative-highlight", sprintf("%.1f", ex$Delta_AIC)),
          ") and reduced the deviance explained from ",
          tags$span(class = "narrative-highlight", paste0(ex$Dev_Full, "%")), " to ",
          tags$span(class = "narrative-highlight", paste0(ex$Dev_NoSeason, "%")),
          " (\u0394 = ", tags$span(class = "narrative-highlight", sprintf("%.1f pts", ex$Delta_Dev)), ")",
          ". Residual autocorrelation at lag 12 was also greater in the model without seasonal ",
          "adjustment (Ljung\u2013Box p = ", sprintf("%.4f", ex$LB_p_Full), " for the full model versus p = ",
          sprintf("%.4f", ex$LB_p_NoSeason), " once s(months) was dropped). ",
          if (is.null(input$gam_smooth_effect_region) || input$gam_smooth_effect_region == "all")
            "This pattern was assessed across all 10 regions (see table above); "
          else "",
          "These findings indicate that the seasonal smooth captures important variation in malaria ",
          "incidence beyond that explained by rainfall and temperature alone, and is retained despite ",
          "worst-case concurvity of ", sprintf("%.2f", ex$Max_Concurvity),
          " for this region, consistent with the principle that concurvity reflects redundancy in the ",
          "basis, not necessarily in the information each term contributes (Wood, 2008)."))
  })
  
  
  # ---- GAMM Seasonal Smooth Effect ----
  observeEvent(input$reset_smooth_effect_region_gamm, {
    updateSelectInput(session, "smooth_effect_region_gamm", selected = "all")
  })
  
  seasonal_smooth_gamm <- function(region_name) {
    pfx   <- pfx_lookup[[region_name]]
    stats <- safe_readRDS(paste0("model_", pfx, "_gamm_noseason_stats.rds"))
    
    if (is.null(stats)) {
      return(data.frame(
        Region = region_name,
        AIC_Full = NA, AIC_NoSeason = NA, Delta_AIC = NA,
        BIC_Full = NA, BIC_NoSeason = NA, Delta_BIC = NA,
        AdjR2_Full = NA, AdjR2_NoSeason = NA,
        Max_Concurvity = NA, LB_p_Full = NA, LB_p_NoSeason = NA,
        Structure = NA, Note = "Precomputed stats unavailable (run precompute_gamm_noseason.R)",
        stringsAsFactors = FALSE
      ))
    }
    
    data.frame(
      Region = region_name,
      AIC_Full = stats$AIC_Full, AIC_NoSeason = stats$AIC_NoSeason, Delta_AIC = stats$Delta_AIC,
      BIC_Full = stats$BIC_Full, BIC_NoSeason = stats$BIC_NoSeason, Delta_BIC = stats$Delta_BIC,
      AdjR2_Full = stats$AdjR2_Full, AdjR2_NoSeason = stats$AdjR2_NoSeason,
      Max_Concurvity = stats$Max_Concurvity,
      LB_p_Full = stats$LB_p_Full, LB_p_NoSeason = stats$LB_p_NoSeason,
      Structure = stats$Structure, Note = stats$Note,
      stringsAsFactors = FALSE
    )
  }
  
  concurvity_smooth_data_gamm <- reactive({
    out <- do.call(rbind, lapply(gamm_regions, seasonal_smooth_gamm))
    rownames(out) <- NULL
    out
  })
  
  output$concurvity_smooth_table_gamm <- DT::renderDataTable({
    df <- concurvity_smooth_data_gamm()
    
    if (!is.null(input$smooth_effect_region_gamm) && input$smooth_effect_region_gamm != "all") {
      df <- df[df$Region == input$smooth_effect_region_gamm, ]
    }
    
    show_df <- df[, c("Region", "Structure", "AIC_Full", "AIC_NoSeason", "Delta_AIC",
                      "BIC_Full", "BIC_NoSeason", "Delta_BIC",
                      "AdjR2_Full", "AdjR2_NoSeason",
                      "Max_Concurvity", "LB_p_Full", "LB_p_NoSeason")]
    show_df$LB_p_Full     <- ifelse(is.na(show_df$LB_p_Full), "\u2014", sprintf("%.4f", show_df$LB_p_Full))
    show_df$LB_p_NoSeason <- ifelse(is.na(show_df$LB_p_NoSeason), "\u2014", sprintf("%.4f", show_df$LB_p_NoSeason))
    
    dt <- DT::datatable(
      show_df,
      options = list(
        dom = "Bfrtip", pageLength = 10, scrollX = TRUE,
        buttons = list(list(extend = "csv"), list(extend = "excel"),
                       list(extend = "pdf"), list(extend = "print")),
        columnDefs = list(list(className = "dt-center", targets = 1:12))
      ),
      extensions = "Buttons", rownames = FALSE,
      colnames = c("Region", "corARMA Structure", "AIC[lme] (Full)", "AIC[lme] (No Season)", "\u0394AIC[lme]",
                   "BIC[lme] (Full)", "BIC[lme] (No Season)", "\u0394BIC[lme]",
                   "Adj. R\u00b2 (%, Full)", "Adj. R\u00b2 (%, No Season)",
                   "Max Concurvity", "Ljung\u2013Box p (Full, lag 12)", "Ljung\u2013Box p (No Season, lag 12)"),
      caption = tags$caption(
        style = "caption-side:bottom;text-align:left;",
        tags$span(style = "color:black;font-weight:bold;font-style:italic;",
                  paste0(
                    "Each region's best corARMA-corrected GAMM (as selected on the GAMM Estimates tab) is refit with the ",
                    "s(months) term-bundle removed from the underlying smoother, holding the SAME corARMA(p,q) structure and ",
                    "fixed NB theta as the saved best model. Model comparison uses AIC/BIC of the linear mixed-effect(lme) component ",
                    "(\u0394AIC = No Season \u2212 Full; lower is better) rather than deviance explained: gamm()'s gam component is ",
                    "fitted via Penalized Quasi-Likelihood on a working pseudo-response, so its deviance-explained figure does ",
                    "not reliably reflect fit to the original negative-binomial response and is intentionally not shown here \u2014 ",
                    "consistent with the GAMM Estimates tab, which also reports AIC/BIC/logLik rather than deviance explained. ",
                    "Adj. R\u00b2 of the underlying smoother is shown only as a secondary, descriptive figure. Max Concurvity is the ",
                    "region's worst-case pairwise concurvity among s(time), s(months), ti(time,months), and the climate smooths. ",
                    "Ljung\u2013Box p-values test residual autocorrelation at lag 12 on normalised residuals for both models. ",
                    "s(months) is retained if removal increases AIC[lme] by more than 2, increases BIC[lme] by more than 2, or ",
                    "reintroduces significant residual autocorrelation absent in the full model."))
      )
    )
    
    DT::formatStyle(dt, "Max_Concurvity",
                    backgroundColor = DT::styleInterval(0.8, c("#EAF3DE", "#FCEBEB")),
                    fontWeight = "bold") %>%
      DT::formatStyle("Delta_AIC",
                      backgroundColor = DT::styleInterval(2, c("#EAF3DE", "#FCEBEB")),
                      fontWeight = "bold") %>%
      DT::formatStyle("Delta_BIC",
                      backgroundColor = DT::styleInterval(2, c("#EAF3DE", "#FCEBEB")),
                      fontWeight = "bold") %>%
      DT::formatStyle("LB_p_NoSeason",
                      color = DT::styleInterval(0.05, c("#A32D2D", "#3B6D11")),
                      fontWeight = "bold")
  })
  
  output$seasonal_smooth_banner_gamm <- renderUI({
    df <- concurvity_smooth_data_gamm()
    req(nrow(df) > 0)
    
    region_sel <- input$smooth_effect_region_gamm
    
    if (is.null(region_sel) || region_sel == "all") {
      return(div(class = "threshold-alert alert-normal",
                 tags$i(class = "fa fa-circle-info"),
                 " Select a single region above for a region-specific seasonal-term effect \u2014 the table below shows all 9 GAMM-fitted regions."))
    }
    
    row <- df[df$Region == region_sel, ]
    req(nrow(row) > 0)
    row <- row[1, ]
    
    if (!is.null(row$Note) && !is.na(row$Note) && row$Note == "Best GAMM unavailable") {
      return(div(class = "threshold-alert alert-high",
                 tags$i(class = "fa fa-triangle-exclamation"),
                 sprintf(" Best GAMM unavailable for %s \u2014 the seasonal-term comparison could not be computed.",
                         region_sel)))
    }
    if (!is.null(row$Note) && !is.na(row$Note) && row$Note == "No s(months) term in best GAMM") {
      return(div(class = "threshold-alert alert-normal",
                 tags$i(class = "fa fa-circle-info"),
                 sprintf(" The best-fitting GAMM for %s does not include a seasonal smooth s(months), so this justification check does not apply here.",
                         region_sel)))
    }
    if (!is.null(row$Note) && !is.na(row$Note) && row$Note == "Reduced GAMM failed to fit/converge under PQL") {
      return(div(class = "threshold-alert alert-high",
                 tags$i(class = "fa fa-triangle-exclamation"),
                 sprintf(" The reduced (no-season) GAMM for %s failed to fit or converge under PQL \u2014 the seasonal-term justification check could not be completed.",
                         region_sel)))
    }
    
    high_concurvity <- !is.na(row$Max_Concurvity) && row$Max_Concurvity >= 0.8
    worse_aic       <- !is.na(row$Delta_AIC) && row$Delta_AIC > 2
    worse_bic       <- !is.na(row$Delta_BIC) && row$Delta_BIC > 2
    reintroduces_ac <- !is.na(row$LB_p_Full) && !is.na(row$LB_p_NoSeason) &&
      row$LB_p_Full >= 0.05 && row$LB_p_NoSeason < 0.05
    justified       <- worse_aic || worse_bic || reintroduces_ac
    
    fired <- c(
      if (worse_aic) sprintf("AIC[lme] increased by %.1f (> 2)", row$Delta_AIC),
      if (worse_bic) sprintf("BIC[lme] increased by %.1f (> 2)", row$Delta_BIC),
      if (reintroduces_ac) sprintf("residual autocorrelation was reintroduced (Ljung\u2013Box p moved from %.4f to %.4f)",
                                   row$LB_p_Full, row$LB_p_NoSeason)
    )
    fired_str <- if (length(fired) > 0) paste(fired, collapse = "; ") else "no criterion was met"
    
    if (high_concurvity && justified) {
      div(class = "threshold-alert alert-high",
          tags$i(class = "fa fa-triangle-exclamation"),
          sprintf(
            " %s (%s): worst-case concurvity among seasonal/climate smooths is high (%.2f), but s(months) is statistically justified \u2014 removing it: %s. Consistent with Wood (2008), high concurvity here does not imply redundancy: s(months) is retained.",
            region_sel, row$Structure, row$Max_Concurvity, fired_str
          ))
    } else if (high_concurvity && !justified) {
      div(class = "threshold-alert alert-low",
          tags$i(class = "fa fa-circle-info"),
          sprintf(
            " %s (%s): worst-case concurvity is high (%.2f), and removing s(months) does not meaningfully worsen fit by AIC[lme] (\u0394AIC = %.1f) or BIC[lme] (\u0394BIC = %.1f), nor reintroduce residual autocorrelation \u2014 s(months) may be largely redundant with the other seasonal/climate smooths in this region and its individual effect estimate should be interpreted cautiously.",
            region_sel, row$Structure, row$Max_Concurvity, row$Delta_AIC, row$Delta_BIC
          ))
    } else {
      div(class = "threshold-alert alert-normal",
          tags$i(class = "fa fa-circle-check"),
          sprintf(
            " %s (%s): worst-case concurvity among seasonal/climate smooths is %.2f (below the 0.80 flag threshold) \u2014 s(months) is well-identified here and its estimate can be interpreted with standard confidence.",
            region_sel, row$Structure, row$Max_Concurvity
          ))
    }
  })
  
  output$concurvity_smooth_effect_writeup_gamm <- renderUI({
    df <- concurvity_smooth_data_gamm()
    req(nrow(df) > 0)
    
    valid <- df[!is.na(df$Delta_AIC), ]
    if (nrow(valid) == 0) {
      return(div(class = "threshold-alert alert-normal", tags$i(class = "fa fa-circle-info"),
                 " Seasonal-term comparison could not be computed for any GAMM region \u2014 check that best GAMMs are available."))
    }
    
    ex <- if (!is.null(input$smooth_effect_region_gamm) && input$smooth_effect_region_gamm != "all") {
      row <- valid[valid$Region == input$smooth_effect_region_gamm, ]
      if (nrow(row) == 0) {
        return(div(class = "threshold-alert alert-normal", tags$i(class = "fa fa-circle-info"),
                   sprintf(" Seasonal-term comparison could not be computed for %s.", input$smooth_effect_region_gamm)))
      }
      row[1, ]
    } else {
      valid[which.max(valid$Delta_AIC), ]
    }
    
    div(class = "summary-narrative-card",
        h4(tags$i(class = "fa fa-file-lines"), "Narrative"),
        tags$hr(),
        p(class = "summary-narrative-text",
          "To evaluate whether the seasonal smooth represented redundant information given its ",
          "concurvity with s(time), ti(time, months), and the climate smooths in the corARMA-corrected ",
          "GAMM, we refit each region's best-fitting GAMM with and without s(months), holding the winning ",
          tags$span(class = "narrative-highlight", ex$Structure), " correlation structure and NB theta fixed. For ",
          tags$span(class = "narrative-highlight", ex$Region), ", removal of the seasonal smooth changed lme-based ",
          "fit from AIC = ",
          tags$span(class = "narrative-highlight", sprintf("%.1f", ex$AIC_Full)), " to ",
          tags$span(class = "narrative-highlight", sprintf("%.1f", ex$AIC_NoSeason)),
          " (\u0394AIC = ", tags$span(class = "narrative-highlight", sprintf("%.1f", ex$Delta_AIC)),
          "; \u0394BIC = ", tags$span(class = "narrative-highlight", sprintf("%.1f", ex$Delta_BIC)), "). ",
          "Deviance explained is not reported for this comparison: gamm()'s underlying gam component is fitted via ",
          "Penalized Quasi-Likelihood on a working pseudo-response rather than the original negative-binomial deviance, ",
          "which makes that statistic unreliable and non-comparable across corARMA structures; AIC/BIC of the lme ",
          "component \u2014 the same criteria used to select the winning correlation structure \u2014 are used instead. ",
          "Residual autocorrelation at lag 12 (on normalised residuals) was ",
          sprintf("%.4f", ex$LB_p_Full), " for the full model versus ",
          sprintf("%.4f", ex$LB_p_NoSeason), " once s(months) was dropped. ",
          if (is.null(input$smooth_effect_region_gamm) || input$smooth_effect_region_gamm == "all")
            "This pattern was assessed across all nine GAMM-fitted regions (see table above); "
          else "",
          "These findings indicate whether the seasonal smooth captures variation in malaria incidence beyond that ",
          "explained by rainfall, temperature, and the fitted correlation structure alone, and inform whether it ",
          "should be retained despite worst-case concurvity of ", sprintf("%.2f", ex$Max_Concurvity),
          " for this region, consistent with the principle that concurvity reflects redundancy in the basis, not ",
          "necessarily in the information each term contributes (Wood, 2008)."))
  })
  
  
  # ==================================================================================
  #  GAM ESTIMATES  (sidebar: "4 GAM Estimates")
  #   Tabs: GAM Metrics | GAM Smooth Terms | GAM Forecast Plot | GAM Forecast Table
  # ==================================================================================
  
  observeEvent(input$reset_est_metrics, {
    reset_tab_filters(session, data, region_input_id = "model_region", default_region = "Northern")
  })
  observeEvent(input$reset_est_smooth, {
    reset_tab_filters(session, data, region_input_id = "smooth_region", default_region = "Northern")
  })
  observeEvent(input$reset_forecast_plot, {
    reset_tab_filters(session, data, region_input_id = "forecast_region_plot", default_region = "Northern")
  })
  observeEvent(input$reset_forecast_table, {
    reset_tab_filters(session, data, region_input_id = "forecast_region_table", default_region = "Northern")
  })
  observeEvent(input$reset_forecast, {
    reset_tab_filters(session, data, region_input_id = "forecast_region")
  })
  
  estimates_bmd <- reactive({
    req(input$model_region)
    get_models_and_best_idx(input$model_region)
  })
  
  model_metrics_df <- reactive({
    bmd    <- estimates_bmd()
    models <- bmd$models
    idx    <- bmd$best_idx
    df <- data.frame(
      Model = paste0("Model ", seq_along(models)),
      REML_Score = sapply(models, function(m) sprintf("%.2f", round(m$gcv.ubre, 2))),
      Adj_R2_num = sapply(models, function(m) round(summary(m)$r.sq * 100, 2)),
      Dev_Explained_num = sapply(models, function(m) round(summary(m)$dev.expl * 100, 2))
    )
    df$Best_Model    <- ifelse(seq_len(nrow(df)) == idx, "Yes \u2605", "No")
    df$Adj_R2        <- sprintf("%.2f", df$Adj_R2_num)
    df$Dev_Explained <- sprintf("%.2f", df$Dev_Explained_num)
    df$Adj_R2_num <- df$Dev_Explained_num <- NULL
    df[, c("Model", "REML_Score", "Adj_R2", "Dev_Explained", "Best_Model")]
  })
  
  output$model_metrics <- DT::renderDataTable({
    DT::datatable(
      model_metrics_df(),
      options = list(
        striped = TRUE, hover = TRUE, bordered = TRUE, dom = "Bfrtip",
        buttons = list(list(extend = "csv"), list(extend = "excel"),
                       list(extend = "pdf"), list(extend = "print")),
        columnDefs = list(list(className = "dt-center", targets = 1:4))
      ),
      extensions = "Buttons", rownames = FALSE,
      colnames = c("Model", "REML Score", "Adj R\u00b2", "Dev Explained", "Best Model"),
      caption = tags$caption(
        style = "caption-side:bottom;text-align:left;",
        tags$span(style = "color:black;font-weight:bold;font-style:italic;",
                  "Adj R\u00b2=Adjusted R Squared; Dev Explained=Deviance Explained; REML Score=restricted maximum likelihood criterion minimized during fitting (lower is better).")),
      callback = JS(
        "table.on('draw.dt', function(){",
        "  table.rows().every(function(){",
        "    var d = this.data();",
        "    if(d[4] && d[4].indexOf('Yes') !== -1){",
        "      $(this.node()).css({'background-color':'#ffd966','font-weight':'bold'});",
        "    }",
        "  });",
        "});"
      )
    )
  })
  
  smooth_bmd <- reactive({
    req(input$smooth_region)
    get_models_and_best_idx(input$smooth_region)
  })
  
  output$best_model_smooth_terms <- DT::renderDataTable({
    bmd    <- smooth_bmd()
    models <- bmd$models
    idx    <- bmd$best_idx
    req(!is.null(models[[idx]]))
    
    st   <- as.data.frame(round(summary(models[[idx]])$s.table, 4))
    st   <- cbind(Parameters = rownames(st), st)
    pt   <- as.data.frame(round(summary(models[[idx]])$p.table, 4))
    pt   <- cbind(Parameters = rownames(pt), pt)
    irow <- pt[pt$Parameters == "(Intercept)", ]
    
    colnames(irow) <- colnames(st) <- c("Parameters", "Col2", "Col3", "Col4", "Col5")
    irow[] <- lapply(irow, function(x) if (is.numeric(x)) format(x, nsmall = 4) else x)
    st[]   <- lapply(st,   function(x) if (is.numeric(x)) format(x, nsmall = 4) else x)
    
    hdr <- data.frame(Parameters = "Non\u2011parametric terms",
                      Col2 = "edf", Col3 = "Ref.df", Col4 = "Chi-square(\u03C7\u00B2)", Col5 = "p-value",
                      stringsAsFactors = FALSE)
    full <- rbind(irow, hdr, st)
    colnames(full) <- c("Parameters", "Estimate", "Std. Error", "z-value", "p-value")
    
    DT::datatable(full,
                  options = list(
                    striped = TRUE, hover = TRUE, bordered = TRUE, dom = "Bfrtip",
                    buttons = list(list(extend = "csv"), list(extend = "excel"),
                                   list(extend = "pdf"), list(extend = "print")),
                    columnDefs = list(list(className = "dt-center", targets = 1:4)),
                    rowCallback = JS(
                      "function(row, data){",
                      "  if(data[0]==='Non\u2011parametric terms'){$(row).addClass('section-header');}",
                      "}"
                    )
                  ),
                  extensions = "Buttons", rownames = FALSE,
                  caption = tags$caption(class = "data-table-title",
                                         "Parametric and Non\u2011Parametric Estimates of the Best GAM")
    )
  })
  
  forecast_model_plot <- reactive({
    req(input$forecast_region_plot)
    get_best_model(get_models(input$forecast_region_plot))
  })
  
  forecast_model_table <- reactive({
    req(input$forecast_region_table)
    get_best_model(get_models(input$forecast_region_table))
  })
  
  forecast_offset <- function(rd, region) {
    if ("log_pop_offset" %in% names(rd)) {
      lpo <- rd$log_pop_offset
      lpo_non_na <- lpo[!is.na(lpo)]
      if (length(lpo_non_na) > 0) return(tail(lpo_non_na, 1))
      showNotification(
        paste0("log_pop_offset is NA for every row in ", region,
               " (unmatched population join) \u2014 forecasting without a population offset adjustment."),
        type = "warning"
      )
      return(0)
    }
    pop_col <- intersect(c("pop_join", "population", "pop", "pop_offset", "Population"), names(rd))
    if (length(pop_col) > 0) {
      pop_vals <- rd[[pop_col[1]]]
      pop_non_na <- pop_vals[!is.na(pop_vals)]
      if (length(pop_non_na) > 0) return(log(tail(pop_non_na, 1)))
    }
    showNotification(
      paste0("No log_pop_offset (or population) column found for ", region,
             " \u2014 forecasting without a population offset adjustment."),
      type = "warning"
    )
    0
  }
  
  output$forecast_plot <- renderPlotly({
    req(input$forecast_region_plot)
    bm  <- forecast_model_plot()
    req(!is.null(bm))
    reg <- input$forecast_region_plot
    rd  <- data %>% filter(region == reg) %>% arrange(date)
    
    last_time <- max(rd$time, na.rm = TRUE)
    last_date <- max(rd$date, na.rm = TRUE)
    n_ahead   <- 48
    future_months <- seq(last_date %m+% months(1), by = "month", length.out = n_ahead)
    
    last_log_pop_offset <- forecast_offset(rd, reg)
    
    future_grid <- data.frame(
      time = seq(last_time + 1, by = 1, length.out = n_ahead),
      months = as.numeric(format(future_months, "%m")),
      rainfall = mean(rd$rainfall, na.rm = TRUE),
      avgtemp = mean(rd$avgtemp, na.rm = TRUE),
      log_pop_offset = last_log_pop_offset,
      date = future_months
    )
    
    pred <- suppressWarnings(predict(bm, newdata = future_grid, type = "response", se.fit = TRUE))
    
    # Forecast horizon: population held fixed at last observed value
    pop_last <- exp(last_log_pop_offset)
    future_grid$fit <- (pred$fit / pop_last) * 1000
    future_grid$lwr <- pmax(0, (pred$fit - 1.96 * pred$se.fit)) / pop_last * 1000
    future_grid$upr <- (pred$fit + 1.96 * pred$se.fit) / pop_last * 1000
    
    # Historical: population varies by row 
    pop <- get_population_series(rd)
    hist_df <- data.frame(date = rd$date, fit = (rd$uncom / pop) * 1000)
    
    p <- ggplot() +
      geom_line(data = hist_df,    aes(x = date, y = fit), color = "black", linewidth = 0.8) +
      geom_ribbon(data = future_grid, aes(x = date, ymin = lwr, ymax = upr), alpha = 0.2, fill = "#2c63ab") +
      geom_line(data = future_grid,   aes(x = date, y = fit), color = "#2c63ab", linewidth = 1, linetype = "dashed") +
      geom_vline(xintercept = last_date, linetype = "dotted", color = "red") +
      scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
      labs(x = "", y = "Incidence (per 1,000 population)") +
      theme_residuals()
    make_static_plotly(p, filename = paste0("forecast_", reg))
  })
  
  output$forecast_table <- DT::renderDataTable({
    req(input$forecast_region_table)
    bm <- forecast_model_table()
    req(!is.null(bm))
    rd <- data %>% filter(region == input$forecast_region_table) %>% arrange(date)
    last_time <- max(rd$time, na.rm = TRUE)
    last_date <- max(rd$date, na.rm = TRUE)
    future_months <- seq(last_date %m+% months(1), by = "month", length.out = 48)
    last_log_pop_offset <- forecast_offset(rd, input$forecast_region_table)
    future_grid <- data.frame(
      time = seq(last_time + 1, by = 1, length.out = 48),
      months = as.numeric(format(future_months, "%m")),
      rainfall = mean(rd$rainfall, na.rm = TRUE),
      avgtemp = mean(rd$avgtemp, na.rm = TRUE),
      log_pop_offset = last_log_pop_offset
    )
    
    pred <- suppressWarnings(predict(bm, newdata = future_grid, type = "response", se.fit = TRUE))
    
    # Population held fixed at last observed value for the whole forecast horizon
    pop_last <- exp(last_log_pop_offset)
    
    out <- data.frame(
      Month = format(future_months, "%b %Y"),
      Predicted = round((pred$fit / pop_last) * 1000, 2),
      Lower_CI  = round(pmax(0, pred$fit - 1.96 * pred$se.fit) / pop_last * 1000, 2),
      Upper_CI  = round((pred$fit + 1.96 * pred$se.fit) / pop_last * 1000, 2)
    )
    DT::datatable(out,
                  options = list(pageLength = 12, dom = "Bfrtip",
                                 buttons = list(list(extend = "csv"), list(extend = "excel"),
                                                list(extend = "pdf"), list(extend = "print")),
                                 columnDefs = list(list(className = "dt-center", targets = 1:3))),
                  extensions = "Buttons", rownames = FALSE,
                  colnames = c("Month", "Predicted (per 1,000 pop)", "Lower 95% CI", "Upper 95% CI"),
                  caption = tags$caption(
                    style = "caption-side:bottom;text-align:left;",
                    tags$span(style = "color:black;font-weight:bold;font-style:italic;",
                              "Forecast expressed as incidence (per 1,000 population), holding population fixed at 
                              its most recently observed level for the full 48-month horizon."))
    )
  })
  
  
  # ======================================================================================
  # GAM DIAGNOSTICS  (sidebar: "5 GAM Diagnostics")
  #   Tabs: Observed vs Fitted | Q-Q Plot | Response vs Fitted | Residual Autocorrelation
  # ======================================================================================
  
  observeEvent(input$reset_diag1, { reset_tab_filters(session, data, region_input_id = "diag_region1", default_region = "Northern") })
  observeEvent(input$reset_diag2, { reset_tab_filters(session, data, region_input_id = "diag_region2", default_region = "Northern") })
  observeEvent(input$reset_diag3, { reset_tab_filters(session, data, region_input_id = "diag_region3", default_region = "Northern") })
  observeEvent(input$reset_diag4, {
    reset_tab_filters(session, data, region_input_id = "diag_region4", default_region = "Northern")
    updateSliderInput(session, "max_lag", value = 12)
  })
  
  output$observed_fitted <- renderPlotly({
    req(input$diag_region1)
    bm <- get_best_model(get_models(input$diag_region1))
    req(!is.null(bm))
    rd <- data %>% filter(region == input$diag_region1)
    pd <- data.frame(Date = rd$date, Observed = rd$uncom / 1000, Fitted = fitted(bm) / 1000)
    p <- ggplot(pd, aes(x = Date)) +
      geom_line(aes(y = Observed, color = "Observed"), linewidth = 1) +
      geom_line(aes(y = Fitted,   color = "Fitted"),   linewidth = 1, linetype = "dashed") +
      scale_color_manual(values = c("Observed" = "black", "Fitted" = "#F8766D")) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
      labs(x = "Year", y = "Uncomplicated Malaria(x10\u00b3)") + theme_residuals()
    make_static_plotly(p, filename = paste0("obs_fit_", input$diag_region1))
  })
  
  output$qq_plot <- renderPlotly({
    req(input$diag_region2)
    bm <- get_best_model(get_models(input$diag_region2))
    req(!is.null(bm))
    p <- qq_plot(bm, method = "simulate") +
      labs(title = NULL, x = "Theoretical Quantiles", y = "Deviance residuals") + theme_residuals()
    make_static_plotly(p, filename = paste0("qq_", input$diag_region2))
  })
  
  output$response_fitted_plot <- renderPlotly({
    req(input$diag_region3)
    bm <- get_best_model(get_models(input$diag_region3))
    req(!is.null(bm))
    rd <- data %>% filter(region == input$diag_region3)
    pd <- data.frame(Fitted = fitted(bm) / 1000, Observed = rd$uncom / 1000)
    p <- ggplot(pd, aes(x = Fitted, y = Observed)) +
      geom_point(size = 2.5, color = "#F8766D") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "steelblue") +
      labs(x = "Fitted values(x10\u00b3)", y = "Uncomplicated Malaria(x10\u00b3)") + theme_residuals()
    make_static_plotly(p, filename = paste0("resp_fit_", input$diag_region3))
  })
  
  output$gam_acf_plot <- renderPlotly({
    req(input$diag_region4, input$max_lag)
    region  <- input$diag_region4
    max_lag <- input$max_lag
    
    bm <- get_best_model(get_models(region))
    req(!is.null(bm))
    
    resid_dev <- residuals(bm, type = "deviance")
    
    acf_res <- acf(resid_dev, lag.max = max_lag, plot = FALSE)
    acf_df  <- data.frame(lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
    
    conf_level <- 0.95
    ci <- qnorm((1 + conf_level) / 2) / sqrt(length(resid_dev))
    acf_df$upper_band <- ci
    acf_df$lower_band <- -ci
    
    p <- ggplot(acf_df, aes(lag, acf)) +
      geom_ribbon(aes(ymin = lower_band, ymax = upper_band), fill = "steelblue", alpha = 0.20) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
      geom_hline(yintercept = c(ci, -ci), linetype = "dashed", colour = "blue") +
      geom_segment(aes(xend = lag, y = 0, yend = acf), colour = "lightblue", linewidth = 0.8) +
      geom_col(width = 0.30, fill = "steelblue") +
      labs(title = NULL, x = "Lag", y = "Autocorrelation Function") +
      theme_autocorr_plots()
    
    make_static_plotly(p, filename = paste0("gam_acf_plot_", region))
  })
  
  output$gam_pacf_plot <- renderPlotly({
    req(input$diag_region4, input$max_lag)
    region  <- input$diag_region4
    max_lag <- input$max_lag
    
    bm <- get_best_model(get_models(region))
    req(!is.null(bm))
    
    resid_dev <- residuals(bm, type = "deviance")
    
    pacf_res <- pacf(resid_dev, lag.max = max_lag, plot = FALSE)
    pacf_df  <- data.frame(lag = as.numeric(pacf_res$lag), pacf = as.numeric(pacf_res$acf))
    
    conf_level <- 0.95
    ci <- qnorm((1 + conf_level) / 2) / sqrt(length(resid_dev))
    pacf_df$upper_band <- ci
    pacf_df$lower_band <- -ci
    
    p <- ggplot(pacf_df, aes(x = lag, y = pacf)) +
      geom_ribbon(aes(ymin = lower_band, ymax = upper_band), fill = "steelblue", alpha = 0.20) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
      geom_hline(yintercept = c(ci, -ci), linetype = "dashed", colour = "blue") +
      geom_segment(aes(xend = lag, y = 0, yend = pacf), colour = "lightblue", linewidth = 0.8) +
      geom_col(width = 0.30, fill = "steelblue") +
      labs(title = NULL, x = "Lag", y = "Partial Autocorrelation Function") +
      theme_autocorr_plots()
    
    make_static_plotly(p, filename = paste0("gam_pacf_plot_", region))
  })
  
  output$gam_ljung_box_table <- DT::renderDataTable({
    req(input$diag_region4)
    region <- input$diag_region4
    
    bm <- get_best_model(get_models(region))
    req(!is.null(bm))
    
    resid_dev <- residuals(bm, type = "deviance")
    
    lb <- lapply(c(12, 24, 36, 48, 60), function(lag) {
      bt <- Box.test(resid_dev, lag = lag, type = "Ljung-Box")
      data.frame(lag = lag, statistic = unname(bt$statistic),
                 df = unname(bt$parameter), p_value = bt$p.value)
    }) %>% bind_rows()
    
    lb$Significant <- ifelse(lb$p_value < 0.05,
                             "Yes \u2713 (autocorrelation remains)",
                             "No \u2717 (no significant autocorrelation)")
    lb$statistic <- round(lb$statistic, 3)
    lb$p_value   <- sprintf("%.4f", lb$p_value)
    
    DT::datatable(lb,
                  options = list(
                    pageLength = 5, dom = "Bfrtip",
                    buttons = list(
                      list(extend = "csv",   text = "CSV"),
                      list(extend = "excel", text = "Excel"),
                      list(extend = "pdf",   text = "PDF"),
                      list(extend = "print", text = "Print")
                    ),
                    columnDefs = list(
                      list(className = "dt-left",  targets = 0),
                      list(className = "dt-center", targets = 1:4)
                    )
                  ),
                  extensions = "Buttons", rownames = FALSE,
                  colnames = c("Lags", "Chi-square(\u03C7\u00B2)", "df", "p-value", "Significant"),
                  caption = tags$caption(
                    style = "caption-side:bottom;text-align:left;",
                    tags$span(style = "color:black;font-weight:bold;font-style:italic;",
                              "Ljung-Box tests on deviance residuals of the best negative-binomial GAM.
                              Because this GAM has no corARMA correction, non-significant p-values here indicate the model's temporal smooth
                              terms already capture the serial structure adequately; significant p-values flag remaining autocorrelation the
                              GAMM was built to address."))
    )
  })
  
  # ==================================================================================
  # GAM PLOTS  (sidebar: "6 GAM Plots")
  # ==================================================================================
  
  observeEvent(input$reset_plots, {
    reset_tab_filters(session, data, region_input_id = "plot_region", default_region = "Northern")
  })
  
  plot_best_model <- reactive({
    req(input$plot_region)
    get_best_model(get_models(input$plot_region))
  })
  
  available_params <- reactive({
    bm <- plot_best_model()
    req(!is.null(bm))
    rownames(summary(bm)$s.table)
  })
  
  output$dynamic_plots <- renderUI({
    params <- available_params()
    term_map <- list(
      "s(time)"              = list(id = "model_plots_1", label = "s(time)"),
      "s(months)"            = list(id = "model_plots_2", label = "s(months)"),
      "s(rainfall)"          = list(id = "model_plots_3", label = "s(rainfall)"),
      "s(avgtemp)"           = list(id = "model_plots_4", label = "s(avgtemp)"),
      "ti(time,months)"      = list(id = "model_plots_5", label = "ti(time,months)"),
      "ti(avgtemp,rainfall)" = list(id = "model_plots_6", label = "ti(avgtemp,rainfall)")
    )
    avail <- Filter(function(t) t$label %in% params, term_map)
    if (length(avail) == 0)
      return(div(style = "text-align:center;padding:50px;",
                 h4("No smooth terms available.", style = "color:gray;")))
    
    make_panel <- function(t) {
      div(style = "margin-bottom:14px;",
          plotlyOutput(t$id))
    }
    
    plots <- lapply(avail, make_panel)
    rows <- lapply(seq(1, length(plots), by = 2), function(i) {
      if (i + 1 <= length(plots))
        fluidRow(column(6, plots[[i]]), column(6, plots[[i + 1]]))
      else
        fluidRow(column(6, plots[[i]]), column(6, div()))
    })
    do.call(tagList, rows)
  })
  
  
  make_gam_plot <- function(term, xlab, ylab = "Partial effect") {
    bm <- plot_best_model()
    req(!is.null(bm), term %in% available_params())
    p <- gratia::draw(bm, overall_uncertainty = TRUE, select = term,
                      smooth_col = "black", ci_col = "cyan",
                      smooth_lwd = 30, ci_lwd = 30, alpha = 0.5) +
      labs(x = xlab, y = ylab, title = NULL) + theme_gam_plots()
    make_static_plotly(p, filename = paste0(gsub("[^a-zA-Z0-9]", "_", term), "_", input$plot_region))
  }
  
  output$model_plots_1 <- renderPlotly({ make_gam_plot("s(time)",              "Time") })
  output$model_plots_2 <- renderPlotly({ make_gam_plot("s(months)",            "Months") })
  output$model_plots_3 <- renderPlotly({ make_gam_plot("s(rainfall)",          "Rainfall (mm)") })
  output$model_plots_4 <- renderPlotly({ make_gam_plot("s(avgtemp)",           "Average temperature (\u00b0C)") })
  output$model_plots_5 <- renderPlotly({ make_gam_plot("ti(time,months)",      "Time", "Months") })
  output$model_plots_6 <- renderPlotly({ make_gam_plot("ti(avgtemp,rainfall)", "Average temperature (\u00b0C)", "Rainfall (mm)") })
  
  
  # ====================================================================================
  # SECTION 10 — GAMM ESTIMATES  (sidebar: "4G GAMM Estimates")
  #   Tabs: GAMM Metrics | GAMM Smooth Terms | GAMM Forecast Plot | GAMM Forecast Table
  # ====================================================================================
  
  observeEvent(input$reset_gest_metrics, { updateSelectInput(session, "gamm_model_region", selected = "Upper East") })
  observeEvent(input$reset_gest_smooth,  { updateSelectInput(session, "gamm_smooth_region", selected = "Upper East") })
  observeEvent(input$reset_gforecast_plot,  { updateSelectInput(session, "gamm_forecast_region_plot", selected = "Upper East") })
  observeEvent(input$reset_gforecast_table, { updateSelectInput(session, "gamm_forecast_region_table", selected = "Upper East") })
  
  output$gamm_model_metrics <- DT::renderDataTable({
    req(input$gamm_model_region)
    grid <- get_gamm_grid(input$gamm_model_region)
    req(!is.null(grid))
    meta <- get_gamm_meta(input$gamm_model_region)
    
    grid$Best          <- ifelse(grid$model == meta$best_label, "Yes \u2605", "No")
    grid$pql_converged <- ifelse(grid$pql_converged, "Yes \u2713", "No \u2717")
    grid$AIC    <- round(grid$AIC, 2)
    grid$BIC    <- round(grid$BIC, 2)
    grid$logLik <- round(grid$logLik, 2)
    
    if (!"Adj_R2" %in% names(grid)) grid$Adj_R2 <- NA_real_
    grid$Adj_R2 <- ifelse(is.na(grid$Adj_R2), "\u2014", sprintf("%.1f%%", grid$Adj_R2 * 100))
    
    grid$model <- sprintf("corARMA (%d, %d)", grid$p, grid$q)
    
    DT::datatable(
      grid[, c("model", "p", "q", "npar", "logLik", "AIC", "BIC", "Adj_R2", "pql_converged", "Best")],
      options = list(
        striped = TRUE, hover = TRUE, bordered = TRUE, dom = "Bfrtip",
        buttons = list(list(extend = "csv"), list(extend = "excel"),
                       list(extend = "pdf"), list(extend = "print")),
        columnDefs = list(list(className = "dt-center", targets = 1:9))
      ),
      extensions = "Buttons", rownames = FALSE,
      colnames = c("Model Structure", "AR (p)", "MA (q)", "npar", "logLik", "AIC", "BIC", "Adj. R\u00b2",
                   "PQL Converged", "Best Model"),
      caption = tags$caption(
        style = "caption-side:bottom;text-align:left;",
        tags$span(style = "color:black;font-weight:bold;font-style:italic;",
                  "npar=number of parameters; logLik=Log Likelihood; AIC/BIC=Akaike/Bayesian Information Criterion (lower is better);
                  Adj. R\u00b2=adjusted R-squared of the underlying GAM component (higher is better); PQL=Penalized Quasi-Likelihood used to fit the GAMM;
                  the grid searches corARMA(p,q) structures around the region's winning negative-binomial smoother.")),
      callback = JS(
        "table.on('draw.dt', function(){",
        "  table.rows().every(function(){",
        "    var d = this.data();",
        "    if(d[9] && d[9].indexOf('Yes') !== -1){",
        "      $(this.node()).css({'background-color':'#ffd966','font-weight':'bold'});",
        "    }",
        "  });",
        "});"
      )
    )
  })
  
  output$gamm_best_model_smooth_terms <- DT::renderDataTable({
    req(input$gamm_smooth_region)
    bg <- get_gamm_model(input$gamm_smooth_region)
    req(!is.null(bg))
    
    st   <- as.data.frame(round(summary(bg$gam)$s.table, 4))
    st   <- cbind(Parameters = rownames(st), st)
    pt   <- as.data.frame(round(summary(bg$gam)$p.table, 4))
    pt   <- cbind(Parameters = rownames(pt), pt)
    irow <- pt[pt$Parameters == "(Intercept)", ]
    
    colnames(irow) <- colnames(st) <- c("Parameters", "Col2", "Col3", "Col4", "Col5")
    irow[] <- lapply(irow, function(x) if (is.numeric(x)) format(x, nsmall = 4) else x)
    st[]   <- lapply(st,   function(x) if (is.numeric(x)) format(x, nsmall = 4) else x)
    
    hdr <- data.frame(Parameters = "Non\u2011parametric terms",
                      Col2 = "edf", Col3 = "Ref.df", Col4 = "F", Col5 = "p-value",
                      stringsAsFactors = FALSE)
    full <- rbind(irow, hdr, st)
    colnames(full) <- c("Parameters", "Estimate", "Std. Error", "t-value", "p-value")
    
    DT::datatable(full,
                  options = list(
                    striped = TRUE, hover = TRUE, bordered = TRUE, dom = "Bfrtip",
                    buttons = list(list(extend = "csv"), list(extend = "excel"),
                                   list(extend = "pdf"), list(extend = "print")),
                    columnDefs = list(list(className = "dt-center", targets = 1:4)),
                    rowCallback = JS(
                      "function(row, data){",
                      "  if(data[0]==='Non\u2011parametric terms'){$(row).addClass('section-header');}",
                      "}"
                    )
                  ),
                  extensions = "Buttons", rownames = FALSE,
                  caption = tags$caption(class = "data-table-title",
                                         "Parametric and Non\u2011Parametric Estimates of the Best GAMM (PQL-based)")
    )
  })
  
  output$gamm_corr_info <- renderUI({
    req(input$smooth_region)
    meta <- get_gamm_meta(input$smooth_region)
    req(!is.null(meta))
    
    nums <- as.integer(regmatches(meta$best_label, gregexpr("[0-9]+", meta$best_label))[[1]])
    p_order <- if (length(nums) == 2) nums[1] else NA_integer_
    q_order <- if (length(nums) == 2) nums[2] else NA_integer_
    
    structure_label <- format_corarma_label(meta$best_label)
    
    phi <- meta$phi
    
    coef_table <- NULL
    if (!all(is.na(phi)) && length(phi) > 0) {
      coef_names <- names(phi)
      coef_type  <- ifelse(grepl("^Phi",   coef_names), "AR (\u03D5)",
                           ifelse(grepl("^Theta", coef_names), "MA (\u03B8)", "Other"))
      coef_table <- data.frame(
        Term = coef_names, Type = coef_type,
        Coefficient = sprintf("%.4f", as.numeric(phi)),
        stringsAsFactors = FALSE
      )
      coef_table <- coef_table[order(coef_table$Type, coef_table$Term), ]
    }
    
    div(class = "summary-section-card", style = "margin-top:16px;",
        h4(tags$i(class = "fa fa-wave-square"), "Correlation Structure (Best Model)"), tags$hr(),
        
        div(class = "summary-info-row",
            span(class = "summary-info-key", "Winning structure"),
            span(class = "summary-info-val", structure_label)),
        
        if (!is.na(p_order) && !is.na(q_order))
          div(class = "summary-info-row",
              span(class = "summary-info-key", "AR order (p) / MA order (q)"),
              span(class = "summary-info-val", sprintf("p = %d, q = %d", p_order, q_order))),
        
        tags$br(),
        
        if (is.null(coef_table)) {
          div(style = "font-size:14px;color:#555;padding:6px 0;",
              tags$em(sprintf(
                "No correlation structure was needed for %s (best model is %s) \u2014 the plain negative-binomial GAM already adequately captured the residual correlation.",
                input$smooth_region, structure_label
              )))
        } else {
          tags$table(
            class = "table table-sm table-bordered", style = "margin-bottom:10px;",
            tags$thead(tags$tr(tags$th("Term"), tags$th("Type"), tags$th("Coefficient"))),
            tags$tbody(
              lapply(seq_len(nrow(coef_table)), function(i) {
                tags$tr(
                  tags$td(coef_table$Term[i]),
                  tags$td(coef_table$Type[i]),
                  tags$td(coef_table$Coefficient[i])
                )
              })
            )
          )
        },
        
        div(class = "summary-info-row",
            span(class = "summary-info-key", "NB theta (dispersion)"),
            span(class = "summary-info-val", round(meta$theta, 3))))
  })
  
  output$gamm_forecast_plot <- renderPlotly({
    req(input$gamm_forecast_region_plot)
    bg <- get_gamm_model(input$gamm_forecast_region_plot); req(!is.null(bg))
    bm <- bg$gam
    reg <- input$gamm_forecast_region_plot
    rd <- data %>% filter(region == reg) %>% arrange(date)
    last_time <- max(rd$time, na.rm = TRUE); last_date <- max(rd$date, na.rm = TRUE)
    future_months <- seq(last_date %m+% months(1), by = "month", length.out = 48)
    offs <- forecast_offset(rd, reg)
    fg <- data.frame(time = seq(last_time + 1, by = 1, length.out = 48),
                     months = as.numeric(format(future_months, "%m")),
                     rainfall = mean(rd$rainfall, na.rm = TRUE), avgtemp = mean(rd$avgtemp, na.rm = TRUE),
                     log_pop_offset = offs, date = future_months)
    pred <- suppressWarnings(predict(bm, newdata = fg, type = "response", se.fit = TRUE))
    
    # Forecast horizon: population held fixed at last observed value
    pop_last <- exp(offs)
    fg$fit <- (pred$fit / pop_last) * 1000
    fg$lwr <- pmax(0, pred$fit - 1.96 * pred$se.fit) / pop_last * 1000
    fg$upr <- (pred$fit + 1.96 * pred$se.fit) / pop_last * 1000
    
    # Historical: population varies by row — pull it explicitly
    pop <- get_population_series(rd)
    hist_df <- data.frame(date = rd$date, fit = (rd$uncom / pop) * 1000)
    
    p <- ggplot() +
      geom_line(data = hist_df, aes(x = date, y = fit), color = "black", linewidth = 0.8) +
      geom_ribbon(data = fg, aes(x = date, ymin = lwr, ymax = upr), alpha = 0.2, fill = "#8e44ad") +
      geom_line(data = fg, aes(x = date, y = fit), color = "#8e44ad", linewidth = 1, linetype = "dashed") +
      geom_vline(xintercept = last_date, linetype = "dotted", color = "red") +
      scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
      labs(x = "", y = "Incidence (per 1,000 population)") + theme_residuals()
    make_static_plotly(p, filename = paste0("gamm_forecast_", reg))
  })
  
  output$gamm_forecast_table <- DT::renderDataTable({
    req(input$gamm_forecast_region_table)
    bg <- get_gamm_model(input$gamm_forecast_region_table); req(!is.null(bg))
    bm <- bg$gam
    rd <- data %>% filter(region == input$gamm_forecast_region_table) %>% arrange(date)
    last_time <- max(rd$time, na.rm = TRUE); last_date <- max(rd$date, na.rm = TRUE)
    future_months <- seq(last_date %m+% months(1), by = "month", length.out = 48)
    offs <- forecast_offset(rd, input$gamm_forecast_region_table)
    fg <- data.frame(time = seq(last_time + 1, by = 1, length.out = 48),
                     months = as.numeric(format(future_months, "%m")),
                     rainfall = mean(rd$rainfall, na.rm = TRUE), avgtemp = mean(rd$avgtemp, na.rm = TRUE),
                     log_pop_offset = offs)
    pred <- suppressWarnings(predict(bm, newdata = fg, type = "response", se.fit = TRUE))
    
    pop_last <- exp(offs)
    
    out <- data.frame(Month = format(future_months, "%b %Y"),
                      Predicted = round((pred$fit / pop_last) * 1000, 2),
                      Lower_CI  = round(pmax(0, pred$fit - 1.96 * pred$se.fit) / pop_last * 1000, 2),
                      Upper_CI  = round((pred$fit + 1.96 * pred$se.fit) / pop_last * 1000, 2))
    DT::datatable(out, options = list(pageLength = 12, dom = "Bfrtip",
                                      buttons = list(list(extend = "csv"), list(extend = "excel"), list(extend = "pdf"), list(extend = "print")),
                                      columnDefs = list(list(className = "dt-center", targets = 1:3))),
                  extensions = "Buttons", rownames = FALSE,
                  colnames = c("Month", "Predicted (per 1,000 pop)", "Lower 95% CI", "Upper 95% CI"),
                  caption = tags$caption(
                    style = "caption-side:bottom;text-align:left;",
                    tags$span(style = "color:black;font-weight:bold;font-style:italic;",
                              "Forecast expressed as incidence (per 1,000 population), holding population fixed at its most recently observed level for the full 48-month horizon.")))
  })
  
  
  # ==================================================================================
  # GAMM DIAGNOSTICS  (sidebar: "5G GAMM Diagnostics")
  # ==================================================================================
  
  observeEvent(input$reset_gdiag1, { updateSelectInput(session, "gamm_diag_region1", selected = "Upper East") })
  observeEvent(input$reset_gdiag2, { updateSelectInput(session, "gamm_diag_region2", selected = "Upper East") })
  observeEvent(input$reset_gdiag3, { updateSelectInput(session, "gamm_diag_region3", selected = "Upper East") })
  observeEvent(input$reset_gdiag4, {
    updateSelectInput(session, "gamm_diag_region4", selected = "Upper East")
    updateSliderInput(session, "gamm_max_lag", value = 12)
  })
  observeEvent(input$reset_gdiag5, {
    updateSelectInput(session, "gamm_concurvity_region", selected = "Upper East")
    updateSelectInput(session, "gamm_concurvity_type", selected = "estimate")
  })
  
  output$gamm_observed_fitted <- renderPlotly({
    req(input$gamm_diag_region1)
    bg <- get_gamm_model(input$gamm_diag_region1); req(!is.null(bg))
    rd <- data %>% dplyr::filter(region == input$gamm_diag_region1) %>% arrange(time)
    pd <- data.frame(Date = rd$date, Observed = rd$uncom / 1000, Fitted = fitted(bg$gam) / 1000)
    p <- ggplot(pd, aes(x = Date)) +
      geom_line(aes(y = Observed, color = "Observed"), linewidth = 1) +
      geom_line(aes(y = Fitted, color = "Fitted"), linewidth = 1, linetype = "dashed") +
      scale_color_manual(values = c(Observed = "black", Fitted = "#F8766D")) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
      labs(title = NULL, x = "Year", y = "Malaria cases (x10\u00b3)") + theme_residuals()
    make_static_plotly(p, filename = paste0("gamm_obs_fit_", input$gamm_diag_region1))
  })
  
  output$gamm_qq_plot <- renderPlotly({
    req(input$gamm_diag_region2)
    bg <- get_gamm_model(input$gamm_diag_region2); req(!is.null(bg))
    p <- gratia::qq_plot(bg$gam, method = "simulate") +
      labs(title = NULL, x = "Theoretical Quantiles", y = "Deviance residuals") + theme_residuals()
    make_static_plotly(p, filename = paste0("gamm_qq_", input$gamm_diag_region2))
  })
  
  output$gamm_response_fitted_plot <- renderPlotly({
    req(input$gamm_diag_region3)
    bg <- get_gamm_model(input$gamm_diag_region3); req(!is.null(bg))
    rd <- data %>% filter(region == input$gamm_diag_region3) %>% arrange(time)
    pd <- data.frame(Fitted = fitted(bg$gam) / 1000, Observed = rd$uncom / 1000)
    p <- ggplot(pd, aes(x = Fitted, y = Observed)) +
      geom_point(size = 2.5, color = "#F8766D") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "steelblue") +
      labs(x = "Fitted values (x10\u00b3)", y = "Malaria cases (x10\u00b3)") + theme_residuals()
    make_static_plotly(p, filename = paste0("gamm_resp_fit_", input$gamm_diag_region3))
  })
  
  output$gamm_diag_acf_plot <- renderPlotly({
    req(input$gamm_diag_region4, input$gamm_max_lag)
    bg <- get_gamm_model(input$gamm_diag_region4)
    req(!is.null(bg))
    
    resid_norm <- residuals(bg$lme, type = "normalized")
    
    acf_res <- acf(resid_norm, lag.max = input$gamm_max_lag, plot = FALSE)
    acf_df <- data.frame(lag = as.numeric(acf_res$lag), acf = as.numeric(acf_res$acf))
    
    conf_level <- 0.95
    ci <- qnorm((1 + conf_level) / 2) / sqrt(length(resid_norm))
    acf_df$upper_band <- ci
    acf_df$lower_band <- -ci
    
    acf_plot <- ggplot(acf_df, aes(lag, acf)) +
      geom_ribbon(aes(ymin = lower_band, ymax = upper_band), fill = "steelblue", alpha = 0.20) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
      geom_hline(yintercept = c(ci, -ci), linetype = "dashed", colour = "blue") +
      geom_segment(aes(xend = lag, y = 0, yend = acf), colour = "lightblue", linewidth = 0.8) +
      geom_col(width = 0.30, fill = "steelblue") +
      labs(title = "", x = "Lag", y = "Autocorrelatation function (ACF)") +
      theme_autocorr_plots()
    
    make_static_plotly(acf_plot, filename = paste0("gamm_diag_acf_", input$gamm_diag_region4))
  })
  
  output$gamm_diag_pacf_plot <- renderPlotly({
    req(input$gamm_diag_region4, input$gamm_max_lag)
    bg <- get_gamm_model(input$gamm_diag_region4)
    req(!is.null(bg))
    
    resid_norm <- residuals(bg$lme, type = "normalized")
    
    pacf_res <- pacf(resid_norm, lag.max = input$gamm_max_lag, plot = FALSE)
    pacf_df <- data.frame(lag = as.numeric(pacf_res$lag), pacf = as.numeric(pacf_res$acf))
    
    conf_level <- 0.95
    ci <- qnorm((1 + conf_level) / 2) / sqrt(length(resid_norm))
    pacf_df$upper_band <- ci
    pacf_df$lower_band <- -ci
    
    pacf_plot <- ggplot(pacf_df, aes(lag, pacf)) +
      geom_ribbon(aes(ymin = lower_band, ymax = upper_band), fill = "steelblue", alpha = 0.20) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
      geom_hline(yintercept = c(ci, -ci), linetype = "dashed", colour = "blue") +
      geom_segment(aes(xend = lag, y = 0, yend = pacf), colour = "lightblue", linewidth = 0.8) +
      geom_col(width = 0.30, fill = "steelblue") +
      labs(title = "", x = "Lag", y = "Partial Autocorrelatation function (PACF)") +
      theme_autocorr_plots()
    
    make_static_plotly(pacf_plot, filename = paste0("gamm_diag_pacf_", input$gamm_diag_region4))
  })
  
  output$gamm_diag_ljung_box_table <- DT::renderDataTable({
    req(input$gamm_diag_region4)
    meta <- get_gamm_meta(input$gamm_diag_region4); req(!is.null(meta))
    lb <- meta$ljung_box
    lb$Significant <- ifelse(lb$p_value < 0.05, "Yes \u2713 (autocorrelation remains)", "No \u2717 (no significant autocorrelation)")
    lb$statistic <- round(lb$statistic, 3); lb$p_value <- sprintf("%.4f", lb$p_value)
    DT::datatable(lb, options = list(pageLength = 5, dom = "Bfrtip",
                                     buttons = list(list(extend = "csv"), list(extend = "excel"), list(extend = "pdf"), list(extend = "print")),
                                     columnDefs = list(list(className = "dt-center", targets = 1:4))),
                  extensions = "Buttons", rownames = FALSE,
                  colnames = c("Lags", "Chi-square(\u03C7\u00B2)", "df", "p-value", "Significant"))
  })
  
  # ==================================================================================
  # GAMM PLOTS  (sidebar: "6G GAMM Plots")
  # ==================================================================================
  
  observeEvent(input$reset_gplots, { updateSelectInput(session, "gamm_plot_region", selected = "Upper East") })
  
  gamm_plot_best_model <- reactive({
    req(input$gamm_plot_region)
    bg <- get_gamm_model(input$gamm_plot_region)
    req(!is.null(bg))
    bg$gam
  })
  
  gamm_available_params <- reactive({
    bm <- gamm_plot_best_model()
    req(!is.null(bm))
    rownames(summary(bm)$s.table)
  })
  
  make_gamm_plot <- function(term, xlab, ylab = "Partial effect") {
    bm <- gamm_plot_best_model(); req(term %in% gamm_available_params())
    p <- suppressWarnings(gratia::draw(bm, overall_uncertainty = TRUE, select = term,
                                       smooth_col = "black", ci_col = "cyan",
                                       smooth_lwd = 30, ci_lwd = 30, alpha = 0.5)) +
      labs(x = xlab, y = ylab, title = NULL) + theme_gam_plots()
    make_static_plotly(p, filename = paste0("gamm_", gsub("[^a-zA-Z0-9]", "_", term), "_", input$gamm_plot_region))
  }
  
  output$gamm_model_plots_1 <- renderPlotly({ make_gamm_plot("s(time)", "Time") })
  output$gamm_model_plots_2 <- renderPlotly({ make_gamm_plot("s(months)", "Months") })
  output$gamm_model_plots_3 <- renderPlotly({ make_gamm_plot("s(rainfall)", "Rainfall (mm)") })
  output$gamm_model_plots_4 <- renderPlotly({ make_gamm_plot("s(avgtemp)", "Average temperature (\u00b0C)") })
  output$gamm_model_plots_5 <- renderPlotly({ make_gamm_plot("ti(time,months)", "Time", "Months") })
  output$gamm_model_plots_6 <- renderPlotly({ make_gamm_plot("ti(avgtemp,rainfall)", "Average temperature (\u00b0C)", "Rainfall (mm)") })
  
  output$gamm_dynamic_plots <- renderUI({
    params <- gamm_available_params()
    term_map <- list(
      "s(time)"              = list(id = "gamm_model_plots_1", label = "s(time)"),
      "s(months)"            = list(id = "gamm_model_plots_2", label = "s(months)"),
      "s(rainfall)"          = list(id = "gamm_model_plots_3", label = "s(rainfall)"),
      "s(avgtemp)"           = list(id = "gamm_model_plots_4", label = "s(avgtemp)"),
      "ti(time,months)"      = list(id = "gamm_model_plots_5", label = "ti(time,months)"),
      "ti(avgtemp,rainfall)" = list(id = "gamm_model_plots_6", label = "ti(avgtemp,rainfall)")
    )
    avail <- Filter(function(t) t$label %in% params, term_map)
    if (length(avail) == 0)
      return(div(style = "text-align:center;padding:50px;",
                 h4("No smooth terms available.", style = "color:gray;")))
    
    make_panel <- function(t) {
      div(style = "margin-bottom:14px;",
          #div(style = "margin-bottom:4px;", tags$strong(t$label)),
          plotlyOutput(t$id))
    }
    
    plots <- lapply(avail, make_panel)
    rows <- lapply(seq(1, length(plots), by = 2), function(i) {
      if (i + 1 <= length(plots))
        fluidRow(column(6, plots[[i]]), column(6, plots[[i + 1]]))
      else
        fluidRow(column(6, plots[[i]]), column(6, div()))
    })
    do.call(tagList, rows)
  })
  
  
  # ==================================================================================
  # APPENDIX  (sidebar: "Appendix")
  #   Tabs: Line Plots | Heatmaps | Seasonal Pattern | GAM & GAMM Framework
  # ==================================================================================
  
  combined_filtered_data <- reactive({
    req(input$combined_dateRange)
    data %>%
      dplyr::filter(date >= as.Date(input$combined_dateRange[1]),
                    date <= as.Date(input$combined_dateRange[2])) %>%
      arrange(region, date) %>% mutate(date = as.Date(date))
  })
  
  output$combined_plot <- renderPlotly({
    pd <- combined_filtered_data()
    req(nrow(pd) > 0)
    p1 <- ggplot(pd, aes(x = date, y = uncom / 1000, color = region)) + geom_line() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = "", y = "Malaria cases(x10\u00b3)", color = "Region") + theme_app_series()
    p2 <- ggplot(pd, aes(x = date, y = rainfall, color = region)) + geom_line() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = "", y = "Rainfall (mm)", color = "Region") + theme_app_series()
    p3 <- ggplot(pd, aes(x = date, y = avgtemp, color = region)) + geom_line() +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(limits = c(22.5, 35), breaks = seq(22.5, 35, by = 2.5)) +
      labs(x = "Year", y = "Avg Temperature (\u00b0C)", color = "Region") +
      theme_classic(base_size = 12) +
      theme(axis.title = element_text(face = "bold", size = 12),
            axis.text = element_text(face = "bold", size = 12),
            panel.background = element_rect(fill = "#f0f0f0"),
            legend.position = "none") +
      guides(color = guide_legend(nrow = 1, title = NULL))
    p1p <- ggplotly(p1); p2p <- ggplotly(p2); p3p <- ggplotly(p3)
    for (i in seq_along(p1p$x$data)) p1p$x$data[[i]]$showlegend <- FALSE
    for (i in seq_along(p2p$x$data)) p2p$x$data[[i]]$showlegend <- FALSE
    for (i in seq_along(p3p$x$data)) p3p$x$data[[i]]$showlegend <- TRUE
    combined <- subplot(p1p, p2p, p3p, nrows = 3, shareX = TRUE, titleY = TRUE)
    combined %>%
      layout(showlegend = TRUE,
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.12, yanchor = "top", font = list(size = 14)),
             yaxis = list(fixedrange = TRUE, title = list(text = "<b>Malaria cases(x10\u00b3)</b>", font = list(size = 14))),
             yaxis2 = list(fixedrange = TRUE, title = list(text = "<b>Rainfall (mm)</b>", font = list(size = 14))),
             yaxis3 = list(fixedrange = TRUE, title = list(text = "<b>Avg Temperature (\u00b0C)</b>", font = list(size = 14))),
             xaxis = list(fixedrange = TRUE), xaxis2 = list(fixedrange = TRUE), xaxis3 = list(fixedrange = TRUE)) %>%
      style(hoverinfo = "none") %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
                                        "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
                                        "sendDataToCloud", "toggleSpikelines", "toImage"),
             displaylogo = FALSE)
  })
  
  output$download_combined_plot <- downloadHandler(
    filename = function() paste0("combined_plot_", Sys.Date(), ".png"),
    content = function(file) {
      pd <- combined_filtered_data()
      p1 <- ggplot(pd, aes(x = date, y = uncom / 1000, color = region)) + geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") + labs(x = "", y = "Malaria cases(x10\u00b3)", color = "Region") + theme_app_series()
      p2 <- ggplot(pd, aes(x = date, y = rainfall, color = region)) + geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") + labs(x = "", y = "Rainfall (mm)", color = "Region") + theme_app_series()
      p3 <- ggplot(pd, aes(x = date, y = avgtemp, color = region)) + geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        scale_y_continuous(limits = c(22.5, 35), breaks = seq(22.5, 35, by = 2.5)) +
        labs(x = "Year", y = "Avg Temperature (\u00b0C)", color = "Region") +
        theme_classic(base_size = 12) +
        theme(axis.title = element_text(face = "bold", size = 12), axis.text = element_text(face = "bold", size = 12),
              panel.background = element_rect(fill = "#f0f0f0"), legend.position = "bottom") +
        guides(color = guide_legend(nrow = 1, title = NULL))
      ggsave(file, plot = gridExtra::arrangeGrob(p1, p2, p3, ncol = 1, nrow = 3), width = 10, height = 10, dpi = 300, device = "png")
    }
  )
  
  heatmap_data <- reactive({
    req(input$heatmap_dateRange)
    data %>%
      dplyr::filter(date >= as.Date(input$heatmap_dateRange[1]),
                    date <= as.Date(input$heatmap_dateRange[2])) %>%
      mutate(month = factor(month, levels = month.abb)) %>%
      group_by(month, region) %>%
      summarise(uncom = mean(uncom, na.rm = TRUE), avgtemp = mean(avgtemp, na.rm = TRUE),
                rainfall = mean(rainfall, na.rm = TRUE), .groups = "drop")
  })
  
  make_heatmaps <- function(df) {
    h1 <- ggplot(df, aes(x = region, y = month, fill = uncom / 1000)) +
      geom_tile(color = "white") +
      scale_fill_gradient(name = "Malaria cases(x10\u00b3)", low = "#f7fcfc", high = "#4d0c4b") +
      labs(x = "", y = "Month", title = "") + theme_heatmap1()
    
    h2 <- ggplot(df, aes(x = region, y = month, fill = rainfall)) +
      geom_tile(color = "white") +
      scale_fill_gradient(name = "Rainfall (mm)", low = "#f7fbff", high = "#08316b") +
      labs(x = "", y = "Month", title = "") + theme_heatmap2() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
    h3 <- ggplot(df, aes(x = region, y = month, fill = avgtemp)) +
      geom_tile(color = "white") +
      scale_fill_distiller(name = "Avg Temperature (\u00b0C)", palette = "Spectral") +
      labs(x = "", y = "Month", title = "") + theme_heatmap3()
    
    list(h1 = h1, h2 = h2, h3 = h3)
  }
  
  output$combined_heatmap <- renderPlot({
    hms <- make_heatmaps(heatmap_data())
    gridExtra::grid.arrange(hms$h1, hms$h2, hms$h3, ncol = 1, nrow = 3)
  })
  
  output$download_heatmap <- downloadHandler(
    filename = function() paste0("combined_heatmap_", Sys.Date(), ".png"),
    content = function(file) {
      hms <- make_heatmaps(heatmap_data())
      ggsave(file, plot = gridExtra::arrangeGrob(hms$h1, hms$h2, hms$h3, ncol = 1, nrow = 3),
             width = 10, height = 14, dpi = 300, device = "png")
    }
  )
  
  stdFun <- function(v, a = 0, b = 1) (v - a) / (b - a)
  
  process_data <- reactive({
    req(!is.na(input$seasonal_dateRange[1]), !is.na(input$seasonal_dateRange[2]),
        input$seasonal_dateRange[1] < input$seasonal_dateRange[2])
    mal_data   <- subset(data, region == input$region)
    start_year <- as.integer(format(input$seasonal_dateRange[1], "%Y"))
    end_year   <- as.integer(format(input$seasonal_dateRange[2], "%Y"))
    pd <- data.frame(year = mal_data$year, month = mal_data$months,
                     uncom = mal_data$uncom, rainfall = mal_data$rainfall, temp = mal_data$avgtemp)
    pd <- pd[pd$year >= start_year & pd$year <= end_year, ]
    validate(need(nrow(pd) >= 12, "Select at least one full year."))
    validate(need(nrow(pd) %% 12 == 0, paste0("Selected range (", nrow(pd), " months) must cover complete years.")))
    
    cases_s    <- ts(pd$uncom,    st = c(start_year, 1), end = c(end_year, 12), fr = 12)
    rainfall_s <- ts(pd$rainfall, st = c(start_year, 1), end = c(end_year, 12), fr = 12)
    temp_s     <- ts(pd$temp,     st = c(start_year, 1), end = c(end_year, 12), fr = 12)
    
    cases_m <- apply(matrix(cases_s,    ncol = 12, byrow = TRUE), 2, sum)
    rain_m  <- apply(matrix(rainfall_s, ncol = 12, byrow = TRUE), 2, mean)
    temp_m  <- apply(matrix(temp_s,     ncol = 12, byrow = TRUE), 2, mean)
    
    std_params <- list(
      "Upper East"    = list(c = c(0, 86e4), r = c(0, 240), t = c(20, 35)),
      "Upper West"    = list(c = c(0, 43e4), r = c(0, 240), t = c(25, 35)),
      "Northern"      = list(c = c(0, 57e4), r = c(0, 200), t = c(25, 35)),
      "Brong Ahafo"   = list(c = c(0, 105e4), r = c(0, 180), t = c(25, 30)),
      "Ashanti"       = list(c = c(0, 73e4), r = c(0, 280), t = c(25, 30)),
      "Eastern"       = list(c = c(0, 80e4), r = c(0, 205), t = c(25, 30)),
      "Volta"         = list(c = c(0, 50e4), r = c(0, 205), t = c(25, 30)),
      "Greater Accra" = list(c = c(0, 30e4), r = c(0, 180), t = c(25, 30)),
      "Central"       = list(c = c(0, 60e4), r = c(0, 230), t = c(24, 29)),
      "Western"       = list(c = c(0, 75e4), r = c(0, 335), t = c(25, 30))
    )
    p <- std_params[[input$region]]
    list(cases_month = stdFun(cases_m, p$c[1], p$c[2]),
         average_monthly_rainfall = stdFun(rain_m, p$r[1], p$r[2]),
         average_monthly_temperature = stdFun(temp_m, p$t[1], p$t[2]))
  })
  
  create_plot <- function() {
    pd        <- process_data()
    monthLabs <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    axTiks    <- seq(0, 1, length.out = 12)
    xpoly <- ypoly <- c()
    for (i in 1:12) {
      xpoly <- c(xpoly, c(rep(axTiks[i] - .04, 2), rep(axTiks[i] + .04, 2), NA))
      ypoly <- c(ypoly, c(0, rep(pd$cases_month[i], 2), 0, NA))
    }
    par(mar = c(4, 3.5, 2, 8), las = 1, mfrow = c(1, 1))
    plot(0, type = "n", xlim = c(-.05, 1.05), ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
    polygon(cbind(xpoly, ypoly), col = "#BDC9E1")
    yCases <- seq(0, 1, length.out = 6)
    max_cases_lookup <- c("Upper East" = 860, "Upper West" = 430, "Northern" = 565, "Brong Ahafo" = 1050,
                          "Ashanti" = 730, "Eastern" = 800, "Volta" = 500, "Greater Accra" = 300, "Central" = 600, "Western" = 750)
    axis(2, yCases, seq(0, max_cases_lookup[[input$region]], length.out = 6),
         line = -1.3, lwd = 2, hadj = .75, padj = 0.05, cex.axis = 1.5, font.axis = 2, col = "black")
    points(axTiks, pd$average_monthly_temperature, type = "l", col = "#045A8D", lwd = 4)
    points(axTiks, pd$average_monthly_temperature, type = "b", col = "#045A8D", lwd = 2, pch = 19)
    temp_range_lookup <- list("Upper East" = c(20, 35), "Upper West" = c(25, 35), "Northern" = c(25, 35),
                              "Brong Ahafo" = c(25, 30), "Ashanti" = c(25, 30), "Eastern" = c(25, 30),
                              "Volta" = c(25, 30), "Greater Accra" = c(25, 30), "Central" = c(24, 29), "Western" = c(25, 30))
    axis(4, yCases, seq(temp_range_lookup[[input$region]][1], temp_range_lookup[[input$region]][2], length.out = 6),
         line = -0.1, col = "#045A8D", lwd = 2, hadj = .4, padj = .05, cex.axis = 1.5, font.axis = 2)
    points(axTiks, pd$average_monthly_rainfall, type = "b", col = "#8F0E00", lwd = 2, pch = 19)
    points(axTiks, pd$average_monthly_rainfall, type = "l", col = "#8F0E00", lwd = 4)
    max_rain_lookup <- c("Upper East" = 240, "Upper West" = 240, "Northern" = 200, "Brong Ahafo" = 180,
                         "Ashanti" = 280, "Eastern" = 205, "Volta" = 205, "Greater Accra" = 180, "Central" = 230, "Western" = 335)
    axis(4, yCases, seq(0, max_rain_lookup[[input$region]], length.out = 6),
         line = 2.9, col = "#8F0E00", lwd = 2, hadj = .4, padj = .05, cex.axis = 1.5, font.axis = 2)
    mtext(expression(bold("Malaria cases (x10\u00b3)")), 2, las = 0, line = 1.4, cex = 1.5, col = "black")
    mtext(expression(bold("Temperature (\u00b0C)")),    4, las = 0, line = -1.00, col = "#045A8D", cex = 1.5)
    mtext(expression(bold(paste("Precipitation", (mm)))), 4, las = 0, line = 2.0, col = "#8F0E00", cex = 1.5)
    mtext(input$region, 3, line = -2, cex = 1.5, adj = 0.1, font = 2)
    axis(1, axTiks, monthLabs, las = 1, tick = FALSE, pos = 0, padj = -0.6, font.axis = 2, cex.axis = 1.5)
    mtext(expression(bold("Time (months)")), 1, line = 1.0, cex = 1.5)
    lines(c(-0.07, 2.0), c(stdFun(0, 0, 75), stdFun(0, 0, 75)), col = "#BDC9E1", lwd = 2)
  }
  
  output$malaria_plot <- renderPlot({ create_plot() })
  output$download_plot <- downloadHandler(
    filename = function() paste0("seasonal_pattern_", input$region, ".png"),
    content = function(file) { png(file, width = 2000, height = 1000, res = 150); create_plot(); dev.off() }
  )
  
  
  #============================================================================
  # DESCRIPTIVE TAB
  #============================================================================
  
  observeEvent(input$reset_desc, {
    reset_tab_filters(session, data, region_input_id = "annual_region",
                      year_input_id = "annual_year")
  })
  
  observeEvent(input$reset_overall, {
    reset_tab_filters(session, data, region_input_id = "overall_region",
                      year_range_input_id = "overall_year_range")
  })
  
  observeEvent(input$reset_ts, {
    reset_tab_filters(session, data, region_input_id = "ts_region",
                      date_range_input_id = "dateRange")
  })
  
  observeEvent(input$reset_season, {
    reset_tab_filters(session, data, region_input_id = "season_region_plots",
                      date_range_input_id = "season_dateRange")
    reset_tab_filters(session, data, region_input_id = "season_region_tests",
                      date_range_input_id = "season_dateRange")
  })
  
  observeEvent(input$reset_corr, {
    reset_tab_filters(session, data, region_input_id = "corr_region",
                      year_range_input_id = "corr_year_range")
  })
  
  descriptive_stats <- function(df) {
    min_val  <- round(min(df,  na.rm = TRUE), 2)
    max_val  <- round(max(df,  na.rm = TRUE), 2)
    mean_val <- round(mean(df, na.rm = TRUE), 2)
    sum_val  <- round(sum(df,  na.rm = TRUE), 2)
    sd_val   <- round(sd(df,   na.rm = TRUE), 2)
    cv_val   <- round((sd_val / mean_val) * 100, 2)
    c(min = min_val, max = max_val, mean = mean_val, sum = sum_val, sd = sd_val, cv = cv_val)
  }
  
  filtered_descriptive_data_annual <- reactive({
    req(input$annual_region, input$annual_year)
    data[data$year == input$annual_year & data$region == input$annual_region, ]
  })
  
  stats_annual <- reactive({
    sel <- filtered_descriptive_data_annual()
    build_stats <- function(col, add_sum = FALSE) {
      s <- descriptive_stats(sel[[col]])
      s["sum"] <- if (add_sum) sum(sel[[col]], na.rm = TRUE) else NA
      s
    }
    do.call(cbind, list(
      uncom    = build_stats("uncom", TRUE),
      mintem   = build_stats("mintem", FALSE),
      avgtemp  = build_stats("avgtemp", FALSE),
      maxtem   = build_stats("maxtem", FALSE),
      rainfall = build_stats("rainfall", TRUE)
    ))
  })
  
  fmt_stats_df <- function(mat) {
    df <- as.data.frame(t(mat))
    colnames(df) <- c("Min", "Max", "Mean", "Sum/Total", "SD", "CV(%)")
    rownames(df) <- c("Uncomplicated Malaria", "Minimum Temperature",
                      "Average Temperature", "Maximum Temperature", "Rainfall")
    df <- cbind(Variables = rownames(df), df)
    num_cols <- c("Min", "Max", "Mean", "Sum/Total", "SD", "CV(%)")
    for (col in num_cols)
      df[[col]] <- ifelse(is.na(df[[col]]), NA, sprintf("%.2f", as.numeric(df[[col]])))
    df[is.na(df)] <- "\u2014"
    df
  }
  
  reactive_stats_df_annual <- reactive({ fmt_stats_df(stats_annual()) })
  
  filtered_descriptive_data_overall <- reactive({
    req(input$overall_region, input$overall_year_range)
    data[data$region == input$overall_region &
           data$year >= input$overall_year_range[1] &
           data$year <= input$overall_year_range[2], ]
  })
  
  stats_overall <- reactive({
    sel <- filtered_descriptive_data_overall()
    build_stats <- function(col, add_sum = FALSE) {
      s <- descriptive_stats(sel[[col]])
      s["sum"] <- if (add_sum) sum(sel[[col]], na.rm = TRUE) else NA
      s
    }
    do.call(cbind, list(
      uncom    = build_stats("uncom", TRUE),
      mintem   = build_stats("mintem", FALSE),
      avgtemp  = build_stats("avgtemp", FALSE),
      maxtem   = build_stats("maxtem", FALSE),
      rainfall = build_stats("rainfall", TRUE)
    ))
  })
  
  reactive_stats_df_overall <- reactive({ fmt_stats_df(stats_overall()) })
  
  yoy_change <- reactive({
    req(input$annual_region, input$annual_year)
    yr  <- as.integer(input$annual_year)
    cur <- sum(data$uncom[data$year == yr   & data$region == input$annual_region], na.rm = TRUE)
    prv <- sum(data$uncom[data$year == yr-1 & data$region == input$annual_region], na.rm = TRUE)
    if (prv == 0) return(NULL)
    list(pct = round((cur - prv) / prv * 100, 1), cur = cur, prv = prv, yr = yr)
  })
  
  output$yoy_badge <- renderUI({
    ch <- yoy_change()
    if (is.null(ch)) return(div(class = "yoy-badge yoy-stable", "\u2014 No prior year"))
    yr <- ch$yr
    cls <- if (ch$pct > 0) "yoy-badge yoy-up" else if (ch$pct < 0) "yoy-badge yoy-down" else "yoy-badge yoy-stable"
    icon_cls <- if (ch$pct > 0) "fa-arrow-trend-up" else if (ch$pct < 0) "fa-arrow-trend-down" else "fa-minus"
    div(class = cls,
        tags$i(class = paste("fa", icon_cls)),
        sprintf(" %s%% (%d vs %d)", abs(ch$pct), yr, yr-1))
  })
  
  output$threshold_alert <- renderUI({
    req(input$annual_region, input$annual_year)
    yr_total <- sum(data$uncom[data$year == input$annual_year &
                                 data$region == input$annual_region], na.rm = TRUE)
    reg_mean <- mean(tapply(data$uncom[data$region == input$annual_region],
                            data$year[data$region == input$annual_region], sum), na.rm = TRUE)
    if (yr_total > reg_mean * 1.2)
      div(class = "threshold-alert alert-high",
          tags$i(class = "fa fa-triangle-exclamation"),
          sprintf(" Cases in %s are %.0f%% above the regional annual average.",
                  input$annual_year, (yr_total / reg_mean - 1) * 100))
    else if (yr_total < reg_mean * 0.8)
      div(class = "threshold-alert alert-low",
          tags$i(class = "fa fa-circle-check"),
          sprintf(" Cases in %s are %.0f%% below the regional annual average.",
                  input$annual_year, (1 - yr_total / reg_mean) * 100))
    else
      div(class = "threshold-alert alert-normal",
          tags$i(class = "fa fa-circle-info"),
          " Cases are within the normal range for this region.")
  })
  
  make_dt <- function(df, caption_text, title_text, table_id = NULL, footnote = NULL) {
    opts <- list(
      pageLength = 10, lengthMenu = c(10, 20),
      striped = TRUE, hover = TRUE, bordered = TRUE,
      columnDefs = list(list(className = "dt-center", targets = 1:6)),
      dom = "Bfrtip",
      buttons = list(
        list(extend = "csv",   text = "CSV"),
        list(extend = "excel", text = "Excel"),
        list(extend = "pdf",   text = "PDF"),
        list(extend = "print", text = "Print")
      )
    )
    cb_js <- if (!is.null(table_id))
      JS(sprintf(
        "table.on('init.dt', function(){
           $('<tr><th colspan=\"7\" style=\"border-top:0;\">%s</th></tr>')
             .insertBefore($('#%s thead tr:first'));
         });", title_text, table_id))
    else JS("")
    
    cap <- if (!is.null(footnote))
      tags$caption(style = "caption-side:bottom;text-align:left;",
                   tags$span(style = "color:black;font-weight:bold;font-style:italic;", footnote))
    else NULL
    
    DT::datatable(df, options = opts, extensions = "Buttons",
                  rownames = FALSE, caption = cap, callback = cb_js)
  }
  
  output$statsTableAnnual <- DT::renderDataTable({
    yr <- input$annual_year
    make_dt(reactive_stats_df_annual(),
            caption_text = "",
            title_text = paste0("Annual Descriptive Statistics (", yr, ")"),
            table_id = "statsTableAnnual",
            footnote = paste0("Statistics describe monthly values for ", yr,
                              ". Sum/Total shows annual aggregate for malaria cases and rainfall.",
                              " Min=Minimum, Max=Maximum, SD=Standard deviation, CV=Coefficient of variation."))
  })
  
  output$statsTableOverall <- DT::renderDataTable({
    yr1 <- input$overall_year_range[1]; yr2 <- input$overall_year_range[2]
    make_dt(reactive_stats_df_overall(),
            caption_text = "",
            title_text = paste0("Overall Descriptive Statistics (", yr1, "\u2013", yr2, ")"),
            table_id = "statsTableOverall",
            footnote = paste0("Statistics across ", yr1, "\u2013", yr2,
                              ". Min=Minimum, Max=Maximum, SD=Standard deviation, CV=Coefficient of variation."))
  })
  
  
  corr_data <- reactive({
    req(input$corr_region, input$corr_year_range)
    data %>%
      filter(region == input$corr_region,
             year >= input$corr_year_range[1],
             year <= input$corr_year_range[2])
  })
  
  
  var_colours <- c(rainfall = "#2c63ab", avgtemp = "#c0392b", mintem = "#e67e22", maxtem = "#8e44ad")
  
  output$scatter_rain_malaria <- renderPlotly({
    df <- corr_data()
    p <- ggplot(df, aes(x = rainfall, y = uncom / 1000)) +
      geom_point(alpha = 0.5, color = var_colours["rainfall"]) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE,
                  color = "black", linewidth = 0.8) +
      labs(x = "Rainfall (mm)", y = "Malaria cases (x10\u00b3)", title = paste0()) +
      theme_residuals()
    make_static_plotly(p, filename = paste0("corr_rain_", input$corr_region))
  })
  
  output$scatter_temp_malaria <- renderPlotly({
    df <- corr_data()
    p <- ggplot(df, aes(x = avgtemp, y = uncom / 1000)) +
      geom_point(alpha = 0.5, color = var_colours["avgtemp"]) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE,
                  color = "black", linewidth = 0.8) +
      labs(x = "Average Temperature (\u00b0C)", y = "Malaria cases (x10\u00b3)", title = paste0()) +
      theme_residuals()
    make_static_plotly(p, filename = paste0("corr_temp_", input$corr_region))
  })
  
  output$corr_matrix_plot <- renderPlotly({
    df  <- corr_data() %>% dplyr::select(uncom, rainfall, mintem, avgtemp, maxtem)
    mat <- cor(df, use = "complete.obs")
    
    bold_labels <- paste0("<b>", colnames(mat), "</b>")
    
    plot_ly(z = mat, x = colnames(mat), y = rownames(mat), type = "heatmap",
            colors = colorRamp(c("#2c63ab", "white", "#c0392b")),
            zmin = -1, zmax = 1,
            colorbar = list(title = "", titleside = "right", len = 1, y = 1)
    ) %>%
      style(hoverinfo = "none") %>%
      layout(
        title = "",
        xaxis = list(title = "", fixedrange = TRUE, tickmode = "array",
                     tickvals = colnames(mat), ticktext = bold_labels, tickangle = 0),
        yaxis = list(title = "", fixedrange = TRUE, tickmode = "array",
                     tickvals = rownames(mat), ticktext = bold_labels),
        margin = list(l = 80, b = 80)
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c(
          "zoom2d", "pan2d", "select2d", "lasso2d",
          "zoomIn2d", "zoomOut2d", "autoScale2d",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "sendDataToCloud", "toggleSpikelines"
        ),
        toImageButtonOptions = list(format = "png", filename = paste0("corr_matrix_", input$corr_region)),
        displaylogo = FALSE
      )
  })
  
  output$corr_table <- DT::renderDataTable({
    df   <- corr_data()
    vars <- c("rainfall", "avgtemp", "mintem", "maxtem")
    res  <- lapply(vars, function(v) {
      pc <- cor.test(df[[v]], df$uncom, method = "pearson")
      sc <- suppressWarnings(cor.test(df[[v]], df$uncom, method = "spearman"))
      data.frame(
        Variable = v,
        Pearson_r = round(pc$estimate, 3),
        Pearson_p = sprintf("%.4f", pc$p.value),
        Spearman_r = round(sc$estimate, 3),
        Spearman_p = sprintf("%.4f", sc$p.value)
      )
    })
    out <- do.call(rbind, res)
    DT::datatable(out,
                  options = list(dom = "Bfrtip", pageLength = 5,
                                 buttons = list(list(extend = "csv"), list(extend = "excel"),
                                                list(extend = "pdf"), list(extend = "print")),
                                 columnDefs = list(list(className = "dt-center", targets = 1:4))),
                  extensions = "Buttons", rownames = FALSE,
                  colnames = c("Variable", "Pearson r", "Pearson p", "Spearman r", "Spearman p")
    )
  })
  
  
  #============================================================================
  # TIME SERIES DECOMPOSITION 
  #============================================================================
  output$short_range <- reactive({
    req(input$dateRange)
    diff_days <- as.numeric(difftime(input$dateRange[2], input$dateRange[1], units="days"))
    diff_days < 731
  })
  
  valid_range <- reactive({
    req(input$dateRange)
    diff_days <- as.numeric(difftime(input$dateRange[2], input$dateRange[1], units="days"))
    diff_days >= 730
  })
  
  
  filtered_data <- reactive({
    req(input$ts_region, input$dateRange)
    data %>%
      dplyr::filter(region == input$ts_region,
                    date >= input$dateRange[1],
                    date <= input$dateRange[2])
  })
  
  malaria_ts <- reactive({
    fd <- filtered_data()
    req(nrow(fd) >= 24)
    ts(fd$uncom, frequency = 12,
       start = c(lubridate::year(min(fd$date)), lubridate::month(min(fd$date))))
  })
  
  malaria_decomposed <- reactive({ stl(malaria_ts(), s.window = "periodic") })
  
  decomposed_df <- reactive({
    fd <- filtered_data()
    dec <- malaria_decomposed()
    data.frame(
      date      = seq.Date(min(fd$date), max(fd$date), by = "month"),
      trend     = as.vector(dec$time.series[, "trend"]),
      seasonal  = as.vector(dec$time.series[, "seasonal"]),
      remainder = as.vector(dec$time.series[, "remainder"]),
      observed  = as.vector(malaria_ts())
    )
  })
  
  make_ts_plot <- function(df, col, colour, ylab) {
    p <- ggplot(df, aes(x = date, y = .data[[col]] / 1000)) +
      geom_line(color = colour) +
      labs(x = "", y = ylab) +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      theme_ts_decompose()
    make_static_plotly(p, filename = paste0(col, "_", isolate(input$ts_region)))
  }
  
  output$observed_plot  <- renderPlotly({
    req(valid_range())
    make_ts_plot(decomposed_df(), "observed",  "black",   "Observed (x10\u00b3)")
  })
  
  output$trend_plot     <- renderPlotly({
    req(valid_range())
    make_ts_plot(decomposed_df(), "trend",     "#06752b", "Trend (x10\u00b3)")
  })
  
  output$seasonal_plot  <- renderPlotly({
    req(valid_range())
    make_ts_plot(decomposed_df(), "seasonal",  "#2c63ab", "Seasonal (x10\u00b3)")
  })
  
  output$remainder_plot <- renderPlotly({
    req(valid_range())
    make_ts_plot(decomposed_df(), "remainder", "#00AFBB", "Remainder (x10\u00b3)")
  })
  
  
  #===========================================================
  #SEASONALITY 
  #===========================================================
  observeEvent(input$reset_season_plots, {
    reset_tab_filters(session, data,
                      region_input_id = "season_region_plots",
                      date_range_input_id = "season_dateRange_plots")
  })
  
  observeEvent(input$reset_season_tests, {
    reset_tab_filters(session, data,
                      region_input_id = "season_region_tests",
                      date_range_input_id = "season_dateRange_tests")
  })
  
  
  data_filtered_season <- reactive({
    req(input$season_dateRange)
    data %>%
      filter(date >= input$season_dateRange[1],
             date <= input$season_dateRange[2])
  })
  
  
  data_filtered_plots <- reactive({
    req(input$season_region_plots, input$season_dateRange_plots)
    data %>% filter(region == input$season_region_plots,
                    date >= input$season_dateRange_plots[1],
                    date <= input$season_dateRange_plots[2])
  })
  
  data_filtered_tests <- reactive({
    req(input$season_region_tests, input$season_dateRange_tests)
    data %>% filter(region == input$season_region_tests,
                    date >= input$season_dateRange_tests[1],
                    date <= input$season_dateRange_tests[2])
  })
  
  
  make_boxplot <- function(df, yvar, colour, ylab) {
    df <- df %>% mutate(month = factor(month, levels = month.abb))
    p <- ggplot(df, aes(x = month, y = .data[[yvar]])) +
      geom_boxplot(color = colour) +
      labs(x = "", y = ylab) +
      theme_seasonality()
    make_static_plotly(p, filename = paste0(yvar, "_", isolate(input$season_region_plots)))
  }
  
  output$boxplot1 <- renderPlotly({
    df <- data_filtered_plots() %>% mutate(uncom_scaled = uncom / 1000)
    make_boxplot(df, "uncom_scaled", "black", "Uncomplicated Malaria(x10\u00b3)")
  })
  output$boxplot2 <- renderPlotly({ make_boxplot(data_filtered_plots(), "mintem",   "#FF6664",  "Minimum Temperature(\u00b0C)") })
  output$boxplot3 <- renderPlotly({ make_boxplot(data_filtered_plots(), "avgtemp",  "steelblue", "Average Temperature(\u00b0C)") })
  output$boxplot4 <- renderPlotly({ make_boxplot(data_filtered_plots(), "maxtem",   "orange",    "Maximum Temperature(\u00b0C)") })
  output$boxplot5 <- renderPlotly({ make_boxplot(data_filtered_plots(), "rainfall", "#00AFBB",   "Rainfall(mm)") })
  
  
  output$short_range_tests <- reactive({
    req(input$season_dateRange_tests)
    diff_days <- as.numeric(difftime(input$season_dateRange_tests[2],
                                     input$season_dateRange_tests[1],
                                     units = "days"))
    diff_days < 366
  })
  
  valid_range_tests <- reactive({
    req(input$season_dateRange_tests)
    diff_days <- as.numeric(difftime(input$season_dateRange_tests[2],
                                     input$season_dateRange_tests[1],
                                     units = "days"))
    diff_days >= 365
  })
  
  output$results_table <- DT::renderDataTable({
    
    filtered <- data %>%
      filter(
        region == input$season_region_tests,
        date >= input$season_dateRange_tests[1],
        date <= input$season_dateRange_tests[2]
      )
    
    req(nrow(filtered) > 0)
    
    # Kruskal-Wallis test
    kw_res <- kruskal.test(uncom ~ months, data = filtered)
    df_kw <- length(unique(filtered$months)) - 1
    
    # Friedman test
    wide <- tidyr::pivot_wider(
      filtered,
      id_cols = year,
      names_from = months,
      values_from = uncom
    )
    
    mat <- as.matrix(wide[, -1])
    
    fr_res <- friedman.test(mat)
    df_fr <- ncol(mat) - 1
    
    res_df <- data.frame(
      Test = c("Kruskal-Wallis", "Friedman"),
      Statistic = round(c(kw_res$statistic, fr_res$statistic), 2),
      Degrees_of_Freedom = c(df_kw, df_fr),
      P_Value = sprintf("%.4f",
                        c(kw_res$p.value, fr_res$p.value)),
      Significant = ifelse(
        c(kw_res$p.value, fr_res$p.value) < 0.05,
        "Yes ✓",
        "No ✗"
      )
    )
    
    DT::datatable(
      res_df,
      options = list(
        pageLength = 5,
        dom = "Bfrtip",
        buttons = list(
          list(extend = "csv"),
          list(extend = "excel"),
          list(extend = "pdf"),
          list(extend = "print")
        ),
        columnDefs = list(
          list(className = "dt-center", targets = 1:4)
        )
      ),
      extensions = "Buttons",
      rownames = FALSE,
      colnames = c("Test", "Chi-square(χ²)", "df", "p-value","Significant"
      ),
      caption = tags$caption(
        style = "caption-side:bottom;text-align:left;",
        tags$span(
          style = "color:black;font-weight:bold;font-style:italic;",
          paste0(
            "Footnote: df = Degrees of freedom. ",
            "The Kruskal-Wallis test detects differences across independent monthly groups, while the 
            Friedman test accounts for the repeated measures structure ",
            "(12 months across ", length(unique(filtered$year)), " years), 
            making it more appropriate for this data."
          )
        )
      )
    )
    
  })
  
  # Map View reset
  observeEvent(input$reset_map, {
    updateSelectInput(session, "map_year", selected = max(data$year, na.rm = TRUE))
    updateSelectInput(session, "map_month", selected = "All")
  })
  
  # Compare Regions reset
  observeEvent(input$reset_compare, {
    updateSelectInput(session, "compare_region1", selected = "Upper East")
    updateSelectInput(session, "compare_region2", selected = "Upper West")
    updateSliderInput(session, "compare_year_range",
                      value = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)))
  })
  
  map_summary_data <- reactive({
    req(input$map_year)
    df <- data %>% filter(year == input$map_year)
    if (!is.null(input$map_month) && input$map_month != "All")
      df <- df %>% filter(month == input$map_month)
    
    pop <- get_population_series(df)
    df$incidence <- ifelse(!is.na(pop) & pop > 0, df$uncom / pop * 1000, NA_real_)
    
    df %>%
      group_by(region) %>%
      summarise(total_cases = sum(uncom, na.rm = TRUE),
                avg_incidence = mean(incidence, na.rm = TRUE),
                avg_rain = mean(rainfall, na.rm = TRUE),
                avg_temp = mean(avgtemp, na.rm = TRUE),
                .groups = "drop") %>%
      left_join(data %>% distinct(region, lng, lat), by = "region")
  })
  
  draw_gam_map_markers <- function(proxy_or_map, df) {
    pal <- colorNumeric("YlOrRd", domain = df$avg_incidence)
    proxy_or_map %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~ifelse(is.na(avg_incidence), 6,
                         scales::rescale(avg_incidence, to = c(8, 28),
                                         from = range(df$avg_incidence, na.rm = TRUE))),
        fillColor = ~pal(avg_incidence),
        color = "#333", weight = 1, fillOpacity = 0.75,
        popup = ~paste0(
          "<b>", region, "</b><br>",
          "Incidence: <b>", round(avg_incidence, 2), "</b> per 1,000 pop<br>",
          "Total Cases: ", format(round(total_cases / 1000, 1), big.mark = ","), "k<br>",
          "Avg Rainfall: ", round(avg_rain, 1), " mm<br>",
          "Avg Temp: ", round(avg_temp, 1), " \u00b0C"
        ),
        label = ~region
      ) %>%
      addLegend("bottomright", pal = pal, values = df$avg_incidence,
                title = "Incidence (per 1,000)", labFormat = labelFormat(suffix = ""))
  }
  
  output$ghana_map <- renderLeaflet({
    df <- map_summary_data()
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -1.2, lat = 7.9, zoom = 6) %>%
      draw_gam_map_markers(df)
  })
  
  observeEvent(list(input$map_year, input$map_month), {
    df <- map_summary_data()
    leafletProxy("ghana_map", data = df) %>%
      clearMarkers() %>% clearControls() %>%
      draw_gam_map_markers(df)
  })
  
  output$map_summary_table <- DT::renderDataTable({
    df <- map_summary_data() %>%
      dplyr::select(region, avg_incidence, total_cases, avg_rain, avg_temp) %>%
      mutate(avg_incidence = round(avg_incidence, 2),
             total_cases = round(total_cases / 1000, 1),
             avg_rain = round(avg_rain, 1),
             avg_temp = round(avg_temp, 1))
    DT::datatable(df,
                  options = list(pageLength = 10, dom = "Bfrtip",
                                 buttons = list(list(extend = "csv"), list(extend = "excel"),
                                                list(extend = "pdf"), list(extend = "print")),
                                 columnDefs = list(list(className = "dt-center", targets = 1:4))),
                  extensions = "Buttons", rownames = FALSE,
                  colnames = c("Region", "Incidence (per 1,000)", "Cases (x10\u00b3)",
                               "Avg Rainfall (mm)", "Avg Temp (\u00b0C)"))
  })
  
  output$ghana_maps <- renderLeaflet({
    leaflet(gh_shape_ll) %>%
      addTiles() %>%
      addPolygons(fillColor = "lightgray", color = "black", label = ~REGION)
  })
  
  compare_data <- reactive({
    req(input$compare_region1, input$compare_region2, input$compare_year_range)
    df <- data %>%
      filter(region %in% c(input$compare_region1, input$compare_region2),
             year >= input$compare_year_range[1],
             year <= input$compare_year_range[2])
    pop <- get_population_series(df)
    df$incidence <- ifelse(!is.na(pop) & pop > 0, df$uncom / pop * 1000, NA_real_)
    df
  })
  
  output$compare_ts_plot <- renderPlotly({
    df <- compare_data()
    p <- ggplot(df, aes(x = date, y = incidence, color = region)) +
      geom_line(linewidth = 1) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
      labs(x = "", y = "Incidence (per 1,000 pop)", color = "Region") +
      theme_ts_decompose()
    make_static_plotly(p, filename = "compare_ts")
  })
  
  output$compare_bar_plot <- renderPlotly({
    df <- compare_data() %>%
      group_by(region, year) %>%
      summarise(annual_incidence = mean(incidence, na.rm = TRUE), .groups = "drop")
    p <- ggplot(df, aes(x = factor(year), y = annual_incidence, fill = region)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "", y = "Annual mean incidence (per 1,000 pop)", fill = "Region") +
      theme_seasonality() + theme(axis.text.x = element_text(angle = 0, hjust = 1))
    make_static_plotly(p, filename = "compare_bar")
  })
  
  output$compare_summary_table <- DT::renderDataTable({
    df <- compare_data() %>%
      group_by(region, year) %>%
      summarise(Total_Cases = round(sum(uncom, na.rm = TRUE) / 1000, 1),
                Mean_Incidence = round(mean(incidence, na.rm = TRUE), 2),
                Peak_Month = month.abb[which.max(tapply(incidence, month, mean, na.rm = TRUE))],
                .groups = "drop")
    DT::datatable(df,
                  options = list(pageLength = 10, dom = "Bfrtip",
                                 buttons = list(list(extend = "csv"), list(extend = "excel"),
                                                list(extend = "pdf"), list(extend = "print")),
                                 columnDefs = list(list(className = "dt-center", targets = 1:4))),
                  extensions = "Buttons", rownames = FALSE,
                  colnames = c("Region", "Year", "Total Cases (x10\u00b3)",
                               "Mean Incidence (per 1,000)", "Peak Month"))
  })
  
  
  output$regional_summary_ui <- renderUI({
    region     <- input$summary_region
    bmd        <- get_models_and_best_idx(region)
    best_model <- bmd$models[[bmd$best_idx]]
    req(!is.null(best_model))
    bm_summary <- summary(best_model)
    best_idx   <- bmd$best_idx
    
    adj_r2     <- round(bm_summary$r.sq * 100, 1)
    dev_expl   <- round(bm_summary$dev.expl * 100, 1)
    reml_score <- round(best_model$gcv.ubre, 2)
    
    model_family <- tryCatch(best_model$family$family, error = function(e) "Negative Binomial")
    model_method <- tryCatch(best_model$method, error = function(e) "REML")
    
    smooth_names <- rownames(bm_summary$s.table)
    has_time   <- any(grepl("s\\(time\\)", smooth_names))
    has_months <- any(grepl("s\\(months\\)", smooth_names))
    has_rain   <- any(grepl("s\\(rainfall\\)", smooth_names))
    has_temp   <- any(grepl("s\\(avgtemp\\)", smooth_names))
    has_ti_tm  <- any(grepl("ti\\(time,months\\)", smooth_names))
    has_ti_tr  <- any(grepl("ti\\(avgtemp,rainfall\\)", smooth_names))
    
    sig_terms <- smooth_names[bm_summary$s.table[, "p-value"] < 0.05]
    
    region_data <- data[data$region == region, ]
    pop     <- get_population_series(region_data)
    has_pop <- !all(is.na(pop))
    incidence <- if (has_pop) (region_data$uncom / pop) * 1000 else rep(NA_real_, nrow(region_data))
    
    inc_df <- data.frame(date = region_data$date, year = region_data$year, month = region_data$month,
                         uncom = region_data$uncom, incidence = incidence)
    
    total_cases    <- round(sum(region_data$uncom, na.rm = TRUE) / 1e3, 1)
    mean_incidence <- if (has_pop) round(mean(incidence, na.rm = TRUE), 2) else NA
    latest_pop     <- if (has_pop) round(tail(pop[!is.na(pop)], 1)) else NA
    
    mean_rain <- round(mean(region_data$rainfall, na.rm = TRUE), 1)
    mean_temp <- round(mean(region_data$avgtemp, na.rm = TRUE), 1)
    year_range <- paste0(min(region_data$year, na.rm = TRUE), "\u2013", max(region_data$year, na.rm = TRUE))
    
    month_lookup <- c(Jan = "January", Feb = "February", Mar = "March", Apr = "April", May = "May",
                      Jun = "June", Jul = "July", Aug = "August", Sep = "September",
                      Oct = "October", Nov = "November", Dec = "December")
    
    seas <- inc_df %>% dplyr::group_by(month) %>%
      dplyr::summarise(avg_incidence = mean(incidence, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(avg_incidence))
    peak_month <- if (has_pop) month_lookup[as.character(seas$month[1])] else "\u2014"
    low_month  <- if (has_pop) month_lookup[as.character(seas$month[nrow(seas)])] else "\u2014"
    
    kw_sig <- NA; kw_p_label <- "N/A"; fr_sig <- NA; fr_p_label <- "N/A"
    if (has_pop) {
      kw <- tryCatch(kruskal.test(incidence ~ month, data = inc_df), error = function(e) NULL)
      if (!is.null(kw)) {
        kw_sig <- kw$p.value < 0.05
        kw_p_label <- if (kw$p.value < 0.001) "< 0.001" else sprintf("%.3f", kw$p.value)
      }
      wide_inc <- tidyr::pivot_wider(inc_df, id_cols = year, names_from = month,
                                     values_from = incidence, values_fn = mean)
      mat_inc <- as.matrix(wide_inc[, -1])
      if (!any(is.na(mat_inc)) && nrow(mat_inc) >= 2) {
        fr <- tryCatch(friedman.test(mat_inc), error = function(e) NULL)
        if (!is.null(fr)) {
          fr_sig <- fr$p.value < 0.05
          fr_p_label <- if (fr$p.value < 0.001) "< 0.001" else sprintf("%.3f", fr$p.value)
        }
      }
    }
    
    annual_inc <- inc_df %>% dplyr::group_by(year) %>%
      dplyr::summarise(annual_incidence = mean(incidence, na.rm = TRUE), .groups = "drop")
    n_yrs <- nrow(annual_inc)
    third <- max(1, floor(n_yrs / 3))
    first_avg <- mean(annual_inc$annual_incidence[1:third], na.rm = TRUE)
    last_avg  <- mean(annual_inc$annual_incidence[(n_yrs - third + 1):n_yrs], na.rm = TRUE)
    first_range <- paste0(min(annual_inc$year[1:third]), "\u2013", max(annual_inc$year[1:third]))
    last_range  <- paste0(min(annual_inc$year[(n_yrs - third + 1):n_yrs]), "\u2013",
                          max(annual_inc$year[(n_yrs - third + 1):n_yrs]))
    trend_pct <- if (has_pop) round((last_avg - first_avg) / first_avg * 100, 1) else NA
    trend_dir <- if (is.na(trend_pct)) "indeterminate"
    else if (trend_pct > 5) "increasing"
    else if (trend_pct < -5) "decreasing"
    else "relatively stable"
    trend_chip_class <- if (is.na(trend_pct)) "trend-chip trend-chip-stable"
    else if (trend_pct > 5) "trend-chip trend-chip-up"
    else if (trend_pct < -5) "trend-chip trend-chip-down"
    else "trend-chip trend-chip-stable"
    trend_icon_fa <- if (is.na(trend_pct)) "fa-minus"
    else if (trend_pct > 5) "fa-arrow-trend-up"
    else if (trend_pct < -5) "fa-arrow-trend-down"
    else "fa-minus"
    narrative_trend_class <- if (is.na(trend_pct)) "narrative-trend-stable"
    else if (trend_pct > 5) "narrative-trend-up"
    else if (trend_pct < -5) "narrative-trend-down"
    else "narrative-trend-stable"
    
    annual_raw <- region_data %>% dplyr::group_by(year) %>%
      dplyr::summarise(annual_cases = sum(uncom, na.rm = TRUE), .groups = "drop")
    raw_first <- mean(annual_raw$annual_cases[1:third], na.rm = TRUE)
    raw_last  <- mean(annual_raw$annual_cases[(n_yrs - third + 1):n_yrs], na.rm = TRUE)
    raw_trend_pct <- round((raw_last - raw_first) / raw_first * 100, 1)
    raw_trend_dir <- if (raw_trend_pct > 5) "increasing" else if (raw_trend_pct < -5) "decreasing" else "relatively stable"
    
    divergence_note <- if (has_pop && !is.na(trend_pct) && trend_dir != raw_trend_dir) {
      div(class = "threshold-alert alert-normal",
          tags$i(class = "fa fa-circle-info"),
          sprintf(" Raw case counts are %s (%s%%) while population-adjusted incidence is %s (%s%%) \u2014 the gap reflects population change over the study period, not disease risk.",
                  raw_trend_dir, abs(raw_trend_pct), trend_dir, abs(trend_pct)))
    } else NULL
    
    reg_annual_inc_mean <- mean(annual_inc$annual_incidence, na.rm = TRUE)
    last_yr_incidence   <- annual_inc$annual_incidence[annual_inc$year == max(annual_inc$year, na.rm = TRUE)]
    threshold_ui <- if (!has_pop || length(last_yr_incidence) == 0 || is.na(reg_annual_inc_mean) || reg_annual_inc_mean == 0) {
      div(class = "threshold-alert alert-normal", tags$i(class = "fa fa-circle-info"),
          " Insufficient population data to assess latest-year incidence against the historical average.")
    } else if (last_yr_incidence > reg_annual_inc_mean * 1.2) {
      div(class = "threshold-alert alert-high",
          tags$i(class = "fa fa-triangle-exclamation"),
          sprintf(" Latest year incidence is %.0f%% above the historical annual average incidence.",
                  (last_yr_incidence / reg_annual_inc_mean - 1) * 100))
    } else if (last_yr_incidence < reg_annual_inc_mean * 0.8) {
      div(class = "threshold-alert alert-low",
          tags$i(class = "fa fa-circle-check"),
          sprintf(" Latest year incidence is %.0f%% below the historical annual average incidence.",
                  (1 - last_yr_incidence / reg_annual_inc_mean) * 100))
    } else {
      div(class = "threshold-alert alert-normal", tags$i(class = "fa fa-circle-info"),
          " Latest year incidence is within the normal range.")
    }
    
    term_labels <- c(
      "s(time)" = "long-term time trend", "s(months)" = "seasonality",
      "s(rainfall)" = "rainfall", "s(avgtemp)" = "average temperature",
      "ti(time,months)" = "time\u2013season interaction",
      "ti(avgtemp,rainfall)" = "temperature\u2013rainfall interaction"
    )
    sig_readable <- term_labels[sig_terms]
    sig_readable <- sig_readable[!is.na(sig_readable)]
    sig_sentence <- if (length(sig_readable) > 0)
      paste0("The following predictors were statistically significant (p\u00a0<\u00a00.05): ",
             paste(sig_readable, collapse = ", "), ".")
    else "No smooth terms reached statistical significance at the 0.05 level."
    
    intercept_val <- if (has_pop && !is.na(latest_pop)) {
      round(exp(coef(best_model)[["(Intercept)"]]) * latest_pop / 1e3, 2)
    } else {
      round(exp(coef(best_model)[["(Intercept)"]]), 4)
    }
    baseline_label <- if (has_pop && !is.na(latest_pop)) "k cases/month (at latest population)" else " cases per capita (rate only \u2014 no population found)"
    
    dl_id <- paste0("dl_summary_", gsub(" ", "_", region))
    
    tagList(
      if (!has_pop)
        div(class = "threshold-alert alert-high",
            tags$i(class = "fa fa-triangle-exclamation"),
            " No population column found for this region \u2014 incidence could not be computed; figures below show \u2014."),
      threshold_ui,
      divergence_note,
      tags$br(),
      fluidRow(
        column(3, div(class = "summary-kpi-card",
                      div(class = "kpi-icon kpi-icon-purple", tags$i(class = "fa fa-bug")),
                      div(class = "kpi-value", paste0(total_cases, "k")),
                      div(class = "kpi-label", "Total observed cases"),
                      div(class = "kpi-sub", year_range))),
        column(3, div(class = "summary-kpi-card",
                      div(class = "kpi-icon kpi-icon-teal", tags$i(class = "fa fa-chart-line")),
                      div(class = "kpi-value", if (has_pop) mean_incidence else "\u2014"),
                      div(class = "kpi-label", "Avg monthly incidence"),
                      div(class = "kpi-sub", "per 1,000 population"))),
        column(3, div(class = "summary-kpi-card",
                      div(class = "kpi-icon kpi-icon-blue", tags$i(class = "fa fa-cloud-rain")),
                      div(class = "kpi-value", paste0(mean_rain, " mm")),
                      div(class = "kpi-label", "Avg monthly rainfall"),
                      div(class = "kpi-sub", "Across all months"))),
        column(3, div(class = "summary-kpi-card",
                      div(class = "kpi-icon kpi-icon-amber", tags$i(class = "fa fa-temperature-half")),
                      div(class = "kpi-value", paste0(mean_temp, " \u00b0C")),
                      div(class = "kpi-label", "Avg temperature"),
                      div(class = "kpi-sub", "Monthly average")))
      ),
      tags$br(),
      fluidRow(
        column(6,
               div(class = "summary-section-card",
                   h4(tags$i(class = "fa fa-chart-line"), "Incidence Trend"), tags$hr(),
                   div(class = trend_chip_class,
                       tags$i(class = paste("fa", trend_icon_fa)),
                       if (is.na(trend_pct)) "Insufficient data" else paste0(abs(trend_pct), "% ", trend_dir)),
                   p(style = "font-size:14px;color:#555;line-height:1.65;text-align:justify;",
                     if (has_pop) paste0(
                       "Comparing first third (", first_range,
                       ") and last third (", last_range,
                       ") of study years, population-adjusted incidence in ", region,
                       " has been ", trend_dir, "."
                     ) else "Incidence trend unavailable \u2014 no population data."))),
        column(6,
               div(class = "summary-section-card",
                   h4(tags$i(class = "fa fa-calendar"), "Seasonal Pattern (Incidence-Based)"), tags$hr(),
                   div(class = "summary-info-row", span(class = "summary-info-key", "Peak incidence month"),  span(class = "summary-info-val", peak_month)),
                   div(class = "summary-info-row", span(class = "summary-info-key", "Lowest incidence month"),   span(class = "summary-info-val", low_month)),
                   div(class = "summary-info-row", span(class = "summary-info-key", "Kruskal-Wallis p"),
                       span(class = "summary-info-val", kw_p_label, " ",
                            if (is.na(kw_sig)) "" else if (kw_sig) tags$span(class = "summary-tick-yes", "\u2713") else tags$span(class = "summary-tick-no", "\u2717"))),
                   div(class = "summary-info-row", span(class = "summary-info-key", "Friedman p"),
                       span(class = "summary-info-val", fr_p_label, " ",
                            if (is.na(fr_sig)) "" else if (fr_sig) tags$span(class = "summary-tick-yes", "\u2713") else tags$span(class = "summary-tick-no", "\u2717")))))
      ),
      tags$br(),
      div(class = "summary-section-card",
          div(style = "display:flex;align-items:center;margin-bottom:2px;",
              h4(tags$i(class = "fa fa-calculator"), paste0("Best GAM \u2014 Model ", best_idx)),
              div(class = "gam-model-num-badge", paste0("M", best_idx))),
          tags$hr(),
          div(class = "gam-stat-trio",
              div(class = "gam-stat-pill", div(class = "gam-stat-val", paste0(adj_r2, "%")), div(class = "gam-stat-lbl", "Adj.\u00a0R\u00b2")),
              div(class = "gam-stat-pill", div(class = "gam-stat-val", paste0(dev_expl, "%")), div(class = "gam-stat-lbl", "Deviance explained")),
              div(class = "gam-stat-pill", div(class = "gam-stat-val", reml_score), div(class = "gam-stat-lbl", "REML score"))),
          div(class = "gam-model-grid",
              div(div(class = "gam-section-label", "Smooth terms included"),
                  div(style = "display:flex;flex-wrap:wrap;gap:6px;margin-bottom:10px;",
                      if (has_time)   tags$span(class = "summary-badge badge-blue",  "s(time)"),
                      if (has_months) tags$span(class = "summary-badge badge-teal",  "s(months)"),
                      if (has_rain)   tags$span(class = "summary-badge badge-blue",  "s(rainfall)"),
                      if (has_temp)   tags$span(class = "summary-badge badge-amber", "s(avgtemp)"),
                      if (has_ti_tm)  tags$span(class = "summary-badge badge-purple", "ti(time, months)"),
                      if (has_ti_tr)  tags$span(class = "summary-badge badge-coral", "ti(avgtemp, rainfall)"))),
              div(div(class = "gam-section-label", "Model details"),
                  div(class = "summary-info-row", span(class = "summary-info-key", "Baseline intercept"),
                      span(class = "summary-info-val", paste0(intercept_val, baseline_label))),
                  div(class = "summary-info-row", span(class = "summary-info-key", "Family"),             span(class = "summary-info-val", model_family)),
                  div(class = "summary-info-row", span(class = "summary-info-key", "Estimation method"),  span(class = "summary-info-val", model_method)))),
          tags$hr(),
          div(class = "gam-section-label", "Significant predictors (p < 0.05)"),
          p(style = "font-size:13px;color:#555;line-height:1.6;", sig_sentence)),
      tags$br(),
      div(class = "summary-narrative-card",
          div(style = "display:flex;justify-content:space-between;align-items:center;",
              h4(tags$i(class = "fa fa-file-lines"), "Summary Narrative"),
              downloadButton(dl_id, "Download Narrative", class = "btn btn-warning btn-sm")),
          tags$hr(),
          p(class = "summary-narrative-text",
            "For the ", tags$span(class = "narrative-highlight", region), " region (",
            tags$span(class = "narrative-highlight", year_range), "), the GAM (population-offset corrected) estimates an average incidence of ",
            if (has_pop) tags$span(class = "narrative-highlight", paste0(mean_incidence, " cases per 1,000 population per month")) else "an indeterminate rate (no population data)",
            ". A total of ", tags$span(class = "narrative-highlight", paste0(total_cases, "k")),
            " raw uncomplicated malaria cases were recorded over the period. Incidence has been ",
            tags$span(class = narrative_trend_class, trend_dir),
            if (!is.na(trend_pct)) tagList(" over the study period (", tags$span(class = "narrative-highlight", paste0(abs(trend_pct), "% change")), ")") else "",
            if (has_pop) tagList(", peaking in ", tags$span(class = "narrative-highlight", peak_month), " and lowest in ", tags$span(class = "narrative-highlight", low_month)) else "",
            ". ",
            if (!is.na(kw_sig)) tagList("The Kruskal-Wallis test confirms that monthly incidence differences are ",
                                        if (kw_sig) tagList(tags$span(class = "summary-tick-yes", "statistically significant"), paste0(" (p\u00a0=\u00a0", kw_p_label, "). "))
                                        else        tagList(tags$span(class = "summary-tick-no", "not statistically significant"), paste0(" (p\u00a0=\u00a0", kw_p_label, "). "))) else "",
            if (!is.na(fr_sig)) tagList("The Friedman test ",
                                        if (fr_sig) tagList(tags$span(class = "summary-tick-yes", "supports this finding"), paste0(" (p\u00a0=\u00a0", fr_p_label, "). "))
                                        else        tagList(tags$span(class = "summary-tick-no", "does not reach significance"), paste0(" (p\u00a0=\u00a0", fr_p_label, "). "))) else "",
            "Mean monthly rainfall is ", tags$span(class = "narrative-highlight", paste0(mean_rain, "\u00a0mm")),
            " and mean average temperature is ", tags$span(class = "narrative-highlight", paste0(mean_temp, "\u00a0\u00b0C")),
            ". The best-fitting GAM (Model\u00a0", best_idx, ") explains ",
            tags$span(class = "narrative-highlight", paste0(dev_expl, "%")),
            " of the deviance (Adj.\u00a0R\u00b2\u00a0=\u00a0",
            tags$span(class = "narrative-highlight", paste0(adj_r2, "%")), "). ", sig_sentence,
            " This is directly comparable to the GAMM Regional Summary, since both models share the same population offset."))
    )
  })
  
  observe({
    region <- req(input$summary_region)
    dl_id  <- paste0("dl_summary_", gsub(" ", "_", region))
    output[[dl_id]] <- downloadHandler(
      filename = function() paste0("summary_", gsub(" ", "_", region), "_", Sys.Date(), ".txt"),
      content  = function(file) {
        rd       <- data[data$region == region, ]
        total    <- round(sum(rd$uncom, na.rm = TRUE) / 1000, 1)
        pop      <- get_population_series(rd)
        has_pop  <- !all(is.na(pop))
        inc      <- if (has_pop) (rd$uncom / pop) * 1000 else NA_real_
        mean_inc <- if (has_pop) round(mean(inc, na.rm = TRUE), 2) else NA
        yr_range <- paste0(min(rd$year, na.rm = TRUE), "-", max(rd$year, na.rm = TRUE))
        writeLines(paste0(
          "GAM-MAP Regional Summary \u2014 ", region, "\n",
          "Date: ", Sys.Date(), "\n\n",
          "Region: ", region, "\n",
          "Period: ", yr_range, "\n",
          "Total cases: ", total, "k\n",
          "Avg monthly incidence: ", if (has_pop) mean_inc else "N/A (no population data)", " cases/1,000 pop\n"
        ), file)
      }
    )
  })
  
  
  output$gam_autocorr_banner <- renderUI({
    req(input$diag_region4)
    status <- gam_autocorrelation_status(input$diag_region4)
    req(!is.null(status))
    
    if (length(status$significant_lags) > 0) {
      div(class = "threshold-alert alert-high",
          tags$i(class = "fa fa-triangle-exclamation"),
          sprintf(
            " Residual autocorrelation remains in the %s GAM at lag(s) %s (Ljung-Box p < 0.05), so it should not be used standalone for inference or forecasting in this region",
            input$diag_region4, paste(status$significant_lags, collapse = ", ")
          ),
          if (status$has_gamm)
            tagList(
              " \u2014 refer instead to the ",
              actionLink("go_gdiag_from_gam_diag", "GAMM results",
                         onclick = "Shiny.setInputValue('tabs','gamm_diagnostics',{priority:'event'})"),
              ", which fit a corARMA correlation structure to correct for this."
            )
          else
            "; no corresponding GAMM was found for this region \u2014 check that model_*_gamm.rds was generated.")
    } else {
      div(class = "threshold-alert alert-normal", tags$i(class = "fa fa-circle-check"),
          sprintf(" No significant residual autocorrelation detected in the %s GAM at any tested lag (12\u201360 months) \u2014 safe to use standalone for this region.",
                  input$diag_region4))
    }
  })
  
  
  output$gam_autocorr_banner_summary <- renderUI({
    req(input$summary_region)
    status <- gam_autocorrelation_status(input$summary_region)
    req(!is.null(status))
    
    if (length(status$significant_lags) > 0) {
      div(class = "threshold-alert alert-high",
          tags$i(class = "fa fa-triangle-exclamation"),
          sprintf(
            " This region's standalone GAM shows significant residual autocorrelation (lags %s) and should not be used for inference \u2014 refer to the ",
            paste(status$significant_lags, collapse = ", ")
          ),
          if (status$has_gamm)
            actionLink("go_gamm_summary_from_gam", "GAMM Regional Summary",
                       onclick = "Shiny.setInputValue('tabs','gamm_regional_summary',{priority:'event'})")
          else
            "GAMM (not yet available for this region)",
          " for the corrected model.")
    } else {
      NULL
    }
  })
  
  
  output$gamm_autocorr_banner <- renderUI({
    req(input$gamm_diag_region4)
    status <- gamm_autocorrelation_status(input$gamm_diag_region4)
    req(!is.null(status))
    
    gam_had_issue <- !is.null(status$gam_status) && length(status$gam_status$significant_lags) > 0
    
    if (length(status$significant_lags) > 0) {
      div(class = "threshold-alert alert-high",
          tags$i(class = "fa fa-triangle-exclamation"),
          sprintf(
            " Residual autocorrelation STILL remains in the %s GAMM (%s) at lag(s) %s, even after correlation correction. Results for this region should be interpreted with caution regardless of model choice.",
            input$gamm_diag_region4, status$structure_label,
            paste(status$significant_lags, collapse = ", ")
          ))
    } else {
      div(class = "threshold-alert alert-normal", tags$i(class = "fa fa-circle-check"),
          sprintf(" No significant residual autocorrelation remains in the %s GAMM (%s) at any tested lag.",
                  input$gamm_diag_region4, status$structure_label),
          if (gam_had_issue)
            sprintf(" This confirms the corARMA correction successfully resolved the autocorrelation present in the standalone GAM (lags %s) \u2014 use this GAMM as the primary model for %s.",
                    paste(status$gam_status$significant_lags, collapse = ", "), input$gamm_diag_region4)
          else "")
    }
  })
  
  output$gamm_autocorr_banner_summary <- renderUI({
    req(input$gamm_summary_region)
    status <- gamm_autocorrelation_status(input$gamm_summary_region)
    req(!is.null(status))
    gam_had_issue <- !is.null(status$gam_status) && length(status$gam_status$significant_lags) > 0
    
    if (gam_had_issue && length(status$significant_lags) == 0) {
      div(class = "threshold-alert alert-normal",
          tags$i(class = "fa fa-circle-check"),
          sprintf(" This region's standalone GAM failed the residual-autocorrelation test; this GAMM (%s) corrects for it and is the recommended model for %s.",
                  status$structure_label, input$gamm_summary_region))
    } else if (length(status$significant_lags) > 0) {
      div(class = "threshold-alert alert-high",
          tags$i(class = "fa fa-triangle-exclamation"),
          sprintf(" Residual autocorrelation remains even in this GAMM (%s) at lag(s) %s \u2014 interpret %s results with caution.",
                  status$structure_label, paste(status$significant_lags, collapse = ", "), input$gamm_summary_region))
    } else NULL
  })
  
  
  output$gamm_regional_summary_ui <- renderUI({
    req(input$gamm_summary_region)
    region <- input$gamm_summary_region
    bg   <- get_gamm_model(region)
    meta <- get_gamm_meta(region)
    req(!is.null(bg), !is.null(meta))
    
    rd  <- data[data$region == region, ]
    rd  <- rd[order(rd$time), ]
    pop <- get_population_series(rd)
    has_pop <- !all(is.na(pop))
    
    fitted_counts <- fitted(bg$gam)
    incidence     <- if (has_pop) (fitted_counts / pop) * 1000 else rep(NA_real_, length(fitted_counts))
    
    inc_df <- data.frame(date = rd$date, year = rd$year, month = rd$month,
                         observed = rd$uncom, fitted = fitted_counts,
                         population = pop, incidence = incidence)
    
    total_cases_obs <- round(sum(inc_df$observed, na.rm = TRUE) / 1e3, 1)
    mean_rain <- round(mean(rd$rainfall, na.rm = TRUE), 1)
    mean_temp <- round(mean(rd$avgtemp, na.rm = TRUE), 1)
    year_range <- paste0(min(rd$year, na.rm = TRUE), "\u2013", max(rd$year, na.rm = TRUE))
    
    mean_incidence <- if (has_pop) round(mean(inc_df$incidence, na.rm = TRUE), 2) else NA
    latest_pop     <- if (has_pop) round(tail(pop[!is.na(pop)], 1)) else NA
    
    month_lookup <- c(Jan = "January", Feb = "February", Mar = "March", Apr = "April", May = "May",
                      Jun = "June", Jul = "July", Aug = "August", Sep = "September",
                      Oct = "October", Nov = "November", Dec = "December")
    
    seas <- inc_df %>% dplyr::group_by(month) %>%
      dplyr::summarise(avg_incidence = mean(incidence, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(avg_incidence))
    peak_month <- if (has_pop) month_lookup[as.character(seas$month[1])] else "\u2014"
    low_month  <- if (has_pop) month_lookup[as.character(seas$month[nrow(seas)])] else "\u2014"
    
    kw_sig <- NA; kw_p_label <- "N/A"; fr_sig <- NA; fr_p_label <- "N/A"
    if (has_pop) {
      kw <- tryCatch(kruskal.test(incidence ~ month, data = inc_df), error = function(e) NULL)
      if (!is.null(kw)) {
        kw_sig <- kw$p.value < 0.05
        kw_p_label <- if (kw$p.value < 0.001) "< 0.001" else sprintf("%.3f", kw$p.value)
      }
      wide_inc <- tidyr::pivot_wider(inc_df, id_cols = year, names_from = month,
                                     values_from = incidence, values_fn = mean)
      mat_inc  <- as.matrix(wide_inc[, -1])
      if (!any(is.na(mat_inc)) && nrow(mat_inc) >= 2) {
        fr <- tryCatch(friedman.test(mat_inc), error = function(e) NULL)
        if (!is.null(fr)) {
          fr_sig <- fr$p.value < 0.05
          fr_p_label <- if (fr$p.value < 0.001) "< 0.001" else sprintf("%.3f", fr$p.value)
        }
      }
    }
    
    annual_inc <- inc_df %>% dplyr::group_by(year) %>%
      dplyr::summarise(annual_incidence = mean(incidence, na.rm = TRUE), .groups = "drop")
    n_yrs <- nrow(annual_inc)
    third <- max(1, floor(n_yrs / 3))
    first_avg <- mean(annual_inc$annual_incidence[1:third], na.rm = TRUE)
    last_avg  <- mean(annual_inc$annual_incidence[(n_yrs - third + 1):n_yrs], na.rm = TRUE)
    first_range <- paste0(min(annual_inc$year[1:third]), "\u2013", max(annual_inc$year[1:third]))
    last_range  <- paste0(min(annual_inc$year[(n_yrs - third + 1):n_yrs]), "\u2013",
                          max(annual_inc$year[(n_yrs - third + 1):n_yrs]))
    trend_pct <- if (has_pop) round((last_avg - first_avg) / first_avg * 100, 1) else NA
    trend_dir <- if (is.na(trend_pct)) "indeterminate"
    else if (trend_pct > 5) "increasing"
    else if (trend_pct < -5) "decreasing"
    else "relatively stable"
    trend_chip_class <- if (is.na(trend_pct)) "trend-chip trend-chip-stable"
    else if (trend_pct > 5) "trend-chip trend-chip-up"
    else if (trend_pct < -5) "trend-chip trend-chip-down"
    else "trend-chip trend-chip-stable"
    trend_icon_fa <- if (is.na(trend_pct)) "fa-minus"
    else if (trend_pct > 5) "fa-arrow-trend-up"
    else if (trend_pct < -5) "fa-arrow-trend-down"
    else "fa-minus"
    narrative_trend_class <- if (is.na(trend_pct)) "narrative-trend-stable"
    else if (trend_pct > 5) "narrative-trend-up"
    else if (trend_pct < -5) "narrative-trend-down"
    else "narrative-trend-stable"
    
    annual_raw <- rd %>% dplyr::group_by(year) %>%
      dplyr::summarise(annual_cases = sum(uncom, na.rm = TRUE), .groups = "drop")
    raw_first <- mean(annual_raw$annual_cases[1:third], na.rm = TRUE)
    raw_last  <- mean(annual_raw$annual_cases[(n_yrs - third + 1):n_yrs], na.rm = TRUE)
    raw_trend_pct <- round((raw_last - raw_first) / raw_first * 100, 1)
    raw_trend_dir <- if (raw_trend_pct > 5) "increasing" else if (raw_trend_pct < -5) "decreasing" else "relatively stable"
    
    divergence_note <- if (has_pop && !is.na(trend_pct) && trend_dir != raw_trend_dir) {
      div(class = "threshold-alert alert-normal",
          tags$i(class = "fa fa-circle-info"),
          sprintf(" Raw case counts are %s (%s%%) while population-adjusted incidence is %s (%s%%) \u2014 the gap reflects population change over the study period, not disease risk.",
                  raw_trend_dir, abs(raw_trend_pct), trend_dir, abs(trend_pct)))
    } else NULL
    
    reg_annual_inc_mean <- mean(annual_inc$annual_incidence, na.rm = TRUE)
    last_yr_incidence   <- annual_inc$annual_incidence[annual_inc$year == max(annual_inc$year, na.rm = TRUE)]
    threshold_ui <- if (!has_pop || length(last_yr_incidence) == 0 || is.na(reg_annual_inc_mean) || reg_annual_inc_mean == 0) {
      div(class = "threshold-alert alert-normal", tags$i(class = "fa fa-circle-info"),
          " Insufficient population data to assess latest-year incidence against the historical average.")
    } else if (last_yr_incidence > reg_annual_inc_mean * 1.2) {
      div(class = "threshold-alert alert-high",
          tags$i(class = "fa fa-triangle-exclamation"),
          sprintf(" Latest year incidence is %.0f%% above the historical annual average incidence.",
                  (last_yr_incidence / reg_annual_inc_mean - 1) * 100))
    } else if (last_yr_incidence < reg_annual_inc_mean * 0.8) {
      div(class = "threshold-alert alert-low",
          tags$i(class = "fa fa-circle-check"),
          sprintf(" Latest year incidence is %.0f%% below the historical annual average incidence.",
                  (1 - last_yr_incidence / reg_annual_inc_mean) * 100))
    } else {
      div(class = "threshold-alert alert-normal", tags$i(class = "fa fa-circle-info"),
          " Latest year incidence is within the normal range.")
    }
    
    gam_summary <- summary(bg$gam)
    adj_r2   <- round(gam_summary$r.sq * 100, 1)
    dev_expl <- round(gam_summary$dev.expl * 100, 1)
    aic_lme  <- tryCatch(round(AIC(bg$lme), 1), error = function(e) NA)
    bic_lme  <- tryCatch(round(BIC(bg$lme), 1), error = function(e) NA)
    
    smooth_names <- rownames(gam_summary$s.table)
    has_time   <- any(grepl("s\\(time\\)", smooth_names))
    has_months <- any(grepl("s\\(months\\)", smooth_names))
    has_rain   <- any(grepl("s\\(rainfall\\)", smooth_names))
    has_temp   <- any(grepl("s\\(avgtemp\\)", smooth_names))
    has_ti_tm  <- any(grepl("ti\\(time,months\\)", smooth_names))
    has_ti_tr  <- any(grepl("ti\\(avgtemp,rainfall\\)", smooth_names))
    
    sig_terms <- smooth_names[gam_summary$s.table[, "p-value"] < 0.05]
    term_labels <- c(
      "s(time)" = "long-term time trend", "s(months)" = "seasonality",
      "s(rainfall)" = "rainfall", "s(avgtemp)" = "average temperature",
      "ti(time,months)" = "time\u2013season interaction",
      "ti(avgtemp,rainfall)" = "temperature\u2013rainfall interaction"
    )
    sig_readable <- term_labels[sig_terms]
    sig_readable <- sig_readable[!is.na(sig_readable)]
    sig_sentence <- if (length(sig_readable) > 0)
      paste0("The following predictors were statistically significant (p\u00a0<\u00a00.05): ",
             paste(sig_readable, collapse = ", "), ".")
    else "No smooth terms reached statistical significance at the 0.05 level."
    
    structure_label <- format_corarma_label(meta$best_label)
    
    intercept_val <- if (has_pop && !is.na(latest_pop)) {
      round(exp(coef(bg$gam)[["(Intercept)"]]) * latest_pop / 1e3, 2)
    } else {
      round(exp(coef(bg$gam)[["(Intercept)"]]), 4)
    }
    baseline_label <- if (has_pop && !is.na(latest_pop))
      "k cases/month (at latest population)" else " cases per capita (rate only \u2014 no population found)"
    
    dl_id <- paste0("dl_gamm_summary_", gsub(" ", "_", region))
    
    tagList(
      if (!has_pop)
        div(class = "threshold-alert alert-high",
            tags$i(class = "fa fa-triangle-exclamation"),
            " No population column found for this region \u2014 incidence could not be computed; figures below show \u2014."),
      threshold_ui,
      divergence_note,
      tags$br(),
      fluidRow(
        column(3, div(class = "summary-kpi-card",
                      div(class = "kpi-icon kpi-icon-purple", tags$i(class = "fa fa-bug")),
                      div(class = "kpi-value", paste0(total_cases_obs, "k")),
                      div(class = "kpi-label", "Total observed cases"),
                      div(class = "kpi-sub", year_range))),
        column(3, div(class = "summary-kpi-card",
                      div(class = "kpi-icon kpi-icon-teal", tags$i(class = "fa fa-chart-line")),
                      div(class = "kpi-value", if (has_pop) mean_incidence else "\u2014"),
                      div(class = "kpi-label", "Avg monthly incidence"),
                      div(class = "kpi-sub", "per 1,000 population"))),
        column(3, div(class = "summary-kpi-card",
                      div(class = "kpi-icon kpi-icon-blue", tags$i(class = "fa fa-cloud-rain")),
                      div(class = "kpi-value", paste0(mean_rain, " mm")),
                      div(class = "kpi-label", "Avg monthly rainfall"),
                      div(class = "kpi-sub", "Across all months"))),
        column(3, div(class = "summary-kpi-card",
                      div(class = "kpi-icon kpi-icon-amber", tags$i(class = "fa fa-temperature-half")),
                      div(class = "kpi-value", paste0(mean_temp, " \u00b0C")),
                      div(class = "kpi-label", "Avg temperature"),
                      div(class = "kpi-sub", "Monthly average")))
      ),
      tags$br(),
      fluidRow(
        column(6,
               div(class = "summary-section-card",
                   h4(tags$i(class = "fa fa-chart-line"), "Incidence Trend"), tags$hr(),
                   div(class = trend_chip_class,
                       tags$i(class = paste("fa", trend_icon_fa)),
                       if (is.na(trend_pct)) "Insufficient data" else paste0(abs(trend_pct), "% ", trend_dir)),
                   p(style = "font-size:14px;color:#555;line-height:1.65;text-align:justify;",
                     if (has_pop) paste0(
                       "Comparing first third (", first_range,
                       ") and last third (", last_range,
                       ") of study years, population-adjusted incidence in ", region,
                       " has been ", trend_dir, "."
                     ) else "Incidence trend unavailable \u2014 no population data.")
               )),
        column(6,
               div(class = "summary-section-card",
                   h4(tags$i(class = "fa fa-calendar"), "Seasonal Pattern (Incidence-Based)"), tags$hr(),
                   div(class = "summary-info-row", span(class = "summary-info-key", "Peak incidence month"),  span(class = "summary-info-val", peak_month)),
                   div(class = "summary-info-row", span(class = "summary-info-key", "Lowest incidence month"),   span(class = "summary-info-val", low_month)),
                   div(class = "summary-info-row", span(class = "summary-info-key", "Kruskal-Wallis p"),
                       span(class = "summary-info-val", kw_p_label, " ",
                            if (is.na(kw_sig)) "" else if (kw_sig) tags$span(class = "summary-tick-yes", "\u2713") else tags$span(class = "summary-tick-no", "\u2717"))),
                   div(class = "summary-info-row", span(class = "summary-info-key", "Friedman p"),
                       span(class = "summary-info-val", fr_p_label, " ",
                            if (is.na(fr_sig)) "" else if (fr_sig) tags$span(class = "summary-tick-yes", "\u2713") else tags$span(class = "summary-tick-no", "\u2717")))))
      ),
      tags$br(),
      div(class = "summary-section-card",
          div(style = "display:flex;align-items:center;margin-bottom:2px;",
              h4(tags$i(class = "fa fa-calculator"), "Best GAMM"),
              div(class = "gam-model-num-badge", structure_label)),
          tags$hr(),
          div(class = "gam-stat-trio",
              div(class = "gam-stat-pill", div(class = "gam-stat-val", paste0(adj_r2, "%")), div(class = "gam-stat-lbl", "Adj.\u00a0R\u00b2")),
              div(class = "gam-stat-pill", div(class = "gam-stat-val", if (!is.na(aic_lme)) aic_lme else "\u2014"), div(class = "gam-stat-lbl", "AIC (lme)")),
              div(class = "gam-stat-pill", div(class = "gam-stat-val", if (!is.na(bic_lme)) bic_lme else "\u2014"), div(class = "gam-stat-lbl", "BIC (lme)"))),
          
          div(class = "gam-model-grid",
              div(div(class = "gam-section-label", "Smooth terms included"),
                  div(style = "display:flex;flex-wrap:wrap;gap:6px;margin-bottom:10px;",
                      if (has_time)   tags$span(class = "summary-badge badge-blue",  "s(time)"),
                      if (has_months) tags$span(class = "summary-badge badge-teal",  "s(months)"),
                      if (has_rain)   tags$span(class = "summary-badge badge-blue",  "s(rainfall)"),
                      if (has_temp)   tags$span(class = "summary-badge badge-amber", "s(avgtemp)"),
                      if (has_ti_tm)  tags$span(class = "summary-badge badge-purple", "ti(time, months)"),
                      if (has_ti_tr)  tags$span(class = "summary-badge badge-coral", "ti(avgtemp, rainfall)"))),
              div(div(class = "gam-section-label", "Model details"),
                  div(class = "summary-info-row", span(class = "summary-info-key", "Baseline intercept"), span(class = "summary-info-val", paste0(intercept_val, baseline_label))),
                  div(class = "summary-info-row", span(class = "summary-info-key", "Family"),             span(class = "summary-info-val", sprintf("Negative Binomial (theta = %.2f)", meta$theta))),
                  div(class = "summary-info-row", span(class = "summary-info-key", "Estimation method"),  span(class = "summary-info-val", paste0("PQL with ", structure_label))))),
          tags$hr(),
          div(class = "gam-section-label", "Significant predictors (p < 0.05)"),
          p(style = "font-size:13px;color:#555;line-height:1.6;", sig_sentence)),
      tags$br(),
      div(class = "summary-narrative-card",
          div(style = "display:flex;justify-content:space-between;align-items:center;",
              h4(tags$i(class = "fa fa-file-lines"), "Incidence Narrative"),
              downloadButton(dl_id, "Download Narrative", class = "btn btn-warning btn-sm")),
          tags$hr(),
          p(class = "summary-narrative-text",
            "For the ", tags$span(class = "narrative-highlight", region), " region (",
            tags$span(class = "narrative-highlight", year_range), "), the GAMM (population-offset corrected, ",
            structure_label, ") estimates an average incidence of ",
            if (has_pop) tags$span(class = "narrative-highlight", paste0(mean_incidence, " cases per 1,000 population per month")) else "an indeterminate rate (no population data)",
            ".  Incidence has been ", tags$span(class = narrative_trend_class, trend_dir),
            if (!is.na(trend_pct)) tagList(" over the study period (", tags$span(class = "narrative-highlight", paste0(abs(trend_pct), "% change")), ")") else "",
            if (has_pop) tagList(", peaking in ", tags$span(class = "narrative-highlight", peak_month), " and lowest in ", tags$span(class = "narrative-highlight", low_month)) else "",
            ". ",
            if (!is.na(kw_sig)) tagList("The Kruskal-Wallis test confirms that monthly incidence differences are ",
                                        if (kw_sig) tagList(tags$span(class = "summary-tick-yes", "statistically significant"), paste0(" (p\u00a0=\u00a0", kw_p_label, "). "))
                                        else        tagList(tags$span(class = "summary-tick-no", "not statistically significant"), paste0(" (p\u00a0=\u00a0", kw_p_label, "). "))) else "",
            if (!is.na(fr_sig)) tagList("The Friedman test ",
                                        if (fr_sig) tagList(tags$span(class = "summary-tick-yes", "supports this finding"), paste0(" (p\u00a0=\u00a0", fr_p_label, "). "))
                                        else        tagList(tags$span(class = "summary-tick-no", "does not reach significance"), paste0(" (p\u00a0=\u00a0", fr_p_label, "). "))) else "",
            "Mean monthly rainfall is ", tags$span(class = "narrative-highlight", paste0(mean_rain, "\u00a0mm")),
            " and mean average temperature is ", tags$span(class = "narrative-highlight", paste0(mean_temp, "\u00a0\u00b0C")),
            ". The GAMM achieves an adjusted R\u00b2 of ", tags$span(class = "narrative-highlight", paste0(adj_r2, "%")),
            ", with a model AIC of ", tags$span(class = "narrative-highlight", if (!is.na(aic_lme)) aic_lme else "N/A"),
            " and BIC of ", tags$span(class = "narrative-highlight", if (!is.na(bic_lme)) bic_lme else "N/A"), ". ",
            sig_sentence,
            " This can differ from the raw-count GAM Regional Summary, which reports absolute case burden without population adjustment."))
    )
  })
  
  observe({
    region <- req(input$gamm_summary_region)
    dl_id  <- paste0("dl_gamm_summary_", gsub(" ", "_", region))
    output[[dl_id]] <- downloadHandler(
      filename = function() paste0("gamm_incidence_summary_", gsub(" ", "_", region), "_", Sys.Date(), ".txt"),
      content  = function(file) {
        bg  <- get_gamm_model(region)
        meta <- get_gamm_meta(region)
        req(!is.null(bg), !is.null(meta))
        rd  <- data[data$region == region, ]; rd <- rd[order(rd$time), ]
        pop <- get_population_series(rd)
        has_pop <- !all(is.na(pop))
        fitted_counts <- fitted(bg$gam)
        incidence <- if (has_pop) (fitted_counts / pop) * 1000 else NA
        mean_inc <- if (has_pop) round(mean(incidence, na.rm = TRUE), 2) else NA
        total    <- round(sum(rd$uncom, na.rm = TRUE) / 1000, 1)
        yr_range <- paste0(min(rd$year, na.rm = TRUE), "-", max(rd$year, na.rm = TRUE))
        writeLines(paste0(
          "GAM-MAP GAMM Regional Summary (Incidence-Adjusted) \u2014 ", region, "\n",
          "Date: ", Sys.Date(), "\n\n",
          "Region: ", region, "\n",
          "Period: ", yr_range, "\n",
          "Total observed cases: ", total, "k\n",
          "Avg monthly incidence: ", if (has_pop) mean_inc else "N/A (no population data)", " cases/1,000 pop\n",
          "Correlation structure: ", format_corarma_label(meta$best_label), "\n"
        ), file)
      }
    )
  })
  
  
  # ==================================================================================
  # OUTPUT-SUSPENSION SETTINGS (suspendWhenHidden = FALSE)
  # Keeps outputs computing even while their tab isn't the active one — needed
  # because several tabs are reached only via the JS section-tab-bar (not
  # Shiny's own tabsetPanel), so Shiny doesn't otherwise know they're "visible".
  # ==================================================================================
  
  outputOptions(output, "statsTableOverall",           suspendWhenHidden = FALSE)
  outputOptions(output, "statsTableAnnual",            suspendWhenHidden = FALSE)
  outputOptions(output, "results_table",               suspendWhenHidden = FALSE)
  outputOptions(output, "short_range",                 suspendWhenHidden = FALSE)
  outputOptions(output, "short_range_tests",           suspendWhenHidden = FALSE)
  outputOptions(output, "best_model_smooth_terms",     suspendWhenHidden = FALSE)
  outputOptions(output, "model_metrics",               suspendWhenHidden = FALSE)
  outputOptions(output, "qq_plot",                     suspendWhenHidden = FALSE)
  outputOptions(output, "response_fitted_plot",        suspendWhenHidden = FALSE)
  outputOptions(output, "observed_fitted",             suspendWhenHidden = FALSE)
  outputOptions(output, "combined_plot",               suspendWhenHidden = FALSE)
  outputOptions(output, "combined_heatmap",            suspendWhenHidden = FALSE)
  outputOptions(output, "malaria_plot",                suspendWhenHidden = FALSE)
  outputOptions(output, "forecast_plot",               suspendWhenHidden = FALSE)
  outputOptions(output, "forecast_table",              suspendWhenHidden = FALSE)
  outputOptions(output, "ghana_map",                   suspendWhenHidden = FALSE)
  outputOptions(output, "map_summary_table",           suspendWhenHidden = FALSE)
  outputOptions(output, "compare_ts_plot",             suspendWhenHidden = FALSE)
  outputOptions(output, "compare_bar_plot",            suspendWhenHidden = FALSE)
  outputOptions(output, "compare_summary_table",       suspendWhenHidden = FALSE)
  outputOptions(output, "regional_summary_ui",         suspendWhenHidden = FALSE)
  outputOptions(output, "yoy_badge",                   suspendWhenHidden = FALSE)
  outputOptions(output, "threshold_alert",             suspendWhenHidden = FALSE)
  outputOptions(output, "gam_acf_plot",                suspendWhenHidden = FALSE)
  outputOptions(output, "gam_pacf_plot",               suspendWhenHidden = FALSE)
  outputOptions(output, "gam_ljung_box_table",         suspendWhenHidden = FALSE)
  outputOptions(output, "gam_autocorr_banner",         suspendWhenHidden = FALSE)
  outputOptions(output, "gam_autocorr_banner_summary", suspendWhenHidden = FALSE)
  outputOptions(output, "concurvity_overall_table",    suspendWhenHidden = FALSE)
  outputOptions(output, "concurvity_pairwise_table",   suspendWhenHidden = FALSE)
  
  for (id in c("model_plots_1", "model_plots_2", "model_plots_3",
               "model_plots_4", "model_plots_5", "model_plots_6")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
  
  for (id in c("gamm_model_metrics", "gamm_best_model_smooth_terms", "gamm_corr_info",
               "gamm_forecast_plot", "gamm_forecast_table", "gamm_observed_fitted",
               "gamm_qq_plot", "gamm_response_fitted_plot", "gamm_diag_acf_plot",
               "gamm_diag_pacf_plot", "gamm_diag_ljung_box_table")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
  
  for (id in c("gamm_model_plots_1", "gamm_model_plots_2", "gamm_model_plots_3",
               "gamm_model_plots_4", "gamm_model_plots_5", "gamm_model_plots_6")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
  
  outputOptions(output, "gamm_autocorr_banner",         suspendWhenHidden = FALSE)
  outputOptions(output, "gamm_autocorr_banner_summary", suspendWhenHidden = FALSE)
  outputOptions(output, "gamm_regional_summary_ui",     suspendWhenHidden = FALSE)
  
  outputOptions(output, "model_status_overview",         suspendWhenHidden = FALSE)
  
  
}

