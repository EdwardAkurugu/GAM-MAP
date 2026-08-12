# ============================================================================
# GAMM_SEASONAL_SMOOTH.R — OFFLINE precompute of "no s(months)" comparison
# stats for the GAMM Seasonal Smooth Effect tab.
# Run standalone: source("models/helpers.R"); then this script.
# Requires: model_*_gamm.rds / model_*_gamm_meta.rds already exist (from gamm.R).
# Produces: model_<PFX>_gamm_noseason_stats.rds
# ============================================================================

source("models/helpers.R")

if (!exists("data")) stop("`data` not found. Source your data-prep pipeline first.")

for (region_name in gamm_regions) {
  pfx <- pfx_lookup[[region_name]]
  cat(sprintf("\n---- %s (%s) ----\n", region_name, pfx))
  
  bg   <- tryCatch(readRDS(paste0("model_", pfx, "_gamm.rds")),      error = function(e) NULL)
  meta <- tryCatch(readRDS(paste0("model_", pfx, "_gamm_meta.rds")), error = function(e) NULL)
  
  if (is.null(bg) || is.null(meta)) {
    cat("  Skipping — model_*_gamm.rds or _gamm_meta.rds missing.\n")
    next
  }
  
  units           <- get_term_units(bg$gam)
  drop_result     <- drop_seasonal_unit(units)
  structure_label <- format_corarma_label(meta$best_label)
  
  lb <- meta$ljung_box
  lb_full_p <- { val <- lb$p_value[lb$lag == 12]; if (length(val) == 0) NA_real_ else val }
  
  if (is.null(drop_result)) {
    stats <- list(
      AIC_Full = round(AIC(bg$lme), 1), AIC_NoSeason = NA, Delta_AIC = NA,
      BIC_Full = round(BIC(bg$lme), 1), BIC_NoSeason = NA, Delta_BIC = NA,
      AdjR2_Full = round(tryCatch(summary(bg$gam)$r.sq * 100, error = function(e) NA_real_), 1),
      AdjR2_NoSeason = NA, Max_Concurvity = NA,
      LB_p_Full = round(lb_full_p, 4), LB_p_NoSeason = NA,
      Structure = structure_label, Note = "No s(months) term in best GAMM"
    )
    saveRDS(stats, file = paste0("model_", pfx, "_gamm_noseason_stats.rds"))
    cat("  No s(months) term — saved note-only stats.\n")
    next
  }
  
  reg_data <- data[data$region == region_name, ]
  reg_data <- reg_data[order(reg_data$time), ]
  reg_data$g <- factor(1)
  
  theta_hat <- meta$theta
  nums <- suppressWarnings(as.integer(regmatches(meta$best_label, gregexpr("[0-9]+", meta$best_label))[[1]]))
  p <- if (length(nums) >= 1 && !is.na(nums[1])) nums[1] else 0
  q <- if (length(nums) >= 2 && !is.na(nums[2])) nums[2] else 0
  
  reduced_terms   <- unlist(drop_result$reduced_units)
  reduced_formula <- as.formula(paste("uncom ~ offset(log_pop_offset) +", paste(reduced_terms, collapse = " + ")))
  
  cat(sprintf("  Fitting reduced (no-season) GAMM with %s ...\n", structure_label))
  fit_reduced <- tryCatch({
    if (p == 0 && q == 0) {
      gamm(reduced_formula, data = reg_data, family = negative.binomial(theta = theta_hat), niterPQL = 100)
    } else {
      gamm(reduced_formula, data = reg_data, family = negative.binomial(theta = theta_hat),
           correlation = corARMA(form = ~ time | g, p = p, q = q),
           control = lmeControl(maxIter = 100, msMaxIter = 100, niterEM = 100),
           niterPQL = 100)
    }
  }, error = function(e) { cat(sprintf("    FAILED: %s\n", conditionMessage(e))); NULL })
  
  wc <- worst_concurvity_excluding_self_interactions(bg$gam, units)
  aic_full   <- AIC(bg$lme)
  bic_full   <- BIC(bg$lme)
  adjr2_full <- tryCatch(summary(bg$gam)$r.sq * 100, error = function(e) NA_real_)
  
  if (is.null(fit_reduced)) {
    stats <- list(
      AIC_Full = round(aic_full, 1), AIC_NoSeason = NA, Delta_AIC = NA,
      BIC_Full = round(bic_full, 1), BIC_NoSeason = NA, Delta_BIC = NA,
      AdjR2_Full = round(adjr2_full, 1), AdjR2_NoSeason = NA,
      Max_Concurvity = round(wc$value, 3),
      LB_p_Full = round(lb_full_p, 4), LB_p_NoSeason = NA,
      Structure = structure_label, Note = "Reduced GAMM failed to fit/converge under PQL"
    )
    saveRDS(stats, file = paste0("model_", pfx, "_gamm_noseason_stats.rds"))
    cat("  Reduced fit failed — saved failure stats.\n")
    next
  }
  
  aic_red   <- AIC(fit_reduced$lme)
  bic_red   <- BIC(fit_reduced$lme)
  adjr2_red <- tryCatch(summary(fit_reduced$gam)$r.sq * 100, error = function(e) NA_real_)
  
  resid_norm_red <- tryCatch(residuals(fit_reduced$lme, type = "normalized"), error = function(e) NULL)
  lb_red_p <- if (!is.null(resid_norm_red)) {
    bt <- tryCatch(Box.test(resid_norm_red, lag = 12, type = "Ljung-Box"), error = function(e) NULL)
    if (is.null(bt)) NA_real_ else bt$p.value
  } else NA_real_
  
  stats <- list(
    AIC_Full = round(aic_full, 1), AIC_NoSeason = round(aic_red, 1), Delta_AIC = round(aic_red - aic_full, 1),
    BIC_Full = round(bic_full, 1), BIC_NoSeason = round(bic_red, 1), Delta_BIC = round(bic_red - bic_full, 1),
    AdjR2_Full = round(adjr2_full, 1), AdjR2_NoSeason = round(adjr2_red, 1),
    Max_Concurvity = round(wc$value, 3),
    LB_p_Full = round(lb_full_p, 4), LB_p_NoSeason = round(lb_red_p, 4),
    Structure = structure_label, Note = ""
  )
  saveRDS(stats, file = paste0("model_", pfx, "_gamm_noseason_stats.rds"))
  cat(sprintf("  Saved model_%s_gamm_noseason_stats.rds (\u0394AIC = %.1f)\n", pfx, stats$Delta_AIC))
}

cat("\nDone. Copy all model_*_gamm_noseason_stats.rds files into the Shiny app's\n",
    "working directory alongside the other model_*.rds files.\n")

