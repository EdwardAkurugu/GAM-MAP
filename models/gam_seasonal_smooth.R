# ============================================================================
# GAM_SEASONAL_SMOOTH.R — OFFLINE precompute of "no s(months)" comparison
# stats for the GAM Seasonal Smooth Effect tab.
# Run standalone: source("models/helpers.R"); then this script.
# Requires: model_*_1..8.rds already exist (from gam.R).
# Produces: model_<PFX>_gam_noseason_stats.rds
# ============================================================================

source("models/helpers.R")

if (!exists("data")) stop("`data` not found. Source your data-prep pipeline first.")

for (region_name in gam_regions) {
  pfx <- pfx_lookup[[region_name]]
  cat(sprintf("\n---- %s (%s) ----\n", region_name, pfx))
  
  models <- lapply(1:8, function(j) {
    tryCatch(readRDS(paste0("model_", pfx, "_", j, ".rds")), error = function(e) NULL)
  })
  bm <- get_best_model(models)
  
  if (is.null(bm)) {
    stats <- list(
      AIC_Full = NA, AIC_NoSeason = NA, Delta_AIC = NA,
      Dev_Full = NA, Dev_NoSeason = NA, Delta_Dev = NA,
      Max_Concurvity = NA, LB_p_Full = NA, LB_p_NoSeason = NA,
      Note = "Best GAM unavailable"
    )
    saveRDS(stats, file = paste0("model_", pfx, "_gam_noseason_stats.rds"))
    cat("  Best GAM unavailable — saved note-only stats.\n")
    next
  }
  
  units       <- get_term_units(bm)
  drop_result <- drop_seasonal_unit(units)
  
  if (is.null(drop_result)) {
    stats <- list(
      AIC_Full = round(AIC(bm), 1), AIC_NoSeason = NA, Delta_AIC = NA,
      Dev_Full = round(summary(bm)$dev.expl * 100, 1), Dev_NoSeason = NA, Delta_Dev = NA,
      Max_Concurvity = NA, LB_p_Full = round(lb_p_at_lag(bm, 12), 4), LB_p_NoSeason = NA,
      Note = "No s(months) term in best model"
    )
    saveRDS(stats, file = paste0("model_", pfx, "_gam_noseason_stats.rds"))
    cat("  No s(months) term — saved note-only stats.\n")
    next
  }
  
  reg_data  <- data[data$region == region_name, ]
  m_reduced <- fit_units_model(drop_result$reduced_units, reg_data)
  wc        <- worst_concurvity_excluding_self_interactions(bm, units)
  
  aic_full <- AIC(bm)
  aic_red  <- if (!is.null(m_reduced)) AIC(m_reduced) else NA_real_
  dev_full <- summary(bm)$dev.expl * 100
  dev_red  <- if (!is.null(m_reduced)) summary(m_reduced)$dev.expl * 100 else NA_real_
  lb_full  <- lb_p_at_lag(bm, 12)
  lb_red   <- if (!is.null(m_reduced)) lb_p_at_lag(m_reduced, 12) else NA_real_
  
  stats <- list(
    AIC_Full       = round(aic_full, 1),
    AIC_NoSeason   = round(aic_red, 1),
    Delta_AIC      = round(aic_red - aic_full, 1),
    Dev_Full       = round(dev_full, 1),
    Dev_NoSeason   = round(dev_red, 1),
    Delta_Dev      = round(dev_full - dev_red, 1),
    Max_Concurvity = round(wc$value, 3),
    LB_p_Full      = round(lb_full, 4),
    LB_p_NoSeason  = round(lb_red, 4),
    Note           = if (is.null(m_reduced)) "Reduced model failed to fit" else ""
  )
  saveRDS(stats, file = paste0("model_", pfx, "_gam_noseason_stats.rds"))
  cat(sprintf("  Saved model_%s_gam_noseason_stats.rds (\u0394AIC = %.1f)\n", pfx, stats$Delta_AIC))
}

cat("\nDone. Copy all model_*_gam_noseason_stats.rds files into the Shiny app's\n",
    "working directory alongside the other model_*.rds files.\n")


