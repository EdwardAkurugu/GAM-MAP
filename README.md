# GAM(M)-MAP: GAM(M)-Based Malaria Analytics Platform
<p align="center">
  <strong>Temporal and Climatic Analysis of Uncomplicated Malaria in Ghana</strong><br/>
  A Generalised Additive Model (GAM) and Generalised Additive Mixed Model (GAMM) framework for regional malaria epidemiology
</p>
<p align="center">
  <img src="https://img.shields.io/badge/R-Shiny-blue?logo=r" alt="R Shiny"/>
  <img src="https://img.shields.io/badge/Model-GAM%20(NB)%20%7C%20GAMM%20(corARMA)-brightgreen" alt="Model"/>
  <img src="https://img.shields.io/badge/Regions-10%20Former%20Regions-orange" alt="Regions"/>
  <img src="https://img.shields.io/badge/Period-2012--2023-purple" alt="Period"/>
  <img src="https://img.shields.io/badge/License-MIT-lightgrey" alt="License"/>
</p>

---

## Overview

**GAM(M)-MAP** is an interactive R Shiny dashboard built to accompany the research article:

> **Akurugu E., Awine T., Seidu B., Peprah N.Y., Mohammed W., Boateng P., Abiwu P.H.A.K., Silal S.P.**
> *Temporal and climatic drivers of uncomplicated malaria in Ghana: A Regional Generalised Additive Model (GAM) and Generalised Additive Mixed Model (GAMM) analysis.*
> PLOS ONE (under review), 2026.

The platform enables users to explore regional malaria case patterns across Ghana's ten former administrative regions (2012–2023), investigate nonlinear relationships between climate variables and malaria, and interact with fitted GAM and GAMM model outputs — all through an intuitive browser-based interface.

---

## Background
Malaria remains endemic across all of Ghana and is influenced by diverse seasonal and climatic factors. This project applies GAMs and GAMMs, fitted with a negative binomial distribution to:
- Capture **nonlinear** relationships between malaria cases and climate (rainfall, temperature) variables
- Decompose **long-term trend**, **seasonality**, and **temporal interactions**
- Correct for **residual serial autocorrelation** where present, using corARMA(p,q) correlation structures within a GAMM framework
- Deliver **region-specific** insights across Ghana's three ecological zones:
  - **Guinea Savannah** (Upper East, Upper West, Northern) — unimodal malaria peak
  - **Transitional Forest** (Brong Ahafo, Ashanti, Eastern, Volta) — bimodal malaria peaks
  - **Coastal** (Greater Accra, Central, Western) — bimodal malaria peaks

---

## Features
| Tab | Description |
|-----|-------------|
| **Welcome** | Platform overview, objectives, and getting-started guide |
| **About** | Study background, ecological zones map, and data sources |
| **Help** | Navigation guide, tips, and contact information |
| **Model Validity** | Per-region GAM vs. GAMM recommendation summary (Ljung-Box residual autocorrelation), concurvity diagnostics, and seasonal smooth-term justification for both GAM and GAMM |
| **Descriptives** | Annual and overall summary statistics for malaria cases and climate variables, correlation analysis, interactive incidence map, and region-to-region comparisons |
| **Time Series** | STL decomposition (observed, trend, seasonal, remainder components) |
| **Seasonality** | Monthly boxplots and formal tests of seasonality (Kruskal-Wallis, Friedman) |
| **GAM Estimates** | GAM model selection metrics (REML score, Adj. R², Deviance Explained), smooth term coefficient tables, and forecast plots/tables |
| **GAM Diagnostics** | Observed vs. fitted, Q-Q, response vs. fitted plots, and residual autocorrelation (ACF/PACF, Ljung-Box) |
| **GAM Plots** | Partial effect smooth term plots for all covariates and interactions |
| **GAM Regional Summary** | Integrated view of regional malaria burden, climate drivers, and GAM outputs, including key findings, and evidence‑based insights,  |
| **GAMM Estimates** | corARMA(p,q) correlation-structure model comparison, smooth term and correlation coefficient tables, and forecast plots/tables |
| **GAMM Diagnostics** | Observed vs. fitted, Q-Q, response vs. fitted plots, and residual autocorrelation on normalised residuals |
| **GAMM Plots** | Partial effect smooth term plots from the GAMM's underlying GAM component |
| **GAMM Regional Summary** | Incidence-adjusted regional summary (population-offset corrected) using the corARMA-corrected GAMM |
| **Appendix** | Combined time series plots, heatmaps, seasonal bar-line patterns, and the GAM & GAMM modeling framework |
| **Source Code** | Direct link to the project's GitHub repository |

---
## Data Sources

| Data | Source | Period |
|------|--------|--------|
| Uncomplicated malaria cases | District Health Information Management System II (DHIMS2), Ghana Health Service / National Malaria Elimination Programme (NMEP) | Jan 2012 – Dec 2023 |
| Rainfall (mm) | Ghana Meteorological Agency (GMeT) | Jan 2012 – Dec 2023 |
| Min / Avg / Max temperature (°C) | Ghana Meteorological Agency (GMeT) | Jan 2012 – Dec 2023 |

> **Note:** The six new regions created in 2019 (Western North, Ahafo, Bono East, Oti, Savannah, North East) were consolidated with their parent regions to maintain a consistent 2012–2023 time series across the ten former administrative regions.

---

## Statistical Methods

### Model Family
All regional GAMs use a **negative binomial** distribution with a **log-link function** to handle overdispersion in monthly malaria count data. Each model includes a population offset, `offset(log_pop_offset)`, so that predictors are modeled against malaria *incidence* relative to the local population rather than raw case counts.

### Full Model Structure

```
uncom ~ offset(log_pop_offset) + s(time) + s(months) + s(rainfall) + s(avgtemp) +
        ti(time, months) + ti(avgtemp, rainfall)
```
where:
- `offset(log_pop_offset)` — population offset, converting the response to a case-rate model
- `s(time)` — long-term trend (nonlinear)
- `s(months)` — seasonality (cyclic)
- `s(rainfall)` — nonlinear effect of monthly rainfall
- `s(avgtemp)` — nonlinear effect of average temperature
- `ti(time, months)` — tensor product interaction (trend × seasonality)
- `ti(avgtemp, rainfall)` — tensor product interaction (temperature × rainfall)

### Model Selection
Eight candidate model specifications per region — each omitting a different smooth term or interaction — were fitted via REML. The best-fitting model was selected as the one maximising the **product of Adjusted R² and Deviance Explained**.

### GAMM Extension
For regions where the best GAM's deviance residuals showed significant serial autocorrelation (Ljung-Box test, lags 12–60 months), a Generalised Additive Mixed Model (GAMM) was fitted on the same smoother, adding a **corARMA(p,q)** correlation structure. A grid of `p, q` combinations was searched, models were fitted via Penalised Quasi-Likelihood (PQL) with the negative binomial (θ) held fixed from the best GAM in that region, and the best-converged structure was selected by lowest AIC of the linear mixed-effects (`lme`) component.

## Software
All GAMs and GAMMs were fitted using the [`mgcv`](https://cran.r-project.org/package=mgcv) and [`nlme`](https://cran.r-project.org/package=nlme) packages in **R version 4.6.1**.

---

## Key Results Summary — Best Selected Model per Region

| Region | Ecological Zone | Model Type | Structure | Adj. R² |
|--------|-----------------|------------|-----------|---------|
| Upper East | Guinea Savannah | GAMM | corARMA(1, 0) | 90.00% |
| Upper West | Guinea Savannah | GAMM | corARMA(0, 1) | 84.80% |
| Northern | Guinea Savannah | GAM | Model 5 | 93.63% |
| Brong Ahafo | Transitional Forest | GAMM | corARMA(0, 1) | 87.40% |
| Ashanti | Transitional Forest | GAMM | corARMA(1, 1) | 81.80% |
| Eastern | Transitional Forest | GAMM | corARMA(1, 1) | 77.50% |
| Volta | Transitional Forest | GAMM | corARMA(3, 3) | 82.10% |
| Greater Accra | Coastal | GAMM | corARMA(1, 0) | 39.70% |
| Central | Coastal | GAMM | corARMA(2, 1) | 82.40% |
| Western | Coastal | GAMM | corARMA(1, 0) | 86.00% |

*Adj. R² for GAMM rows is that of the underlying GAM component (PQL-based); Northern is the only region where the standalone GAM cleared the Ljung-Box residual-autocorrelation test at every lag and did not require a corARMA correction.*

---

## Repository Structure
```
GAMM-MAP/
├── app.R                                             # Main entry point: loads dependencies, sources all files, runs app
├── app/
│ ├── server.R                                        # Server logic (runtime only — reads .rds via safe_readRDS(), never fits models)
│ └── ui.R                                            # UI definition
├── dashboard_styles.R                                # Custom CSS
├── packages.R                                        # Package dependencies
├── data.R                                            # Data loading and preparation
├── static_plot_function.R                            # Static plot functions
├── plot_themes.R                                     # Plot themes
├── models/
│ ├── helpers.R                                       # Shared helpers/lookups (pfx_lookup, k_time_lookup, get_term_units,
│ │                                                   #   drop_seasonal_unit, worst_concurvity_..., get_best_model,
│ │                                                   #   format_corarma_label, build_model_status_table,
│ │                                                   #   reset_tab_filters, get_population_series, etc.)
│ │                                                   #   Sourced by gam.R, gamm.R, gam_seasonal_smooth.R,
│ │                                                   #   gamm_seasonal_smooth.R, AND app/server.R — single source of truth.
│ │
│ ├── gam.R                                           # OFFLINE: fits 8 candidate NB-GAMs per region (source helpers.R first)
│ │                                                   #   → writes model_<PFX>_1.rds ... model_<PFX>_8.rds
│ ├── gamm.R                                          # OFFLINE: fits corARMA(p,q) grid per region, selects best by AIC
│ │                                                   #   → writes model_<PFX>_gamm.rds, model_<PFX>_gamm_meta.rds,
│ │                                                   #     gamm_corarma_grid_comparison_<PFX>.csv
│ ├── gam_seasonal_smooth.R                           # OFFLINE: precomputes "no s(months)" GAM comparison stats
│ │                                                   #   (requires model_<PFX>_1..8.rds from gam.R)
│ │                                                   #   → writes model_<PFX>_gam_noseason_stats.rds
│ ├── gamm_seasonal_smooth.R                          # OFFLINE: precomputes "no s(months)" GAMM comparison stats
│ │                                                   #   (requires model_<PFX>_gamm.rds / _gamm_meta.rds from gamm.R)
│ │                                                   #   → writes model_<PFX>_gamm_noseason_stats.rds
│ │
│ │   # ---- Generated .rds/.csv outputs (produced by the four scripts above; read at runtime by app/server.R) ----
│ ├── model_UE_1.rds ... model_UE_8.rds               # Upper East — 8 candidate GAMs
│ ├── model_UW_1.rds ... model_UW_8.rds               # Upper West
│ ├── model_NO_1.rds ... model_NO_8.rds               # Northern
│ ├── model_BA_1.rds ... model_BA_8.rds               # Brong Ahafo
│ ├── model_AS_1.rds ... model_AS_8.rds               # Ashanti
│ ├── model_EA_1.rds ... model_EA_8.rds               # Eastern
│ ├── model_VO_1.rds ... model_VO_8.rds               # Volta
│ ├── model_GA_1.rds ... model_GA_8.rds               # Greater Accra
│ ├── model_CE_1.rds ... model_CE_8.rds               # Central
│ ├── model_WE_1.rds ... model_WE_8.rds               # Western
│ │
│ ├── model_<PFX>_gamm.rds                            # Best GAMM per region (9 regions, excludes Northern)
│ ├── model_<PFX>_gamm_meta.rds                       # GAMM metadata (corARMA label, phi, theta, Ljung-Box)
│ ├── gamm_corarma_grid_comparison_<PFX>.csv          # Full corARMA(p,q) grid search results per region
│ │
│ ├── model_NO_gam_noseason_stats.rds                 # GAM  "no s(months)" comparison stats (Northern only — gam_regions)
│ └── model_<PFX>_gamm_noseason_stats.rds             # GAMM "no s(months)" comparison stats (9 regions — gamm_regions)
│
└── www/
├── images/                                           # Logos and figures
└── markdown/                                         # Help, About, and Welcome page content
├── about_page_.Rmd
├── welcome_page_.Rmd
├── help_page_.Rmd
├── summary_help_*.Rmd
└── gam(m)_model_building.md

```

**`app.R`**

```r
source("packages.R")
source("data.R")
source("static_plot_function.R")
source("plot_themes.R")
source("dashboard_styles.R")
source("app/server.R")
source("app/ui.R")
shinyApp(ui = ui, server = server)

```
> `server.R` must be sourced before `ui.R` — `ui.R` references `gam_regions` and `gamm_regions`, which are defined at the top level of `server.R`, directly at UI-build time.

---

## Installation & Running Locally

### Prerequisites

- R ≥ 4.6.1
- RStudio (recommended)

### Required R Packages

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinyBS", "shinycssloaders",
  "DT", "plotly", "sf", "leaflet",
  "mgcv", "gratia", "gridExtra", "markdown", "htmltools",
  "nlme", "MASS", "gt", "reshape2", "readxl"
))

```
### Clone and Run

```bash
git clone https://github.com/EdwardAkurugu/GAMM-MAP.git
cd GAMM-MAP
```

Then in R:

```r
shiny::runApp()
```


> **Note:** Pre-fitted model `.rds` files must be present in the working directory (or `models/` subfolder) before launching the app. The app loads all 80 regional model objects at startup. Fitting models from scratch requires access to the DHIMS2 and GMeT datasets, which are available upon request from NMEP and GMeT respectively.

---

## Ethical Approval

- **Ghana Health Service Ethical Review Committee:** GHS-ERC: 019/04/24
- **Human Research Ethics Committee, University of Cape Town:** SCI/01798/2025

Data access was granted by Ghana's National Malaria Elimination Programme (NMEP) and the Ghana Meteorological Agency (GMeT).

---

## Funding

This work was supported by the **Bill & Melinda Gates Foundation** [Grant number: INV047‑048]. The funders had no role in study design, data collection and analysis, decision to publish, or preparation of the manuscript.

---

## Authors & Affiliations

| Author | Affiliation |
|--------|------------|
| **Edward Akurugu** *(Corresponding)* | MASHA, Dept. of Statistical Sciences, University of Cape Town |
| Timothy Awine | Navrongo Health Research Centre, Ghana |
| Baba Seidu | C.K. Tedam University of Technology and Applied Sciences, Ghana |
| Nana Yaw Peprah | National Malaria Elimination Programme, Ghana Health Service |
| Wahjib Mohammed | National Malaria Elimination Programme, Ghana Health Service |
| Paul Boateng | National Malaria Elimination Programme, Ghana Health Service |
| Paul Hilarius Asiwome Kosi Abiwu | National Malaria Elimination Programme, Ghana Health Service |
| Sheetal Prakash Silal | MASHA, UCT & Nuffield Dept. of Medicine, University of Oxford |

📧 Corresponding author: [akredw001@myuct.ac.za](mailto:akredw001@myuct.ac.za) | [akuruguedward@gmail.com](mailto:akuruguedward@gmail.com) 

---

## Citation

If you use this platform or the associated analysis in your work, please cite:

```bibtex
@article{akurugu2026gammap,
  author  = {Akurugu, Edward and Awine, Timothy and Seidu, Baba and
             Peprah, Nana Yaw and Mohammed, Wahjib and Boateng, Paul and
             Abiwu, Paul Hilarius Asiwome Kosi and Silal, Sheetal Prakash},
  title   = {Temporal and climatic drivers of uncomplicated malaria in Ghana:
             A Regional Generalised Additive Model and Generalised Additive Mixed Model analysis},
  journal = {PLOS ONE},
  year    = {2026},
  note    = {Under review}
}
```

---

## License

This project is released under a **MIT License** licence.

---

## Acknowledgements

- [Modelling and Simulation Hub, Africa (MASHA), University of Cape Town](https://masha.uct.ac.za)
- [Clinton Health Access Initiative (CHAI), South Africa](https://www.clintonhealthaccess.org/south-africa/)
- [Southern African Development Community Elimination 8 (SADC E8)](https://sadce8.org/)
- [Ghana Health Service — National Malaria Elimination Programme](https://ghs.gov.gh/)
- [Ghana Meteorological Agency (GMeT)](https://www.meteo.gov.gh/)
