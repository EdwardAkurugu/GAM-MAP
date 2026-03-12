# GAM-MAP: GAM-Based Malaria Analytics Platform

<p align="center">
  <strong>Temporal and Climatic Analysis of Uncomplicated Malaria in Ghana</strong><br/>
  A Generalised Additive Model (GAM) framework for regional malaria epidemiology
</p>

<p align="center">
  <img src="https://img.shields.io/badge/R-Shiny-blue?logo=r" alt="R Shiny"/>
  <img src="https://img.shields.io/badge/Model-GAM%20%7C%20quasi--Poisson-brightgreen" alt="Model"/>
  <img src="https://img.shields.io/badge/Regions-10%20Former%20Regions-orange" alt="Regions"/>
  <img src="https://img.shields.io/badge/Period-2012--2023-purple" alt="Period"/>
  <img src="https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey" alt="License"/>
</p>

---

## Overview

**GAM-MAP** is an interactive R Shiny dashboard built to accompany the research article:

> **Akurugu E., Awine T., Seidu B., Peprah N.Y., Mohammed W., Boateng P., Abiwu P.H.A.K., Silal S.P.**
> *Temporal and climatic drivers of uncomplicated malaria in Ghana: A Regional Generalised Additive Model analysis.*
> PLOS ONE (under review), 2026.

The platform enables users to explore regional malaria transmission dynamics across Ghana's ten former administrative regions (2012–2023), investigate relationships between meteorological variables and malaria caseloads, and interact with fitted GAM model outputs — all through an intuitive browser-based interface.

---

## Background

Malaria remains endemic across all of Ghana and is influenced by diverse seasonal and climatic factors. This project applies Generalised Additive Models (GAMs) with a quasi-Poisson distribution to:

- Capture **nonlinear** relationships between malaria and weather (rainfall, temperature)
- Decompose **long-term trend**, **seasonality**, and **temporal interactions**
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
| **Descriptives** | Annual and overall summary statistics for malaria and climate variables |
| **Time Series** | STL decomposition (observed, trend, seasonal, remainder components) |
| **Seasonality** | Monthly boxplots and formal tests of seasonality (Kruskal-Wallis, Friedman) |
| **Model Estimates** | GAM model selection metrics (GCV, Adj. R², Deviance Explained) and smooth term coefficient tables |
| **Model Diagnostics** | Observed vs. fitted plots, Q-Q plots, and response vs. fitted plots |
| **Model Plots** | Partial effect smooth term plots for all covariates and interactions |
| **Appendix** | Combined time series plots, heatmaps, seasonal bar-line patterns, and GAM framework |

---

## Data Sources

| Data | Source | Period |
|------|--------|--------|
| Uncomplicated malaria cases | District Health Information Management System II (DHIMS2), Ghana Health Service / National Malaria Elimination Programme (NMEP) | Jan 2012 – Dec 2023 |
| Monthly rainfall (mm) | Ghana Meteorological Agency (GMeT) — 12 stations | Jan 2012 – Dec 2023 |
| Min / Avg / Max temperature (°C) | Ghana Meteorological Agency (GMeT) — 12 stations | Jan 2012 – Dec 2023 |

> **Note:** The six new regions created in 2019 (Western North, Ahafo, Bono East, Oti, Savannah, North East) were consolidated with their parent regions to maintain a consistent 2012–2023 time series across the ten former administrative regions.

---

## Statistical Methods

### Model Family
All regional GAMs use a **quasi-Poisson** distribution with a **log-link function** to handle overdispersion in monthly malaria count data.

### Full Model Structure

```
uncom ~ s(time) + s(months) + s(rainfall) + s(avgtemp) +
        ti(time, months) + ti(avgtemp, rainfall)
```

Where:
- `s(time)` — long-term trend (nonlinear)
- `s(months)` — seasonality (cyclic)
- `s(rainfall)` — nonlinear effect of monthly rainfall
- `s(avgtemp)` — nonlinear effect of average temperature
- `ti(time, months)` — tensor product interaction (trend × seasonality)
- `ti(avgtemp, rainfall)` — tensor product interaction (temperature × rainfall)

### Model Selection
Eight candidate models per region were fitted via backward elimination. Final models were selected using:
- **Adjusted R²**
- **Deviance Explained**

### Software
All models were fitted using the [`mgcv`](https://cran.r-project.org/package=mgcv) package in **R version 4.5.2**.

---

## Key Results Summary

| Region | Ecological Zone | Selected Model | GCV | Adj. R² | Dev. Explained |
|--------|----------------|---------------|-----|---------|----------------|
| Upper East | Guinea Savannah | Model 1 | 801.31 | 94.9% | 96.5% |
| Upper West | Guinea Savannah | Model 4 | 771.04 | 90.8% | 93.7% |
| Northern | Guinea Savannah | Model 1 | 654.10 | 95.2% | 96.1% |
| Brong Ahafo | Transitional Forest | Model 5 | 883.81 | 91.9% | 94.6% |
| Ashanti | Transitional Forest | Model 1 | 472.40 | 92.9% | 95.5% |
| Eastern | Transitional Forest | Model 1 | 606.18 | 91.9% | 95.5% |
| Volta | Transitional Forest | Model 1 | 510.48 | 92.0% | 94.8% |
| Greater Accra | Coastal | Model 4 | 306.91 | 68.0% | 76.9% |
| Central | Coastal | Model 1 | 508.03 | 90.0% | 93.6% |
| Western | Coastal | Model 1 | 566.42 | 92.7% | 95.0% |

---

## Repository Structure
```
GAM-MAP/
├── app.R                        # Main Shiny app entry point
├── server.R                     # Server logic
├── ui.R                         # UI definition
├── dashboard_styles.R           # Custom CSS
├── global.R                     # Global data loading and packages
├── packages.R                   # Package dependencies
├── data.R                       # Data loading and preparation
├── static plot function.R       # Static plot functions
└── plot_themes.R                # Plot themes
```

**`global.R`**
```r
# Load packages
source("packages.R")

# Load and prepare data
source("data.R")

# Load static plot functions
source("static plot function.R")

# Load plot themes
source("plot_themes.R")

# Load custom CSS styles
source("dashboard_styles.R")
```

**`app.R`**
```r
# Source global dependencies
source("global.R")

# Source UI and server
source("ui.R")
source("server.R")

# Run the Shiny application
shinyApp(ui = ui, server = server)
```


## Repository Structure

```
GAM-MAP/
├── app.R                        # Main Shiny app entry point
├── server.R                     # Server logic
├── ui.R                         # UI definition
├── dashboard_styles.R           # Custom CSS
├── global.R                     # Global data loading and packages
│
├── models/                      # Pre-fitted RDS model objects
│   ├── model_UE_1.rds ... model_UE_8.rds   # Upper East
│   ├── model_UW_1.rds ... model_UW_8.rds   # Upper West
│   ├── model_NO_1.rds ... model_NO_8.rds   # Northern
│   ├── model_BA_1.rds ... model_BA_8.rds   # Brong Ahafo
│   ├── model_AS_1.rds ... model_AS_8.rds   # Ashanti
│   ├── model_EA_1.rds ... model_EA_8.rds   # Eastern
│   ├── model_VO_1.rds ... model_VO_8.rds   # Volta
│   ├── model_GA_1.rds ... model_GA_8.rds   # Greater Accra
│   ├── model_CE_1.rds ... model_CE_8.rds   # Central
│   └── model_WE_1.rds ... model_WE_8.rds   # Western
│
└── www/
    ├── images/                  # Logos and figures
    └── markdown/                # Help, About, and Welcome page content
        ├── about_0.md
        ├── about_page_*.Rmd
        ├── welcome_page_*.Rmd
        ├── help_page_*.Rmd
        ├── summary_help_*.Rmd
        └── gam model building.md
```

---

## Installation & Running Locally

### Prerequisites

- R ≥ 4.5.2
- RStudio (recommended)

### Required R Packages

```r
install.packages(c(
  "shiny",
  "sf",
  "shinydashboard",
  "shinyjs",
  "shinyBS",
  "DT",
  "plotly",
  "ggplot2",
  "dplyr",
  "tidyr",
  "lubridate",
  "mgcv",
  "gratia",
  "gridExtra",
  "markdown",
  "htmltools",
  "tidyverse"
))
```
### Clone and Run

```bash
git clone https://github.com/EdwardAkurugu/GAM-MAP.git
cd GAM-MAP
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

This work was supported, in whole or in part, by the **Bill & Melinda Gates Foundation** [Grant number: INV047-048] and the **Malaria Modelling and Analytics: Leader in Africa (MMALA)** project through the Modelling and Simulation Hub, Africa (MASHA), University of Cape Town.

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
| Paul H.A.K. Abiwu | National Malaria Elimination Programme, Ghana Health Service |
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
             A Regional Generalised Additive Model analysis},
  journal = {PLOS ONE},
  year    = {2026},
  note    = {Under review}
}
```

---

## License

This project is released under a **Creative Commons Attribution (CC BY 4.0)** licence, as required by the Bill & Melinda Gates Foundation grant conditions.

---

## Acknowledgements

- [Modelling and Simulation Hub, Africa (MASHA)](https://masha.uct.ac.za), University of Cape Town
- [Clinton Health Access Initiative (CHAI)](https://www.clintonhealthaccess.org/south-africa/), South Africa
- [Southern African Development Community Elimination 8 (SADC E8)](https://sadce8.org/)
- [Ghana Health Service — National Malaria Elimination Programme](https://ghs.gov.gh/)
- [Ghana Meteorological Agency (GMeT)](https://www.meteo.gov.gh/)
