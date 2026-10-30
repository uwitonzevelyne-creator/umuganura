<p align="center">
  <img src="www/hero/logo.png" alt="Umuganura logo" width="120" />
</p>

<h1 align="center">Umuganura</h1>

<p align="center">
  <strong>Rwanda's post-harvest loss platform — from harvest tonnes to francs lost and action plans.</strong>
</p>

<p align="center">
  <a href="https://uwitonzeevelyne.shinyapps.io/umuganura/"><img src="https://img.shields.io/badge/Live%20app-shinyapps.io-success?style=for-the-badge" alt="Live app" /></a>
  <img src="https://img.shields.io/badge/R-4.5+-276DC3?style=for-the-badge&logo=r&logoColor=white" alt="R 4.5+" />
  <img src="https://img.shields.io/badge/Shiny-Posit-178BCA?style=for-the-badge" alt="Shiny" />
  <img src="https://img.shields.io/badge/License-MIT-yellow?style=for-the-badge" alt="MIT License" />
</p>

<p align="center">
  <img src="www/ikigega/basket.png" alt="Ikigega basket motif" width="72" />
</p>

---

## Table of Contents

- [About Umuganura](#about-umuganura)
- [Key Features](#key-features)
- [Technology Stack](#technology-stack)
- [Getting Started](#getting-started)
- [Platform Modules](#platform-modules)
- [Data Sources](#data-sources)
- [Screenshots](#screenshots)
- [Project Structure](#project-structure)
- [Contributing](#contributing)
- [License](#license)
- [Contact](#contact)

---

## About Umuganura

**Umuganura** turns **Umusaruro**, **Isoko**, and **Ikigega** into one clear story: what Rwanda harvests, what markets pay, what is stored, how much is lost after the field, who pays in francs, and what farmers or ministries can do next.

The platform is built for **data integrity** — harvest, price, and store figures stay tied to loaded national tables and external series. Charts do not invent a national loss percentage to fill a gap.

**Live demo:** [https://uwitonzeevelyne.shinyapps.io/umuganura/](https://uwitonzeevelyne.shinyapps.io/umuganura/)

---

## Key Features

- **Umusaruro** — Season production by district and crop, yield against history, and map-first exploration of Rwanda's harvest.
- **Isoko** — Farm-gate and market prices across NAEB markets, linked to macro factors (inflation, GDP, jobs, exchange rates, credit).
- **Ikigega** — National stocks, food-balance residuals, and cooperative storage signals on the map.
- **Ejo Hazaza** — Six post-harvest loss causes, money lost and regained, operations-research routing, institutional desk, and action-plan analytics.
- **Farm calculator** — Cause-specific inputs, RWF loss charts, and farmer-facing outputs (English / Kinyarwanda).
- **About** — Mission, values, and team story aligned with the live product narrative.

---

## Technology Stack

| Layer | Tools |
| --- | --- |
| **App** | [R Shiny](https://shiny.posit.co/), `ggplot2`, `plotly`, `leaflet` |
| **Data** | NISR SAS tables, FAO / World Bank series, NAEB prices, APHLIS, NASA POWER, OSRM matrices |
| **Front-end** | Custom CSS/JS (`www/css`, `www/js`), institutional portal and action-plan UI |
| **Deploy** | [Posit Connect / shinyapps.io](https://www.shinyapps.io/) via `rsconnect`, `.rscignore`, `data/prebuilt/` RDS cache |
| **Reproducibility** | `renv.lock` (optional local `renv` restore) |

---

## Getting Started

### Prerequisites

- **R** ≥ 4.5 (see `renv.lock`)
- Core packages: `shiny`, `ggplot2`, `plotly`, `jsonlite` (plus `leaflet`, `lpSolve`, `data.table`, and others as you open modules)

### Run locally

```bash
git clone https://github.com/uwitonzevelyne-creator/umuganura.git
cd umuganura
```

In R:

```r
# Optional: reproducible library
if (file.exists("renv.lock") && requireNamespace("renv", quietly = TRUE)) {
  renv::restore()
}

# Smoke-check sources load (same check used before deploy)
system("Rscript tools_smoke_boot.R")

# Run the app (app.R is the entry file at repo root)
shiny::runApp(".")
```

### Deploy to shinyapps.io

With `rsconnect` configured for your account:

```r
Rscript deploy_shinyapps.R
```

The script runs `tools_smoke_boot.R` first, then publishes **`appName = umuganura`**.

---

## Platform Modules

| Module | Kinyarwanda | Role |
| --- | --- | --- |
| **Umusaruro** | Umusaruro | What left the field — production, districts, crops, seasons |
| **Isoko** | Isoko | Prices and macro bridges that turn tonnes into francs |
| **Ikigega** | Ikigega | Storage, reserves, and balance-implied loss between harvest and plate |
| **Ejo Hazaza** | Ejo Hazaza | Cause mix, forecasts, OR plans, institutional workflow |
| **Amakuru / About** | — | Narrative, team, and navigation into the rooms above |

---

## Data Sources

Cleaned datasets shipped in this repo support the live app:

- **NISR** — Seasonal Agricultural Survey (SAS) district tables (`dataset hachthon/CLEANED/nisr_sas_tables/`)
- **Master Rwanda ag** — World Bank, FAO production/prices/balances, climate (`dataset hachthon/CLEANED/master_rwanda_ag/`)
- **NAEB** — Market price panels and cleaned product/location maps (`dataset hachthon/RWANDA_INSTITUTIONS/NAEB/`)
- **Ejo Hazaza external** — APHLIS losses, OSRM routes/matrices, NASA POWER, FBS historic (`dataset hachthon/EXTERNAL/ejo_hazaza/`)
- **Prebuilt cache** — `data/prebuilt/*.rds` for faster cold starts on shinyapps.io

Institutional export panel fallback: `dataset hachthon/CLEANED/rwanda_institutions/naeb_export_performance_panel.csv`.

---

## Screenshots

_Screenshots and a walkthrough video will be added soon._

<!-- Placeholder: replace with GIF/PNG from the live app when ready -->

| Home | Ejo Hazaza |
| --- | --- |
| *Coming soon* | *Coming soon* |

---

## Project Structure

```
umuganura/
├── app.R                 # Shiny entry point
├── global.R              # Data load, indicators, geo
├── interface.R           # UI shell and assets
├── analysis.R            # Server wiring
├── umusaruro.R / isoko (analysis) / ikigega.R / ejo*.R
├── startup_cache.R       # prebuilt + local cache
├── data/prebuilt/        # Deploy-friendly RDS
├── dataset hachthon/     # Cleaned CSVs for production
├── www/                  # css, js, hero, cards, ejo, about, geo
├── deploy_shinyapps.R
├── tools_smoke_boot.R
├── tools_build_prebuilt.R
├── .rscignore
├── renv.lock
└── README.md
```

---

## Contributing

Contributions that improve data quality, accessibility, or farmer-facing clarity are welcome.

1. Fork the repository.
2. Create a branch for your change.
3. Run `Rscript tools_smoke_boot.R` before opening a pull request.
4. Open a PR with a short description of the data or UX impact.

Please do not commit secrets, `.env` files, or personal `rsconnect` tokens.

---

## License

This project is licensed under the **MIT License** — see [LICENSE](LICENSE).

---

## Contact

- **GitHub:** [@uwitonzevelyne-creator](https://github.com/uwitonzevelyne-creator)
- **Team**
  - **Uwitonze Evelyne** — Data Analyst (University of Rwanda, Actuarial Science)
  - **Ishimwe Gloria** — Data Engineer (University of Rwanda, Economic Statistics)

For questions about the live deployment, use the shinyapps.io app link above or open a GitHub issue on this repository.
