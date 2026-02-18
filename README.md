# 🦈 Smiling Seahorse Trip Report Scraper

This repository contains a full scraping, validation, and Bayesian
analysis pipeline for extracting and analysing shark and ray sightings
from The Smiling Seahorse liveaboard trip reports.

------------------------------------------------------------------------

## 📦 Features

### 🕷 Scraper

-   Uses Selenium to handle popups and render JS-loaded blog content
-   Detects trip start dates, dive sites, and Day X headers
-   Fuzzy matches dive site names
-   Extracts individual shark and ray mentions with contextual excerpts
-   Exports a structured sightings dataset as CSV

------------------------------------------------------------------------

### ✅ Validation App

The repository includes `validator_app.py`, a lightweight Streamlit
interface to manually validate extracted sightings.

Purpose:

-   Confirm species identification
-   Verify dive site and sighting date
-   Review contextual excerpts
-   Export a validated dataset for modelling

Run with:

    streamlit run validator_app.py

------------------------------------------------------------------------

## 📊 Analysis Pipeline

The `analysis/` folder contains structured R scripts:

-   `00_RUN.R` -- Master script to run the full pipeline
-   `01_CLEAN.R` -- Data cleaning and harmonisation
-   `02_EXPLORE.R` -- Exploratory data analysis
-   `03_MODEL.R` -- Bayesian hierarchical models (trip, group, species)
-   `04_SPP_MODELS.R` -- Species-level trend extraction and
    visualisation

Supporting directories:

-   `data_raw/` -- Raw scraped data
-   `data_clean/` -- Cleaned datasets
-   `figures/` -- Model figures
-   `reports/` -- Summary outputs
-   `outputs/` -- Model objects and posterior summaries

------------------------------------------------------------------------

## 🧠 Modelling Framework

The analysis uses Bayesian negative binomial hierarchical models (via
`brms`) to evaluate:

-   Temporal trends in encounter rates
-   Differences between Myanmar and Thailand
-   Group-level trends (sharks vs rays)
-   Species-specific trajectories through time

Models incorporate partial pooling across:

-   Regions
-   Trips
-   Species

------------------------------------------------------------------------

## 🧪 Example Output

  ------------------------------------------------------------------------
  scientific_name              sighting_date     dive_site     excerpt
  ---------------------------- ----------------- ------------- -----------
  Rhincodon typus              2025-01-23        Roe Bank      "...we
                                                               spotted a
                                                               whale
                                                               shark..."

  Mobula birostris             2025-01-24        Roe Bank      "...an
                                                               oceanic
                                                               manta
                                                               glided
                                                               by..."
  ------------------------------------------------------------------------

------------------------------------------------------------------------

## 🔧 How to Run

### 1️⃣ Run the Scraper

    pip install -r requirements.txt
    python scraper.py

### 2️⃣ Validate Sightings

    streamlit run validator_app.py

### 3️⃣ Run the Analysis

Open the `analysis/analysis.Rproj` file in RStudio and run:

    00_RUN.R

Or execute scripts sequentially:

    01_CLEAN.R
    02_EXPLORE.R
    03_MODEL.R
    04_SPP_MODELS.R

------------------------------------------------------------------------

## 🎯 Project Goal

This project uses dive tourism records as a structured citizen-science
dataset to evaluate long-term trends in elasmobranch encounter rates in
data-poor regions of Myanmar and Thailand.

By combining automated scraping, manual validation, and hierarchical
modelling, the workflow enables reproducible ecological inference from
narrative dive logs.
