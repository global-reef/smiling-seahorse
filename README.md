
# 🦈 Smiling Seahorse Trip Report Scraper

This repository contains a web scraper for extracting shark and ray sightings from [The Smiling Seahorse](https://www.thesmilingseahorse.com/blog) liveaboard trip reports.

## 📦 Features

- Uses Selenium to handle popups and render JS-loaded blog content
- Detects trip start dates, dive sites, and "Day X" headers
- Fuzzy matches dive site names
- Extracts individual shark/ray mentions with contextual excerpts
- Exports a clean dataset as CSV

## 🧪 Example Output

| species        | sighting_date | dive_site   | excerpt                         |
|----------------|----------------|--------------|----------------------------------|
| whale shark    | 2025-01-23     | Roe Bank    | "...we spotted a whale shark..."|
| oceanic manta  | 2025-01-24     | Roe Bank    | "...an oceanic manta glided by..."|

## 🔧 How to Run

1. Clone the repository
2. Install dependencies:

```bash
pip install -r requirements.txt
