### 00_RUN.R ####
### Purpose: Set analysis date, create output folders, define shared helpers, then run pipeline scripts

### 00. SETUP ####

library(tidyverse)
library(ggplot2)

analysis_date <- format(Sys.Date(), "%Y_%m_%d")

# fixed relative paths (assumes repo root is the working directory)
wd0 <- getwd()
if (basename(wd0) == "analysis") setwd("..")

analysis_dir <- "analysis"
dir.create(analysis_dir, showWarnings = FALSE, recursive = TRUE)

# outputs live inside analysis/
output_dir  <- file.path(analysis_dir, paste0("Analysis_", analysis_date))
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

fits_dir    <- file.path(output_dir, "fits")
plots_dir   <- file.path(output_dir, "plots")
stats_dir   <- file.path(output_dir, "stats")
summ_dir    <- file.path(output_dir, "summaries")
eda_dir     <- file.path(output_dir, "eda")
tables_dir  <- file.path(output_dir, "tables")

dir.create(fits_dir,   showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(stats_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(summ_dir,   showWarnings = FALSE, recursive = TRUE)
dir.create(eda_dir,    showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

# run log
writeLines(
  c(
    paste0("analysis_date: ", analysis_date),
    paste0("wd: ", normalizePath(getwd(), winslash = "/")),
    paste0("analysis_dir: ", normalizePath(analysis_dir, winslash = "/", mustWork = FALSE)),
    paste0("output_dir: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE)),
    paste0("run_time: ", Sys.time())
  ),
  con = file.path(output_dir, "run_log.txt")
)

### 01. GLOBAL THEMES + COLOURS ####

theme_clean <- theme_minimal(base_family = "Arial") +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank()
  )

country_cols <- c("Myanmar" = "#007A87", "Thailand" = "#66BFA6")
elasmo_cols  <- c("shark" = "#007A87", "ray" = "#66BFA6")

### 02. HELPERS ####

format_p <- function(p) ifelse(p < 0.001, "<0.001",
                               formatC(p, format = "f", digits = 3))

save_obj <- function(x, filename, dir = summ_dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(x, file.path(dir, filename))
  invisible(TRUE)
}

brms_fixef_export <- function(fit, model_name, out_dir = tables_dir, sigfigs = 3) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  fx <- as.data.frame(brms::fixef(fit, summary = TRUE))
  fx_out <- tibble(
    Effect   = rownames(fx),
    Estimate = signif(fx$Estimate, sigfigs),
    SE       = signif(fx$Est.Error, sigfigs),
    CI       = paste0("[", signif(fx$Q2.5, sigfigs), ", ", signif(fx$Q97.5, sigfigs), "]")
  )
  
  write_csv(fx_out, file.path(out_dir, paste0(model_name, "_fixef.csv")))
  invisible(fx_out)
}

### 03. RUN PIPELINE ####
# NOTE:
# - Each sourced script should rely on the shared objects above (analysis_date, output_dir, *_dir, theme_clean).
# - Keep scripts writing outputs into these folders rather than the project root.

source(file.path("analysis", "01_CLEAN.R"))
source(file.path("analysis", "02_EXPLORE.R"))
source(file.path("analysis", "03_MODEL.R"))
source(file.path("analysis", "04_SPP_MODELS.R"))

### 04. END ####

message("Run complete: ", analysis_date)
message("Outputs saved to: ", output_dir)
