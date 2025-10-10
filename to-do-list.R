
# 
# Aggregation Functions
# 
# aggregate_duckdb(): Runs aggregation queries (mean, min, max, slice_min, slice_max, etc.) dynamically using DuckDB.
# Parameters: data, group_by, metrics, functions.
# Returns a new aggregated table and appends metadata (aggregation recipe, timestamp).
# 
# list_aggregators(): Helper that returns available aggregation types (e.g., c("mean", "min", "max", "slice_min", "slice_max")) for UI dropdowns.
# 
# Outlier Detection Functions
# 
# auto_detect_outliers(): Wrapper that chooses detection method based on user input.
# 
# flag_outliers(): Adds .outlier_flag column and stores threshold + method metadata.
# 
# Imputation Functions
# 
# impute_selected_points(): Given selected row indices, replaces missing/outlier values with imputed ones.
# Methods: mean, median, regression, KNN.
# Optionally records which columns were imputed.
# 
# list_imputation_methods(): Returns available imputation options for UI dropdown.
# 
# Statistics Functions
# 
# generate_statistics(): Returns summary stats (mean, sd, quantiles) and relational stats (correlation, t-tests, etc.) for:
#   Full data
# Data without selected points
# Data with imputed selected points
# 
# export_statistics_table(): Formats results for CSV, display, or embedding in S3 object.
# 
# Linear Model Functions
# 
# run_lm_duckdb(): Runs regression models in DuckDB for the three variants:
#   Full dataset
# Without selected points
# With selected points imputed
# Returns coefficients, fitteds, residuals, and summary stats.
# 
# extract_lm_diagnostics(): Converts DuckDB lm() results to tidy tibble format with residuals and fitted values.
# 
# Plot Conversion Functions
# 
# convert_plotly_to_ggplot(): Given a plotly object, returns a ggplot2 object for static export.
# Converts axis labels, colors, and point mappings.
# Used for PDF or RMarkdown output.
# 
# Session Construction Functions
# 
# build_cherry_session(): Creates the top-level S3 object (cherryPickerSession) containing all data, models, stats, plots, and metadata.
# 
# append_to_session(): Adds new data, plots, or models to an existing session object.
# 
# rehydrate_cherry_session(): Reloads an S3 session into the app (cherry_picker(session = x)).
# 
# PHASE 2 — Application Tabs (Modules)
# Inputs
# 
# Accept 3 types:
#   No input → file upload UI
# Data frame / table input
# S3 session input (restores everything)
# 
# Main Tab: "Data Exploration"
# 
# Interactive scatter plot (Plotly)
# 
# Point-selection tools: lasso, rectangle, click
# 
# Automatic Outlier Detection:
#   Button to run chosen detection method (z-score, IQR, Mahalanobis)
# Threshold slider for z-score (2–5 range)
# Highlight outliers automatically
# 
# Actions on selected points:
#   Keep
# Remove / exclude
# Impute (choose variables + method)
# 
# Export plots and data
# 
# Generate statistics (distribution + relational)
# Runs 3 variants: full / excluded / imputed
# Saves to session object
# 
# Aggregation Tab (DuckDB)
# 
# Point-and-click interface for groupings and aggregation functions
# 
# Returns new aggregated table and updates session object
# 
# Export aggregated results to CSV/parquet
# 
# Regression / Modeling Tab
# 
# Select dependent & independent variables
# 
# Runs 3 regressions: full / excluded / imputed
# 
# Displays coefficients, residuals, fitteds
# 
# Linked residual/fitted plot to main scatter
# 
# Exports diagnostics into session object
# 
# Additional Machine Learning Tab (Extension-Enabled)
# 
# CRAN version: static info panel + GitHub link
# 
# GitHub version: interactive interface using duckdb_ml for:
#   Logistic regression, KNN, clustering, PCA, random forest (if supported)
# 
# Stores results in session object
# 
# Behind the Scenes
# 
# Everything stored in a top-level S3 object cherryPickerSession with:
#   $tables (full / excluded / imputed)
# $models (full / excluded / imputed)
# $stats (full / excluded / imputed)
# $plots (plotly + ggplot versions)
# $metadata (thresholds, imputation methods, UI state, timestamps)
# 
# App either returns the session directly or saves .rds file
# 
# Rehydration supported via cherry_picker(session = saved_session)
# 
# PHASE 3 — Modularization
# 
# Each tab has its own mod_*.R file with both UI and server functions:
#   mod_data_explore_ui() / server()
# mod_aggregate_ui() / server()
# mod_modeling_ui() / server()
# mod_ml_ui() / server()
# 
# session_store <- reactiveValues() bridges modules and holds shared data.
# 
# Each module reads/writes to session_store keys:
#   session_store$data_full
# session_store$outlier_flags
# session_store$models
# session_store$stats
# session_store$plots
# session_store$metadata
# 
# PHASE 4 — CRAN Readiness Summary
# CRAN-safe practices:
#   
#   Never write to .GlobalEnv
# 
# Never auto-launch app
# 
# Wrap examples in if (interactive())
#   
#   Only write to tempfile() or via user download
# 
# No remote or GitHub dependencies in Imports
# 
# Use requireNamespace() for optional ML extensions
# 
# Document all datasets and functions with @export, @examples, and @param
# 
# Keep embedded data < 5 MB
# 
# Provide one main export (cherry_picker())
# 
# Run devtools::check() and rhub::check_for_cran() clean
# 
# Include NEWS.md, LICENSE, vignette, small demo dataset
# 
# PHASE 5 — Unit Test Coverage (≈15 tests)
# You’ll use testthat::test_that() and expect_*() for each function.
# 
# Category and Unit Tests:
#   Data Conversion
# 
# convert_to_tbl() converts ts, xts, matrix → tibble with same row count.
# 
# All columns preserved and classed correctly.
# 
# Aggregation
# 3. Aggregation returns correct group counts.
# 4. slice_max works as expected.
# 
# Outlier Detection
# 5. detect_outliers_zscore() flags rows with z > threshold.
# 6. User threshold changes output predictably.
# 
# Imputation
# 7. Mean/median methods replace NAs.
# 8. Only selected columns imputed.
# 
# Statistics
# 9. generate_statistics() returns 3 variants (full/excluded/imputed).
# 10. Correlation results consistent with base cor().
# 
# LM Models
# 11. run_lm_duckdb() returns coefficients matching base lm() (within tolerance).
# 12. Residuals sum ~ 0.
# 
# Plot Conversion
# 13. convert_plotly_to_ggplot() returns valid ggplot object.
# 14. Axis labels preserved.
# 
# Session Object
# 15. build_cherry_session() produces class "cherryPickerSession" and contains all top-level slots.
# 16. Rehydrating restores same metadata and model count.
# 
# UI/Server Integration
# 17. (optional) Simulate app with shiny::testServer() to verify modules update shared session_store.
# 
# Note: CRAN requires tests to run fast and without Shiny UI; use mock datasets and skip heavy computation with testthat::skip_if_not(interactive()) where needed.
# 
# Cherry Picker Pro Addendum
# 
# CherryPickerPro will list CherryPicker as a dependency (Depends: cherryPicker (>= 1.0.0)).
# 
# It will import Cherry Picker’s internal functions (via :::) such as aggregator, outlier, imputation, and modular UI/server components.
# 
# It will add only the ML extension tab, the DuckDB ML support, and the expanded UI.
# 
# CherryPickerPro will be its own GitHub-only package and will not be submitted to CRAN.
# 
# CRAN version remains the stable base package; Pro builds on it to enable advanced modeling features.