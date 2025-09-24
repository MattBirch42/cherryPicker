# Cherry Picker

Cherry Picker is a lightweight R package that provides both a helper function and a Shiny application for interactively exploring and filtering tabular data.  
It was designed to make it easy to isolate specific points (select rows/columns) from CSV or Parquet files and export the results.

---

## Features

- **Shiny App**: Launch the Cherry Picker app to interactively explore data.
- **File Support**: Upload `.csv` and `.parquet` files directly into the app.
- **Filtering Tools**: Optional filtering buttons to subset the dataset.
- **Packaging**: Only the main user-facing function (`cherry_picker()`) is exported, keeping internals minimal.

---

## Installation

You can install the development version directly from GitHub:

```r
# install.packages("devtools")
devtools::install_github("mattbirch42/cherryPicker")
