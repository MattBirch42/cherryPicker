# Cherry Picker

Cherry Picker is a lightweight R package that combines a helper function and an interactive Shiny application for exploring and filtering tabular data.
It’s designed to make it fast and intuitive to spot outliers, highlight interesting subsets, and visualize data patterns without writing extra code.

## Features

Interactive Shiny App – Launch a click-based interface to explore data in real time.

Outlier & Anomaly Detection Made Simple – Click directly on scatterplots to highlight unusual points or isolate specific rows.

Flexible File Support – Upload .csv or .parquet files with ease.

Filtering & Subsetting – Apply filters, clear them, or iteratively remove exported points to refine your dataset.

Export Options – Save selected points to CSV or directly into your R session (with optional annotations) for further analysis.

Minimalist Packaging – Only the user-facing cherry_picker() function is exported, keeping internals clean and lightweight.

## Installation

Install the development version directly from GitHub:

# install.packages("devtools")
devtools::install_github("mattbirch42/cherryPicker")
