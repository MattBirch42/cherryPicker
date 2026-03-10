#' Generate Fake Hourly Dataset
#'
#' Creates a synthetic hourly dataset with realistic temporal, numeric, and
#' categorical variables. Includes seasonal and diurnal temperature patterns,
#' linear and parabolic trends, random noise, and injected missing values and
#' outliers for testing data cleaning and visualization tools.
#'
#' @param N Number of hourly rows to generate. Default = 20000.
#' @param seed Random seed for reproducibility. Default = 1234.
#' @return Invisibly returns the generated tibble, but also assigns it as
#'   `fake_data` in the global environment.
#' @export
#' @examples
#' fake_data()
#' head(fake_data)
fake_data <- function(N = 20000, seed = 42) {
  library(dplyr)
  library(lubridate)
  library(tidyr)

  set.seed(seed)
  start_time <- ymd_hms("2020-01-01 00:00:00", tz = "UTC")

  df <- tibble(
    timestamp_utc = start_time + hours(0:(N - 1)),
    date = as.Date(start_time + hours(0:(N - 1))),
    year = year(start_time + hours(0:(N - 1))),
    month = month(start_time + hours(0:(N - 1))),
    day = day(start_time + hours(0:(N - 1))),
    hour_begin = hour(start_time + hours(0:(N - 1))),
    weekday = as.character(wday(
      start_time + hours(0:(N - 1)),
      label = TRUE,
      abbr = TRUE
    ))
  )

  # --- Temperature pattern ---
  season_effect <- 50 + 50 * sin(2 * pi * (yday(df$date) / 365 - 0.25))
  daily_effect <- 20 * sin(2 * pi * (df$hour_begin - 4) / 24)

  df <- df %>%
    mutate(
      temp_avg = pmax(
        0,
        pmin(100, season_effect + daily_effect + rnorm(N, sd = 5))
      ),
      linear = 42 + 0.02 * (1:N) + rnorm(N, 0, 2),
      smile = 100 - 150 * (seq(-1, 1, length.out = N)^2) + rnorm(N, 0, 5),
      noise = runif(N, 0, 80),
      noise2 = (noise - 20)^2,
      little_string = sample(
        c("oranges", "cat", "economics", "widget"),
        N,
        TRUE
      ),
      big_string = sample(
        apply(expand.grid(LETTERS, LETTERS), 1, paste0, collapse = ""),
        N,
        TRUE
      ),
      daily_thing = 5 * as.numeric(hour_begin) + rnorm(N, 0, 3)
    )

  # --- Inject missing values (~1/1000 rows) ---
  rows_na <- sample(seq_len(N), N / 1000)
  cols_na <- sample(
    setdiff(names(df), c("timestamp_utc", "date", "weekday")),
    length(rows_na),
    replace = TRUE
  )
  for (i in seq_along(rows_na)) {
    col <- cols_na[i]
    if (!is.integer(df[[col]])) df[[col]][rows_na[i]] <- NA
  }

  # --- Inject outliers (~1/250 rows) ---
  rows_out <- sample(seq_len(N), N / 250)
  cols_out <- sample(
    setdiff(names(df), c("timestamp_utc", "date", "weekday")),
    length(rows_out),
    replace = TRUE
  )
  for (i in seq_along(rows_out)) {
    col <- cols_out[i]
    val <- df[[col]][rows_out[i]]
    if (is.numeric(val) && !is.integer(val)) {
      df[[col]][rows_out[i]] <- val * runif(1, 10, 100)
    } else if (inherits(val, c("Date", "POSIXct"))) {
      df[[col]][rows_out[i]] <- val + days(sample(c(-3650, 3650), 1))
    } else if (is.character(val)) {
      df[[col]][rows_out[i]] <- paste0(
        "OUTLIER_",
        paste0(sample(letters, 6, TRUE), collapse = "")
      )
    }
  }

  # --- Assign and return ---
  assign("fake_data", df, envir = .GlobalEnv)
  invisible(df)
}
