#' Marginal Scatter Plot with Histograms
#'
#' Builds a scatter plot with marginal histograms for the selected x and y
#' variables. Handles numeric, date/time, and factor/character axes gracefully.
#' Allows persistent highlighting of selected points. Missing values are
#' displayed beyond the axis range with a dotted separator line, custom "NA"
#' tick label, and a faint shaded rectangle (overlapping zones are darker).
#'
#' @param df Data frame containing the data to plot.
#' @param xvar Name of the variable to use on the x-axis.
#' @param yvar Name of the variable to use on the y-axis.
#' @param nbins_x Number of bins for top histogram.
#' @param nbins_y Number of bins for right histogram.
#' @param highlight_ids Optional vector of `.row_uid` values to highlight.
#' @param remove_ids Optional vector of `.row_uid` values to exclude.
#' @param colorvar Optional variable name for coloring points.
#'
#' @return A plotly object.
#' @keywords internal
make_marginal_scatter <- function(
  df,
  xvar,
  yvar,
  nbins_x = 30,
  nbins_y = 30,
  highlight_ids = NULL,
  remove_ids = NULL,
  colorvar = NULL
) {
  plot_df <- df
  if (!is.null(remove_ids) && length(remove_ids) > 0) {
    plot_df <- plot_df |> dplyr::filter(!.row_uid %in% remove_ids)
  }

  # ----------------------------------------------------------
  # NA imputation for plotting (plotting copy only, never raw data)
  # Returns list(col, line_pos, na_pos, has_na, na_label)
  #   line_pos : position of dotted separator line
  #   na_pos   : exact position where NA values are plotted (for tick label)
  # ----------------------------------------------------------
  .impute_na_for_plot <- function(col) {
    has_na <- anyNA(col)
    if (!has_na) {
      return(list(
        col = col,
        line_pos = NULL,
        na_pos = NULL,
        has_na = FALSE,
        na_label = NULL
      ))
    }

    if (is.numeric(col)) {
      rng <- range(col, na.rm = TRUE)
      span <- rng[2] - rng[1]
      if (span == 0) {
        span <- abs(rng[2]) * 0.1 + 1
      }
      na_pos <- rng[2] + 0.10 * span
      line_pos <- rng[2] + 0.05 * span
      col[is.na(col)] <- na_pos
      return(list(
        col = col,
        line_pos = line_pos,
        na_pos = na_pos,
        has_na = TRUE,
        na_label = "NA"
      ))
    } else if (inherits(col, c("Date", "POSIXct", "POSIXt"))) {
      rng <- range(col, na.rm = TRUE)
      span_sec <- as.numeric(difftime(rng[2], rng[1], units = "secs"))
      if (span_sec == 0) {
        span_sec <- 86400
      }
      if (inherits(col, "Date")) {
        na_pos <- rng[2] + ceiling(0.10 * as.numeric(rng[2] - rng[1]))
        line_pos <- rng[2] + ceiling(0.05 * as.numeric(rng[2] - rng[1]))
        class(na_pos) <- "Date"
        class(line_pos) <- "Date"
      } else {
        na_pos <- structure(
          rng[2] + 0.10 * span_sec,
          class = class(rng[2]),
          tzone = attr(rng[2], "tzone")
        )
        line_pos <- structure(
          rng[2] + 0.05 * span_sec,
          class = class(rng[2]),
          tzone = attr(rng[2], "tzone")
        )
      }
      col[is.na(col)] <- na_pos
      return(list(
        col = col,
        line_pos = line_pos,
        na_pos = na_pos,
        has_na = TRUE,
        na_label = "NA"
      ))
    } else if (is.factor(col) || is.character(col)) {
      col <- as.character(col)
      col[is.na(col)] <- "(NA)"
      col <- factor(col, levels = c(setdiff(unique(col), "(NA)"), "(NA)"))
      n_levels <- nlevels(col)
      line_pos <- n_levels - 0.5
      na_pos <- n_levels
      return(list(
        col = col,
        line_pos = line_pos,
        na_pos = na_pos,
        has_na = TRUE,
        na_label = "(NA)"
      ))
    }

    list(
      col = col,
      line_pos = NULL,
      na_pos = NULL,
      has_na = FALSE,
      na_label = NULL
    )
  }

  # Apply NA imputation to plotting copy
  x_imp <- .impute_na_for_plot(plot_df[[xvar]])
  y_imp <- .impute_na_for_plot(plot_df[[yvar]])
  plot_df[[xvar]] <- x_imp$col
  plot_df[[yvar]] <- y_imp$col

  # Keep character as factor
  if (is.character(plot_df[[xvar]])) {
    plot_df[[xvar]] <- as.factor(plot_df[[xvar]])
  }
  if (is.character(plot_df[[yvar]])) {
    plot_df[[yvar]] <- as.factor(plot_df[[yvar]])
  }

  # ----------------------------------------------------------
  # Build axis settings for scatter (xaxis3 / yaxis3)
  # Appends "NA" tick at na_pos for numeric axes.
  # Categorical axes already have "(NA)" as the last factor level.
  # ----------------------------------------------------------
  .build_na_axis <- function(imp, col_vals, title_text) {
    base <- list(title = list(text = title_text, standoff = 10))
    if (!imp$has_na || is.null(imp$na_pos)) {
      return(base)
    }

    if (is.numeric(col_vals)) {
      real_vals <- col_vals[col_vals < imp$line_pos]
      real_rng <- range(real_vals, na.rm = TRUE)
      auto_ticks <- pretty(real_rng, n = 5)
      auto_ticks <- auto_ticks[
        auto_ticks >= real_rng[1] & auto_ticks <= real_rng[2]
      ]
      tick_vals <- c(auto_ticks, imp$na_pos)
      tick_text <- c(as.character(auto_ticks), "NA")
      c(
        base,
        list(tickmode = "array", tickvals = tick_vals, ticktext = tick_text)
      )
    } else {
      base # categorical: factor levels already include "(NA)"
    }
  }

  # ----------------------------------------------------------
  # Shapes: dotted separator lines + shaded NA rectangles
  #
  # Three possible rectangles (all layer = "below"):
  #   1. X NA strip    : vertical band on right  (xref="x3", yref="paper")
  #   2. Y NA strip    : horizontal band on top  (xref="paper", yref="y3")
  #   3. Overlap corner: stacked on top of both  (xref="x3", yref="y3")
  #      doubles opacity naturally, making it visibly darker
  # ----------------------------------------------------------
  na_shapes <- list()
  na_fill <- "rgba(100, 100, 200, 0.08)"

  if (x_imp$has_na && !is.null(x_imp$line_pos)) {
    na_shapes[[length(na_shapes) + 1]] <- list(
      type = "rect",
      xref = "x3",
      yref = "paper",
      x0 = x_imp$line_pos,
      x1 = x_imp$na_pos * 1.05,
      y0 = 0,
      y1 = 1,
      fillcolor = na_fill,
      line = list(width = 0),
      layer = "below"
    )
    na_shapes[[length(na_shapes) + 1]] <- list(
      type = "line",
      xref = "x3",
      yref = "paper",
      x0 = x_imp$line_pos,
      x1 = x_imp$line_pos,
      y0 = 0,
      y1 = 1,
      line = list(color = "gray50", width = 1.5, dash = "dot")
    )
  }

  if (y_imp$has_na && !is.null(y_imp$line_pos)) {
    na_shapes[[length(na_shapes) + 1]] <- list(
      type = "rect",
      xref = "paper",
      yref = "y3",
      x0 = 0,
      x1 = 1,
      y0 = y_imp$line_pos,
      y1 = y_imp$na_pos * 1.05,
      fillcolor = na_fill,
      line = list(width = 0),
      layer = "below"
    )
    na_shapes[[length(na_shapes) + 1]] <- list(
      type = "line",
      xref = "paper",
      yref = "y3",
      x0 = 0,
      x1 = 1,
      y0 = y_imp$line_pos,
      y1 = y_imp$line_pos,
      line = list(color = "gray50", width = 1.5, dash = "dot")
    )
  }

  # Overlap corner: second rect stacked on top — doubles opacity
  if (
    x_imp$has_na &&
      !is.null(x_imp$line_pos) &&
      y_imp$has_na &&
      !is.null(y_imp$line_pos)
  ) {
    na_shapes[[length(na_shapes) + 1]] <- list(
      type = "rect",
      xref = "x3",
      yref = "y3",
      x0 = x_imp$line_pos,
      x1 = x_imp$na_pos * 1.05,
      y0 = y_imp$line_pos,
      y1 = y_imp$na_pos * 1.05,
      fillcolor = na_fill,
      line = list(width = 0),
      layer = "below"
    )
  }

  # ----------------------------------------------------------
  # Color handling and scatter creation
  # ----------------------------------------------------------
  scatter <- NULL

  if (!is.null(colorvar) && colorvar %in% names(df)) {
    vals <- df[[colorvar]]

    if (is.numeric(vals) || inherits(vals, c("Date", "POSIXct", "POSIXt"))) {
      numeric_vals <- if (is.numeric(vals)) vals else as.numeric(vals)
      rng_num <- range(numeric_vals, na.rm = TRUE)
      rng_label <- range(vals, na.rm = TRUE)

      scatter <- plotly::plot_ly(
        data = plot_df,
        x = ~ .data[[xvar]],
        y = ~ .data[[yvar]],
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 7,
          opacity = 0.7,
          color = numeric_vals,
          colorscale = list(c(0, "yellow"), c(0.5, "green"), c(1, "blue")),
          colorbar = list(
            title = colorvar,
            tickmode = "array",
            tickvals = rng_num,
            ticktext = format(rng_label, digits = 4),
            ticks = "outside"
          )
        ),
        customdata = ~.row_uid
      )
    } else if (is.factor(vals) || is.character(vals)) {
      plot_df[[colorvar]] <- as.factor(vals)
      nlev <- nlevels(plot_df[[colorvar]])

      if (nlev <= 10) {
        pal <- RColorBrewer::brewer.pal(max(3, nlev), "Set2")
        scatter <- plotly::plot_ly(showlegend = TRUE)

        for (i in seq_len(nlev)) {
          lev <- levels(plot_df[[colorvar]])[i]
          df_sub <- plot_df[plot_df[[colorvar]] == lev, ]
          scatter <- scatter |>
            plotly::add_trace(
              data = df_sub,
              x = ~ .data[[xvar]],
              y = ~ .data[[yvar]],
              type = "scatter",
              mode = "markers",
              marker = list(size = 7, opacity = 0.7, color = pal[i]),
              name = paste0(colorvar, ": ", lev),
              legendgroup = lev,
              showlegend = TRUE,
              customdata = ~.row_uid
            )
        }

        scatter <- scatter |>
          plotly::layout(
            showlegend = TRUE,
            legend = list(
              title = list(text = colorvar),
              orientation = "v",
              x = 1.02,
              y = 1,
              xanchor = "left",
              yanchor = "top",
              font = list(size = 11)
            )
          )
      } else {
        shiny::showNotification(
          "No legend for more than 10 discrete values",
          type = "warning"
        )
        scatter <- plotly::plot_ly(
          data = plot_df,
          x = ~ .data[[xvar]],
          y = ~ .data[[yvar]],
          type = "scatter",
          mode = "markers",
          marker = list(size = 7, opacity = 0.7, color = "gray"),
          customdata = ~.row_uid
        ) |>
          plotly::layout(
            annotations = list(list(
              x = 1.02,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              text = "(No legend for more than 10 discrete values)",
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "middle",
              font = list(size = 11, color = "gray40")
            )),
            showlegend = FALSE
          )
      }
    }
  } else {
    scatter <- plotly::plot_ly(
      data = plot_df,
      x = ~ .data[[xvar]],
      y = ~ .data[[yvar]],
      type = "scatter",
      mode = "markers",
      marker = list(size = 7, opacity = 0.7, color = "gray"),
      customdata = ~.row_uid
    )
  }

  # === Highlights ===
  if (!is.null(highlight_ids) && length(highlight_ids) > 0) {
    hi <- plot_df |> dplyr::filter(.row_uid %in% highlight_ids)
    if (nrow(hi) > 0) {
      scatter <- scatter |>
        plotly::add_trace(
          data = hi,
          x = ~ .data[[xvar]],
          y = ~ .data[[yvar]],
          type = "scatter",
          mode = "markers",
          marker = list(size = 7, color = "red", opacity = 0.9),
          inherit = FALSE,
          showlegend = FALSE
        )
    }
  }

  # ----------------------------------------------------------
  # Marginal histograms (numeric axes only)
  # ----------------------------------------------------------
  x_is_num <- is.numeric(plot_df[[xvar]])
  y_is_num <- is.numeric(plot_df[[yvar]])

  if (x_is_num) {
    xbins <- cut(plot_df[[xvar]], breaks = nbins_x, include.lowest = TRUE)
    xhist <- plot_df |>
      dplyr::mutate(bin = xbins) |>
      dplyr::group_by(bin) |>
      dplyr::summarise(
        count = dplyr::n(),
        ids = list(.row_uid),
        center = mean(.data[[xvar]], na.rm = TRUE),
        .groups = "drop"
      )
    top_hist <- plotly::plot_ly(
      data = xhist,
      x = ~center,
      y = ~count,
      type = "bar",
      customdata = ~ids,
      source = "scatterplot",
      marker = list(
        color = "lightgray",
        line = list(color = "darkgray", width = 1)
      )
    ) |>
      plotly::layout(
        xaxis = list(
          showticklabels = FALSE,
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          showticklabels = FALSE,
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        )
      )
  } else {
    top_hist <- plotly::plotly_empty()
  }

  if (y_is_num) {
    ybins <- cut(plot_df[[yvar]], breaks = nbins_y, include.lowest = TRUE)
    yhist <- plot_df |>
      dplyr::mutate(bin = ybins) |>
      dplyr::group_by(bin) |>
      dplyr::summarise(
        count = dplyr::n(),
        ids = list(.row_uid),
        center = mean(.data[[yvar]], na.rm = TRUE),
        .groups = "drop"
      )
    right_hist <- plotly::plot_ly(
      data = yhist,
      x = ~count,
      y = ~center,
      type = "bar",
      orientation = "h",
      customdata = ~ids,
      source = "scatterplot",
      marker = list(
        color = "lightgray",
        line = list(color = "darkgray", width = 1)
      )
    ) |>
      plotly::layout(
        xaxis = list(
          showticklabels = FALSE,
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          showticklabels = FALSE,
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        )
      )
  } else {
    right_hist <- plotly::plotly_empty()
  }

  # ----------------------------------------------------------
  # Subplot layout
  # Slot 1 = top_hist   -> xaxis,  yaxis
  # Slot 2 = empty      -> xaxis2, yaxis2
  # Slot 3 = scatter    -> xaxis3, yaxis3  <-- titles + NA ticks + shapes
  # Slot 4 = right_hist -> xaxis4, yaxis4
  # ----------------------------------------------------------
  x_axis_settings <- .build_na_axis(x_imp, plot_df[[xvar]], xvar)
  y_axis_settings <- .build_na_axis(y_imp, plot_df[[yvar]], yvar)

  p <- plotly::subplot(
    top_hist,
    plotly::plotly_empty(),
    scatter,
    right_hist,
    nrows = 2,
    shareX = FALSE,
    shareY = FALSE,
    widths = c(0.8, 0.2),
    heights = c(0.2, 0.8),
    which_layout = "merge"
  ) |>
    plotly::layout(
      dragmode = "select",
      shapes = if (length(na_shapes) > 0) na_shapes else NULL,
      xaxis3 = x_axis_settings,
      yaxis3 = y_axis_settings
    ) |>
    plotly::config(displaylogo = FALSE)

  p$x$source <- "scatterplot"
  p <- p |>
    plotly::event_register("plotly_click") |>
    plotly::event_register("plotly_selected")
  p
}
