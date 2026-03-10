#' Marginal Scatter Plot with Histograms
#'
#' Builds a scatter plot with marginal histograms for the selected x and y
#' variables. Handles numeric, date/time, and factor/character axes gracefully.
#' Allows persistent highlighting of selected points.
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
    plot_df <- plot_df %>% dplyr::filter(!.row_uid %in% remove_ids)
  }

  # Keep factor variables as categorical
  if (is.character(plot_df[[xvar]])) {
    plot_df[[xvar]] <- as.factor(plot_df[[xvar]])
  }
  if (is.character(plot_df[[yvar]])) {
    plot_df[[yvar]] <- as.factor(plot_df[[yvar]])
  }

  # --- Color handling and scatter creation ---
  scatter <- NULL

  if (!is.null(colorvar) && colorvar %in% names(df)) {
    vals <- df[[colorvar]]

    # === Continuous numeric / date / time ===
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
      # === Discrete factor/character ===
      plot_df[[colorvar]] <- as.factor(vals)
      nlev <- nlevels(plot_df[[colorvar]])

      if (nlev <= 10) {
        pal <- RColorBrewer::brewer.pal(max(3, nlev), "Set2")
        scatter <- plotly::plot_ly(showlegend = TRUE)

        for (i in seq_len(nlev)) {
          lev <- levels(plot_df[[colorvar]])[i]
          df_sub <- plot_df[plot_df[[colorvar]] == lev, ]

          scatter <- scatter %>%
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

        scatter <- scatter %>%
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
        ) %>%
          plotly::layout(
            annotations = list(
              x = 1.02,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              text = "(No legend for more than 10 discrete values)",
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "middle",
              font = list(size = 11, color = "gray40")
            ),
            showlegend = FALSE
          )
      }
    }
  } else {
    # === Default (no color variable) ===
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
    hi <- plot_df %>% dplyr::filter(.row_uid %in% highlight_ids)
    if (nrow(hi) > 0) {
      scatter <- scatter %>%
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

  # === Marginal histograms only for numeric axes ===
  x_is_num <- is.numeric(df[[xvar]])
  y_is_num <- is.numeric(df[[yvar]])

  if (x_is_num) {
    xbins <- cut(plot_df[[xvar]], breaks = nbins_x, include.lowest = TRUE)
    xhist <- plot_df %>%
      dplyr::mutate(bin = xbins) %>%
      dplyr::group_by(bin) %>%
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
    ) %>%
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
    yhist <- plot_df %>%
      dplyr::mutate(bin = ybins) %>%
      dplyr::group_by(bin) %>%
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
    ) %>%
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

  # === Combine ===
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
  ) %>%
    plotly::layout(
      dragmode = "select",
      xaxis = list(title = xvar),
      yaxis2 = list(title = yvar)
    ) %>%
    plotly::config(displaylogo = FALSE)

  p$x$source <- "scatterplot"
  p <- p %>%
    plotly::event_register("plotly_click") %>%
    plotly::event_register("plotly_selected")
  p
}
