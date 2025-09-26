#' Marginal Scatter Plot with Histograms
#'
#' Internal helper for Cherry Picker. Builds a scatter plot with marginal
#' histograms for the selected x and y variables. Allows persistent highlighting
#' of selected points.
#'
#' @param df Data frame containing the data to plot.
#' @param xvar Name of the variable to use on the x-axis.
#' @param yvar Name of the variable to use on the y-axis.
#' @param highlight_ids Optional vector of `.row_uid` values to highlight in red.
#' @param remove_ids Optional vector of `.row_uid` values to exclude from plotting.
#'
#' @return A plotly object with scatter and histograms.
#' @keywords internal
make_marginal_scatter <- function(df, 
                                  xvar, 
                                  yvar,
                                  nbins_x = 30,
                                  nbins_y = 30, 
                                  highlight_ids = NULL, 
                                  remove_ids = NULL) {
  plot_df <- df
  if (!is.null(remove_ids) && length(remove_ids) > 0) {
    plot_df <- plot_df %>% dplyr::filter(!.row_uid %in% remove_ids)
  }
  
  # Base scatter
  scatter <- plotly::plot_ly(
    data = plot_df,
    x = ~.data[[xvar]], y = ~.data[[yvar]],
    type = "scattergl", mode = "markers",
    marker = list(size = 7, opacity = 0.6, color = "gray"),
    customdata = ~.row_uid
  )
  
  # Highlights
  if (!is.null(highlight_ids) && length(highlight_ids) > 0) {
    hi <- plot_df %>% dplyr::filter(.row_uid %in% highlight_ids)
    if (nrow(hi) > 0) {
      scatter <- scatter %>% plotly::add_trace(
        data = hi,
        x = ~.data[[xvar]], y = ~.data[[yvar]],
        type = "scattergl", mode = "markers",
        marker = list(size = 7, color = "red", opacity = 0.9),
        inherit = FALSE, showlegend = FALSE
      )
    }
  }
  
  # Histograms
  top_hist <- plotly::plot_ly(
    plot_df, x = ~.data[[xvar]],
    type = "histogram", nbinsx = nbins_x,
    marker = list(color = "lightgray", line = list(color = "darkgray", width = 1))
  ) %>% plotly::layout(
    xaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE, zeroline = FALSE),
    yaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE, zeroline = FALSE)
  )
  
  right_hist <- plotly::plot_ly(
    plot_df, y = ~.data[[yvar]],
    type = "histogram", nbinsy = nbins_y,
    marker = list(color = "lightgray", line = list(color = "darkgray", width = 1))
  ) %>% plotly::layout(
    xaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE, zeroline = FALSE),
    yaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE, zeroline = FALSE)
  )
  
  # Combine
  p <- plotly::subplot(
    top_hist, plotly::plotly_empty(),
    scatter, right_hist,
    nrows = 2, shareX = TRUE, shareY = TRUE,
    widths = c(0.8, 0.2), heights = c(0.2, 0.8),
    which_layout = "merge"
  ) %>% plotly::layout(
    showlegend = FALSE,
    dragmode = "lasso",
    xaxis = list(
      title = xvar,
      showline = TRUE, zeroline = FALSE,
      showgrid = TRUE, ticks = "outside",
      showticklabels = TRUE
    ),
    yaxis2 = list(
      title = yvar,
      showline = TRUE, zeroline = FALSE,
      showgrid = TRUE, ticks = "outside",
      showticklabels = TRUE
    )
  ) %>% plotly::config(modeBarButtonsToAdd = c("lasso2d", "select2d"))
  
  p$x$source <- "scatterplot"
  p
}
