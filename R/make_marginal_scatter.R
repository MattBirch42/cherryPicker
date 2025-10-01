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
  
  # these let you remove a bunch a missing factor levels won't break it:
  {
    if (is.factor(plot_df[[xvar]])) {
      plot_df[[xvar]] <- as.numeric(as.character(plot_df[[xvar]]))
    } else {
      plot_df[[xvar]] <- as.numeric(plot_df[[xvar]])
    }
  
    if (is.factor(plot_df[[yvar]])) {
      plot_df[[yvar]] <- as.numeric(as.character(plot_df[[yvar]]))
    } else {
      plot_df[[yvar]] <- as.numeric(plot_df[[yvar]])
    }
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
  
  xbins <- cut(plot_df[[xvar]], breaks = nbins_x, include.lowest = TRUE)
  xhist <- plot_df %>%
    dplyr::mutate(bin = xbins) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      count  = dplyr::n(),
      ids    = list(.row_uid),
      center = mean(.data[[xvar]], na.rm = TRUE),
      .groups = "drop"
    )
  
  top_hist <- plotly::plot_ly(
    data = xhist,
    x = ~center,
    y = ~count,
    type = "bar",
    customdata = ~ids,              # carry row IDs
    source = "scatterplot",         # unify with scatter for click events
    marker = list(color = "lightgray",
                  line = list(color = "darkgray", width = 1))
  ) %>% plotly::layout(
    xaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE, zeroline = FALSE),
    yaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE, zeroline = FALSE)
  )
  

  ybins <- cut(plot_df[[yvar]], breaks = nbins_y, include.lowest = TRUE)
  yhist <- plot_df %>%
    dplyr::mutate(bin = ybins) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      count  = dplyr::n(),
      ids    = list(.row_uid),
      center = mean(.data[[yvar]], na.rm = TRUE),
      .groups = "drop"
    )
  
  right_hist <- plotly::plot_ly(
    data = yhist,
    x = ~count,
    y = ~center,
    type = "bar",
    orientation = "h",              # horizontal bars
    customdata = ~ids,              # carry row IDs
    source = "scatterplot",         # unify with scatter for click events
    marker = list(color = "lightgray",
                  line = list(color = "darkgray", width = 1))
  ) %>% plotly::layout(
    xaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE, zeroline = FALSE),
    yaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE, zeroline = FALSE)
  )
  
  # Combine
  p <- plotly::subplot(
    top_hist, plotly::plotly_empty(type = "scatter", mode = "markers"),
    scatter,  right_hist,
    nrows = 2, shareX = TRUE, shareY = TRUE,
    widths = c(0.8, 0.2), heights = c(0.2, 0.8),
    which_layout = "merge"
  ) %>%
    plotly::layout(
      showlegend = FALSE,
      dragmode = "select",   # default is click/zoom, not lasso
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
    ) %>%
    plotly::config(
      modeBarButtons = list(
        list("zoom2d", "pan2d"),             # zoom & pan
        list("lasso2d", "select2d"),         # selection tools (now obvious)
        list("resetScale2d", "autoScale2d")  # reset/auto scale
      ),
      displaylogo = FALSE                    # hide plotly logo
    )
  
  p$x$source <- "scatterplot"
  p <- p %>%
    plotly::event_register("plotly_click") %>%
    plotly::event_register("plotly_selected")
  
  p
}
