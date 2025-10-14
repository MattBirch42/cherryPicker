#' Data Import and Filter Server
#'
#' Handles file upload, optional auto-detection conversions, metadata summary,
#' and filtering for the Cherry Picker application.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param rvals Shared reactiveValues object from the main server.
#'
#' @return None; called for its side effects.
#' @keywords internal
#' @import shiny
server_data_import <- function(input, output, session, rvals) {
  
  # Detect whether a preloaded dataset is present
  output$preloadedMode <- shiny::reactive({
    !is.null(rvals$data)
  })
  shiny::outputOptions(output, "preloadedMode", suspendWhenHidden = FALSE)
  
  # --- Handle file upload ---
  shiny::observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    df <- switch(
      tolower(ext),
      "csv"     = utils::read.csv(input$file$datapath, header = input$header, stringsAsFactors = FALSE),
      "parquet" = as.data.frame(arrow::read_parquet(input$file$datapath)),
      {
        shiny::showNotification("Unsupported file type. Please upload a CSV or Parquet file.", type = "error")
        return(NULL)
      }
    )
    
    df$.row_uid <- seq_len(nrow(df))
    
    # Apply user-selected auto-detection functions
    if (isTRUE(input$auto_dates) && exists("detect_and_convert_dates")) {
      df <- detect_and_convert_dates(df, session)
    }
    if (isTRUE(input$auto_timestamps) && exists("detect_and_convert_timestamps")) {
      df <- detect_and_convert_timestamps(df, session)
    }
    if (isTRUE(input$convert_characters) && exists("detect_and_convert_characters")) {
      df <- detect_and_convert_characters(df, session, unique_warn_threshold = 50)
    }
    
    rvals$data <- df
    rvals$filtered_data <- df
    
    if (nrow(df) > 20000) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Large Data Warning",
          shiny::p(
            paste0("Your dataset has ", nrow(df), " rows and ", ncol(df),
                   " columns. Larger datasets may slow down the app. ",
                   "Do you want to filter the data first?")
          ),
          footer = shiny::tagList(
            shiny::actionButton("proceed_no_filter", "Proceed without filtering"),
            shiny::actionButton("do_filter_modal", "Filter data")
          ),
          easyClose = FALSE
        )
      )
    }
  })
  
  # Proceed without filtering
  shiny::observeEvent(input$proceed_no_filter, {
    shiny::removeModal()
    rvals$filtered_data <- rvals$data
  })
  
  # --- Metadata Summary (reacts to filters) ---
  output$data_meta_summary <- shiny::renderUI({
    req(rvals$filtered_data)
    df <- rvals$filtered_data
    
    meta <- data.frame(
      Variable = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      NAs = sapply(df, function(x) sum(is.na(x))),
      Not_NAs = sapply(df, function(x) sum(!is.na(x))),
      stringsAsFactors = FALSE
    )
    
    meta$Unique_Values <- sapply(df, function(x) {
      if (is.numeric(x) || is.character(x) || is.factor(x) || inherits(x, c("Date", "POSIXt"))) {
        length(unique(x))
      } else {
        NA
      }
    })
    
    meta$Range <- sapply(df, function(x) {
      if (is.numeric(x)) {
        paste0(round(min(x, na.rm = TRUE), 3), " – ", round(max(x, na.rm = TRUE), 3))
      } else if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
        rng <- range(x, na.rm = TRUE)
        paste0(as.character(rng[1]), " – ", as.character(rng[2]))
      } else {
        ""
      }
    })
    
    shiny::tagList(
      shiny::h4("Dataset Summary"),
      shiny::tags$p(paste("Rows:", nrow(df), "| Columns:", ncol(df))),
      shiny::tags$table(
        class = "meta-table",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Variable"),
            shiny::tags$th("Type"),
            shiny::tags$th("Unique Values"),
            shiny::tags$th("Range (if applicable)"),
            shiny::tags$th("NAs"),
            shiny::tags$th("Not NAs")
          )
        ),
        shiny::tags$tbody(
          lapply(seq_len(nrow(meta)), function(i) {
            shiny::tags$tr(
              shiny::tags$td(meta$Variable[i]),
              shiny::tags$td(meta$Type[i]),
              shiny::tags$td(meta$Unique_Values[i]),
              shiny::tags$td(meta$Range[i]),
              shiny::tags$td(meta$NAs[i]),
              shiny::tags$td(meta$Not_NAs[i])
            )
          })
        )
      )
    )
  })
  
  # --- Filter handling ---
  shiny::observeEvent(c(input$do_filter, input$do_filter_modal), {
    shiny::removeModal()
    df <- rvals$data
    if (is.null(df)) {
      shiny::showNotification("No data available to filter.", type = "warning")
      return(NULL)
    }
    
    shiny::showModal(
      shiny::modalDialog(
        title = "Filter Data",
        shiny::uiOutput("filter_ui"),
        footer = shiny::tagList(
          shiny::div(
            style = "flex-shrink: 0;",
            shiny::actionButton("apply_filters", "Apply Filters"),
            shiny::actionButton("cancel_filters", "Cancel Filters")
          )
        ),
        size = "l",
        easyClose = FALSE
      )
    )
    
    output$filter_ui <- shiny::renderUI({
      if (exists("build_filter_ui")) {
        build_filter_ui(df)
      } else {
        shiny::helpText("Filter UI not available. Please define build_filter_ui().")
      }
    })
  })
  
  # --- Apply filters (fixed ordering) ---
  shiny::observeEvent(input$apply_filters, {
    if (exists("apply_filters")) {
      # Apply filters before removing modal
      filtered_df <- apply_filters(rvals$data, input)
      rvals$filtered_data <- filtered_df
      
      shiny::removeModal()
      
      shiny::showNotification(
        paste("Filters applied. Showing", nrow(filtered_df), "of", nrow(rvals$data), "rows."),
        type = "message"
      )
    } else {
      shiny::showNotification("apply_filters() not available.", type = "warning")
    }
  })
  
  # Cancel filters
  shiny::observeEvent(input$cancel_filters, {
    shiny::removeModal()
  })
  
  # Clear filters
  shiny::observeEvent(input$clear_filters, {
    rvals$filtered_data <- rvals$data
    shiny::showNotification("Filters cleared. Showing all rows.", type = "message")
  })
  
  # --- Filter counter inside modal ---
  output$filter_counter <- shiny::renderUI({
    if (!is.null(rvals$data)) {
      total <- nrow(rvals$data)
      if (exists("apply_filters")) {
        preview <- apply_filters(rvals$data, input)
        current <- nrow(preview)
      } else {
        current <- total
      }
      shiny::tags$p(
        paste0("Filtered rows: ", current, " / ", total),
        style = "margin: 0; font-weight: bold;"
      )
    }
  })
  
  # --- Data preview section (reactive to filtering) ---
  output$data_preview <- shiny::renderUI({
    req(rvals$filtered_data)
    total <- if (!is.null(rvals$data)) nrow(rvals$data) else NA
    filtered <- nrow(rvals$filtered_data)
    
    shiny::tagList(
      shiny::h4("Data Preview"),
      shiny::tags$p(
        paste("Rows available:", filtered, "of", total),
        style = "font-weight: bold; color: #2E86AB;"
      ),
      shiny::tableOutput("data_head")
    )
  })
  
  output$data_head <- shiny::renderTable({
    req(rvals$filtered_data)
    df <- head(rvals$filtered_data, 10)
    
    # Convert date/time columns to readable strings
    df <- dplyr::mutate(df, dplyr::across(
      where(~ inherits(., c("Date", "POSIXt"))),
      ~ as.character(.)
    ))
    
    df
  })
}
