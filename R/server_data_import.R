#' Data Import and Filter Server (Module Version, Using DuckDB)
#'
#' Handles file upload or preloaded data, performs optional conversions,
#' and loads everything into DuckDB via `convert_to_tbl()`.
#'
#' @param id Module ID (must match the one used in the UI).
#' @param rvals Shared reactiveValues object from the main server.
#' @param preloaded_data Optional data frame to initialize the app.
#'
#' @return None; called for its side effects.
#' @keywords internal
#' @import shiny
server_data_import <- function(id, rvals, preloaded_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rvals <- reactiveValues()
    message("=== Data Import Module Initialized ===")
    message("preloaded_data class: ", paste(class(preloaded_data), collapse = ", "))
    message("preloaded_data is NULL? ", is.null(preloaded_data))
    
    # ---- Initialize DuckDB connection once ---------------------------------
    
    observe({
      if (is.null(isolate(rvals$con.app)) || !DBI::dbIsValid(isolate(rvals$con.app))) {
        message("Initializing new DuckDB connection...")
        rvals$con.app <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = FALSE)
        message("Connection object assigned. Valid? ", DBI::dbIsValid(rvals$con.app))
        showNotification("DuckDB connection established.", type = "message")
      } else {
        message("DuckDB connection already valid.")
      }
    })
    
    
    # ---- Handle preloaded data --------------------------------------------
    observe({
      invalidateLater(500, session)  # check twice per second
      
      # Wait until connection exists and is valid
      if (is.null(rvals$con.app) || !DBI::dbIsValid(rvals$con.app)) return()
      
      # Run only once
      if (!is.null(preloaded_data) && is.null(isolate(rvals$data))) {
        message("Preloading dataset into DuckDB (delayed until connection ready)...")
        
        # Write the preloaded data frame directly into DuckDB
        DBI::dbWriteTable(
          conn = rvals$con.app,
          name = "data",
          value = preloaded_data,
          overwrite = TRUE,
          temporary = FALSE
        )
        
        # Assign lazy references
        rvals$data <- dplyr::tbl(rvals$con.app, "data")
        rvals$filtered_data <- rvals$data
        
        message("Tables in DuckDB:")
        print(DBI::dbListTables(rvals$con.app))
        message("First few rows:")
        print(DBI::dbGetQuery(rvals$con.app, "SELECT * FROM data LIMIT 5"))
        
        showNotification(
          paste0("Preloaded dataset loaded into DuckDB (",
                 ncol(preloaded_data), " columns, ",
                 nrow(preloaded_data), " rows)."),
          type = "message"
        )
      }
    })
    
    
    # ---- Handle user file upload (always overwrites existing) --------------
    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      
      # --- Read file into a base data.frame first ---
      df <- switch(
        tolower(ext),
        "csv"     = utils::read.csv(input$file$datapath,
                                    header = input$header,
                                    stringsAsFactors = FALSE),
        "parquet" = as.data.frame(arrow::read_parquet(input$file$datapath)),
        {
          showNotification("Unsupported file type. Please upload CSV or Parquet.", type = "error")
          return(NULL)
        }
      )
      
      message("Overwriting DuckDB 'data' table with uploaded dataset...")
      
      # --- Write uploaded data directly into DuckDB (overwrite) ---
      DBI::dbWriteTable(
        conn = rvals$con.app,
        name = "data",
        value = df,
        overwrite = TRUE,
        temporary = FALSE
      )
      
      # --- Reassign lazy references (always overwrite preloaded) ---
      rvals$data <- dplyr::tbl(rvals$con.app, "data")
      rvals$filtered_data <- rvals$data
      
      message("Tables in DuckDB after upload:")
      print(DBI::dbListTables(rvals$con.app))
      
      showNotification(
        paste0("Uploaded dataset loaded into DuckDB (",
               ncol(df), " columns, ", nrow(df), " rows)."),
        type = "message"
      )
    })
    
    # ---- Reactive for filtered data (DuckDB table reference) ---------------
    filtered_data <- reactive({
      req(rvals$filtered_data)
      rvals$filtered_data
    })
    
    # ---- Metadata summary --------------------------------------------------
    output$data_meta_summary <- renderUI({
      df <- filtered_data() %>% dplyr::collect()
      req(df)
      
      meta <- data.frame(
        Variable = names(df),
        Type = sapply(df, function(x) class(x)[1]),
        NAs = sapply(df, function(x) sum(is.na(x))),
        Not_NAs = sapply(df, function(x) sum(!is.na(x))),
        stringsAsFactors = FALSE
      )
      
      meta$Unique_Values <- sapply(df, function(x) {
        if (is.numeric(x) || is.character(x) || is.factor(x) || inherits(x, c("Date", "POSIXt")))
          length(unique(x))
        else NA
      })
      
      meta$Range <- sapply(df, function(x) {
        if (is.numeric(x)) {
          paste0(round(min(x, na.rm = TRUE), 3), " – ", round(max(x, na.rm = TRUE), 3))
        } else if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
          rng <- range(x, na.rm = TRUE)
          paste0(as.character(rng[1]), " – ", as.character(rng[2]))
        } else ""
      })
      
      tagList(
        h4("Dataset Summary"),
        tags$p(paste("Rows:", nrow(df), "| Columns:", ncol(df))),
        tags$table(
          class = "meta-table",
          tags$thead(
            tags$tr(
              tags$th("Variable"), tags$th("Type"), tags$th("Unique Values"),
              tags$th("Range (if applicable)"), tags$th("NAs"), tags$th("Not NAs")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(meta)), function(i) {
              tags$tr(
                tags$td(meta$Variable[i]),
                tags$td(meta$Type[i]),
                tags$td(meta$Unique_Values[i]),
                tags$td(meta$Range[i]),
                tags$td(meta$NAs[i]),
                tags$td(meta$Not_NAs[i])
              )
            })
          )
        )
      )
    })
    
    # ---- Data preview ------------------------------------------------------
    output$data_preview <- renderUI({
      df <- filtered_data() %>% head(10) %>% dplyr::collect()
      tagList(
        h4("Data Preview"),
        tableOutput(ns("data_head"))
      )
    })
    
    output$data_head <- renderTable({
      filtered_data() %>% head(10) %>% dplyr::collect()
    })
  })
}
