library(htmltools)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(lubridate)
library(sf)
library(ncdf4)
library(plotly)
library(reshape2)
library(ggplot2)
library(geosphere)
library(sp)
library(lwgeom)
library(rmapshaper)

label_with_help <- function(id, label_text, help_text) {
  tags$label(
    style = "display: inline-flex; align-items: center;",
    label_text,
    tags$span(
      icon("question-circle", style = "color:#337ab7; cursor:pointer; margin-left: 5px;", id = id,
           tabindex = "0", `data-toggle` = "popover", `data-trigger` = "hover focus",
           `data-content` = help_text, `data-html` = "true", title = "")
    ),
    tags$script(HTML(sprintf("
      $(function() {
        $('#%s').popover({placement: 'right', container: 'body'});
      });
    ", id)))
  )
}

options(shiny.maxRequestSize = 500 * 1024^2)  # 100 MB max upload size

ui <- fluidPage(
  useShinyjs(),
  # JavaScript for inactivity timer
  tags$script(HTML("
    var activityTimeout;
    var aggregationInProgress = false;
    
    function resetActivityTimer() {
      if (!aggregationInProgress) {
        clearTimeout(activityTimeout);
        activityTimeout = setTimeout(function() {
          Shiny.setInputValue('inactive', true, {priority: 'event'});
        }, 60000); // 1 minute = 60000 ms
      }
    }
    
    $(document).on('click keypress mousemove scroll', resetActivityTimer);
    resetActivityTimer();
    
    Shiny.addCustomMessageHandler('aggregationStatus', function(status) {
      aggregationInProgress = status;
      if (status) {
        clearTimeout(activityTimeout);
      } else {
        resetActivityTimer();
      }
    });
  ")),
  
  tags$head(
    tags$script(HTML("
      $(function () {
        $('[data-toggle=\"popover\"]').popover();
      });
    "))
  ),
  tags$div(
    style = "display: flex; align-items: center; gap: 15px; margin-bottom: 20px;",
    tags$img(src = "logo.png", width = "100px", style = "border-radius: 20px;"),
    tags$h2("NetCDF Zonal Statistics Calculator", style = "margin: 0;")
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("ncFile", label_with_help("help_ncFile", "Upload NetCDF File", "NetCDF file (.nc) to aggregate."),
                accept = ".nc"),
      
      fileInput("weightsFile", label_with_help("help_weightsFile", "Upload Grid Cells Weight File (Optional)", "Optional RDRS grid cells weights file (.txt)."),
                accept = ".txt"),
      
      fileInput("hrufile", label_with_help("help_hrufile", "Upload HRU Shapefile (.zip)", "Shapefile zip containing .shp, .dbf, .shx, etc."),
                accept = ".zip"),
      
      hidden(
        div(id = "shapefileInputs",
            selectInput("varnames", label_with_help("help_varnames", "NetCDF Variable Names (lon, lat)", "Select NetCDF variables with spatial dims only."), choices = NULL, multiple = TRUE),
            selectInput("dimnames", label_with_help("help_dimnames", "NetCDF Dimension Names (lon_dim, lat_dim)", "Select NetCDF dimension names."), choices = NULL, multiple = TRUE),
            selectInput("HRU_ID", label_with_help("help_HRU_ID", "HRU ID Field Name", "Select the HRU ID field from the shapefile."), choices = NULL, multiple = FALSE)
        )
      ),
      actionButton("runButton", "Run Aggregation", icon = icon("play"), class = "btn-primary"),
      br(),br(),
      uiOutput("download_ui")
    ),
    mainPanel(
      verbatimTextOutput("logText"),
      br(),
      uiOutput("dynamicTabs")
    )
  )
)

server <- function(input, output, session) {
  shp_dir <- reactiveVal(NULL)
  shp_data <- reactiveVal(NULL)
  shp_path <- reactiveVal(NULL)
  aggregated_csv_path <- reactiveVal(NULL)
  
  # Track aggregation status for inactivity timer
  aggregation_active <- reactiveVal(FALSE)
  
  # Handle inactivity
  observeEvent(input$inactive, {
    if (input$inactive && !aggregation_active()) {
      append_log("Session closed due to inactivity")
      session$close()
    }
  })
  
  # Update JavaScript with aggregation status
  observe({
    if (aggregation_active()) {
      session$sendCustomMessage("aggregationStatus", TRUE)
    } else {
      session$sendCustomMessage("aggregationStatus", FALSE)
    }
  })
  
  log_messages <- reactiveVal(character())
  output$download_ui <- renderUI({
    req(aggregated_csv_path())
    tagList(
      downloadButton("downloadCSV", "Download Aggregated CSV", class = "btn-success"),
      tags$div(
        id = "downloadWaitMsg",
        style = "color: grey; display: none; margin-top: 5px;",
        icon("spinner", class = "fa-spin"),
        " Preparing file for download. Please wait..."
      )
    )
  })
  
  append_log <- function(msg) {
    time_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    new_msg <- paste0("[", time_str, "] ", msg)
    isolate({
      old <- log_messages()
      log_messages(c(old, new_msg))
    })
  }
  
  output$logText <- renderText({
    msgs <- log_messages()
    if (length(msgs) == 0) return("Ready to run.\n")
    paste(msgs, collapse = "\n")
  })
  
  observeEvent(input$ncFile, {
    req(input$ncFile)
    append_log(paste("NetCDF file uploaded:", input$ncFile$name))
    
    try({
      nc <- ncdf4::nc_open(input$ncFile$datapath)
      on.exit(ncdf4::nc_close(nc), add = TRUE)
      
      dims <- nc$dim
      space_dims <- names(dims)[grepl("degrees", sapply(dims, `[[`, "units"), ignore.case = TRUE)]
      time_dims <- names(dims)[grepl("since", sapply(dims, `[[`, "units"), ignore.case = TRUE)]
      
      vars <- names(nc$var)
      
      spatial_vars <- vars[sapply(nc$var[vars], function(v) {
        dim_names <- sapply(v$dim, `[[`, "name")
        has_spatial <- all(space_dims %in% dim_names)
        has_time <- any(time_dims %in% dim_names)
        has_spatial && !has_time
      })]
      
      updateSelectInput(session, "varnames", choices = spatial_vars, selected = NULL)
      updateSelectInput(session, "dimnames", choices = names(dims), selected = head(names(dims), 2))
      
      append_log(paste("Spatial-only variables detected:", paste(spatial_vars, collapse = ", ")))
      append_log(paste("Dimensions detected:", paste(names(dims), collapse = ", ")))
    }, silent = TRUE)
  })
  
  observeEvent(input$weightsFile, {
    req(input$weightsFile)
    append_log(paste("Grid weights file uploaded:", input$weightsFile$name))
  })
  
  observeEvent(input$hrufile, {
    req(input$hrufile)
    
    shp_dir(NULL)
    shp_path(NULL)
    
    tempdir <- tempfile("shp_unzip_")
    dir.create(tempdir)
    unzip(input$hrufile$datapath, exdir = tempdir)
    shp_files <- list.files(tempdir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shp_files) == 0) {
      append_log("Error: No .shp file found in uploaded zip.")
      shinyjs::hide("shapefileInputs")
      return()
    }
    
    shp_dir(tempdir)
    shp_path(shp_files[1])
    
    shp <- sf::st_read(shp_files[1], quiet = TRUE)
    shp_data(shp)
    
    shinyjs::show("shapefileInputs")
    
    append_log(paste("Shapefile uploaded and unpacked:", basename(shp_files[1])))
    append_log(paste("Number of polygon features:", nrow(shp)))
    
    updateSelectInput(session, "HRU_ID", choices = names(shp), selected = names(shp)[1])
  })
  
  aggregated_result <- eventReactive(input$runButton, {
    aggregation_active(TRUE)
    append_log("Starting aggregation...")
    req(input$ncFile)
    ncFile_path    <- input$ncFile$datapath
    weightsFile_path <- if (!is.null(input$weightsFile)) input$weightsFile$datapath else NULL
    hrufile_path   <- shp_path()
    
    varnames_vec <- NULL
    dimnames_vec <- NULL
    HRU_ID_val   <- NULL
    if (!is.null(hrufile_path)) {
      varnames_vec <- input$varnames
      dimnames_vec <- input$dimnames
      HRU_ID_val   <- input$HRU_ID
    }
    
    if (!exists("rdrs_spatial_aggregator")) {
      append_log("Loading rdrs_spatial_aggregator function...")
      source("https://raw.githubusercontent.com/rarabzad/RDRS/refs/heads/main/scripts/rdrs_spatial_aggregator.R")
      append_log("Function loaded.")
    }
    
    out <- tryCatch({
      append_log("Running rdrs_spatial_aggregator...")
      
      res <- withProgress(message = "Aggregating NetCDF...", value = 0, {
        incProgress(0.1, detail = "Initializing...")
        r <- rdrs_spatial_aggregator(
          ncFile      = ncFile_path,
          weightsFile = weightsFile_path,
          hrufile     = hrufile_path,
          varnames    = varnames_vec,
          dimnames    = dimnames_vec,
          HRU_ID      = HRU_ID_val
        )
        incProgress(0.8, detail = "Finalizing...")
        r
      })
      
      csv_out_path <- file.path(
        dirname(ncFile_path),
        paste0(tools::file_path_sans_ext(basename(input$ncFile$name)), "_aggregated.csv")
      )
      aggregated_csv_path(csv_out_path)
      
      append_log("Aggregation done.")
      append_log(paste("Aggregated file path:", csv_out_path))
      
      res
    }, error = function(e) {
      append_log(paste("Error:", e$message))
      NULL
    })
    
    aggregation_active(FALSE)
    out
  }, ignoreNULL = FALSE)
  
  output$dynamicTabs <- renderUI({
    req(aggregated_result())
    res        <- aggregated_result()
    dims       <- lapply(res, dim)
    time_vars   <- names(dims)[sapply(dims, function(d) d[1] > 1)]
    static_vars <- names(dims)[sapply(dims, function(d) d[1] == 1)]
    
    tabs <- list()
    
    for (v in time_vars) {
      tabs[[v]] <- tabPanel(
        title = v,
        plotlyOutput(paste0("plot_", v), height = "500px")
      )
    }
    
    for (v in static_vars) {
      tabs[[v]] <- tabPanel(
        title = v,
        DTOutput(paste0("static_", v))
      )
    }
    
    args <- c(list(id = "resultTabs"), unname(tabs))
    do.call(tabsetPanel, args)
  })
  
  observe({
    req(aggregated_result())
    res   <- aggregated_result()
    dims  <- lapply(res, dim)
    ts_v  <- names(dims)[sapply(dims, function(d) d[1] > 1)]
    
    for (v in ts_v) {
      local({
        varname <- v
        output[[paste0("plot_", varname)]] <- renderPlotly({
          mat   <- res[[varname]]
          times <- as.POSIXct(rownames(mat), tz="UTC")
          
          q025  <- apply(mat, 1, quantile, probs = 0.025, na.rm=TRUE)
          med   <- apply(mat, 1, median,       na.rm=TRUE)
          q975  <- apply(mat, 1, quantile, probs = 0.975, na.rm=TRUE)
          
          df <- data.frame(time = times, q025 = q025, median = med, q975 = q975)
          
          p <- ggplot(df, aes(x = time)) +
            geom_ribbon(aes(ymin = q025, ymax = q975),
                        fill = "grey70", alpha = 0.4) +
            geom_line(aes(y = median), size = 0.8, color = "blue") +
            labs(title = varname,
                 x = "Time",
                 y = varname,
                 caption = "Shaded area = 95% interval; line = median") +
            theme_minimal()
          
          ggplotly(p, tooltip = c("x", "median"))
        })
      })
    }
  })
  
  observe({
    req(aggregated_result())
    res       <- aggregated_result()
    dims      <- lapply(res, dim)
    static_v  <- names(dims)[sapply(dims, function(d) d[1] == 1)]
    
    for (v in static_v) {
      local({
        varname <- v
        output[[paste0("static_", varname)]] <- renderDT({
          vals <- round(as.numeric(res[[varname]][1, ]),2)
          df   <- data.frame(HRU = seq_along(vals), 
                             Value = vals)
          datatable(df, 
                    colnames = c("Zone index", varname),
                    options = list(pageLength = 10, scrollX = TRUE))
        })
      })
    }
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      req(input$ncFile)
      paste0(tools::file_path_sans_ext(input$ncFile$name), "_aggregated.csv")
    },
    content = function(file) {
      withProgress(message = "Preparing download...", value = 0, {
        incProgress(0.2, detail = "Fetching results...")
        res_list <- aggregated_result()
        req(res_list)
        
        incProgress(0.5, detail = "Combining data...")
        combined <- do.call(cbind, res_list)
        
        incProgress(0.8, detail = "Writing CSV file...")
        write.csv(combined, file, row.names = TRUE)
        
        incProgress(1, detail = "Done.")
      })
    }
  )
  
  output$tableAggregated <- renderDT({
    res <- aggregated_result()
    req(res)
    if (length(res) > 0) {
      head_df <- head(res[[1]])
      datatable(head_df, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      datatable(data.frame(Message = "No data"))
    }
  })
  
  output$rawList <- renderText({
    res <- aggregated_result()
    if (is.null(res)) return("No result.")
    paste(capture.output(str(res)), collapse = "\n")
  })
  
  output$summaryText <- renderText({
    res <- aggregated_result()
    if (is.null(res)) return("No result.")
    paste0("Number of variables aggregated: ", length(res), "\n",
           "Dimensions of first variable: ", paste(dim(res[[1]]), collapse = " x "))
  })
}

shinyApp(ui, server)