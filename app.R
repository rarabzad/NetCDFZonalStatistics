required_packages <- c("shiny","shinyjs","shinyWidgets","htmltools","DT",
          "ncdf4","lubridate","sf","geosphere","dplyr",
          "ggplot2","plotly","reshape2","rmapshaper")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
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
library(raster)
library(leaflet)
library(zip)
library(dplyr)

# put a warning on time/resources limit would appear on the second line of the log
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
        }, 300000); // 1 minute = 300000 ms
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
    tags$h2("NetCDF Zonal Statistics Calculator", style = "margin: 0;"),
    tags$p(
      "This calculator requires two inputs for the area being processed: (1) a NetCDF file with spatial dimensions, and (2) either a grid cell weights text file or a shapefile defining your zones of interest. It outputs a CSV file containing the average value of the selected variable(s) within each zone.",
      style = "font-size: 1.2em; color: #555;"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      tags$p(
        "For more information and sample data ",
        tags$a(href = "https://github.com/rarabzad/NetCDFZonalStatistics",
               "click here", target = "_blank")
      ),
      
      fileInput("ncFile", label_with_help("help_ncFile", "Upload NetCDF File", "NetCDF file (.nc) to remap to user defined zones!"),
                accept = ".nc"),
      # Toggle selector
      radioButtons("uploadChoice", "Select Input Type:",
                   choices = c("Grid Cell Weights File" = "weights",
                               "HRU Shapefile" = "hru"),
                   inline = TRUE),
      
      # Conditionally show fileInputs
      conditionalPanel(
        condition = "input.uploadChoice == 'weights'",
        fileInput("weightsFile", 
                  label_with_help("help_weightsFile", 
                                  "1. Upload Grid Cell Weights File (Optional)", 
                                  HTML("This <code>.txt</code> file assigns weights to each grid cell for calculating the average in each zone. 
           You can generate this file using 
           <a href='https://raven-gridweightsgenerator.share.connect.posit.cloud/' target='_blank'>this Shiny app</a> 
           or 
           <a href='https://github.com/rarabzad/GridWeightsGenerator/blob/main/grids_weights_generator.R' target='_blank'>this function</a>.")
                  ),
                  accept = ".txt"
        )
      ),
      
      conditionalPanel(
        condition = "input.uploadChoice == 'hru'",
        fileInput("hrufile", 
                  label_with_help("help_hrufile", 
                                  "2. Upload HRU Shapefile (.zip)", 
                                  "Upload a zipped shapefile containing all required files (.shp, .dbf, .shx, etc.). 
       Use this if you don’t have a grid cell weights file."),
                  accept = ".zip"
        )
      ),
      uiOutput("varsAndDims"),
      actionButton("runButton", "Calculate Averages", icon = icon("play"), class = "btn-primary"),
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
  aggregated_result_val <- reactiveVal(NULL)
  aggregated_csv_path <- reactiveVal(NULL)
  ncInfo <- reactive({
    req(input$ncFile)
    nc <- ncdf4::nc_open(input$ncFile$datapath)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    
    all_dims <- names(nc$dim)
    # detect lon/lat dims by name
    space_dims <- grep("lon|lat", all_dims, ignore.case=TRUE, value=TRUE)
    # detect time dims by name
    time_dims  <- grep("time", all_dims, ignore.case=TRUE, value=TRUE)
    spatial_dims <-all_dims[!(all_dims %in% time_dims)]
    vars <- names(nc$var)
    spatial_vars <- vars[sapply(nc$var[vars], function(v) {
      dn <- sapply(v$dim, `[[`, "name")
      all(space_dims %in% dn) && !any(time_dims %in% dn)
    })]
    
    list(
      spatial_dims = spatial_dims,
      dims         = all_dims,
      spatial_vars = spatial_vars
    )
  })
  
  
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
      append_log(paste("NetCDF variables to be averaged in each zone:", paste0(vars[!(vars %in% spatial_vars)],collapse=", ")))
      
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

  #––– STEP 2: dynamically render the varnames/dimnames/HRU_ID selects –––
  output$varsAndDims <- renderUI({
    # wait until BOTH NetCDF and HRU shapefile are uploaded
    req(input$ncFile, input$hrufile)
    
    # extract NetCDF dims & spatial variables
    nc <- ncdf4::nc_open(input$ncFile$datapath)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    all_dims <- names(nc$dim)
    space_dims <- grep("lon|lat", all_dims, ignore.case = TRUE, value = TRUE)
    time_dims  <- grep("time",   all_dims, ignore.case = TRUE, value = TRUE)
    spatial_dims <-all_dims[!(all_dims %in% time_dims)]
    vars       <- names(nc$var)
    spatial_vars <- vars[sapply(nc$var[vars], function(v) {
      dn <- sapply(v$dim, `[[`, "name")
      all(space_dims %in% dn) && !any(time_dims %in% dn)
    })]
    
    tagList(
      selectInput("varnames",
                  label_with_help("help_varnames",
                                  "NetCDF Variable Names (lon, lat)",
                                  "Select NetCDF variables with spatial dims only."),
                  choices  = spatial_vars,
                  selected = NULL,
                  multiple = TRUE),
      
      selectInput("dimnames",
                  label_with_help("help_dimnames",
                                  "NetCDF Spatial Dimension Names (lon_dim, lat_dim)",
                                  "Select NetCDF dimension names."),
                  choices  = spatial_dims,
                  selected = head(spatial_dims, 2),
                  multiple = TRUE),
      
      selectInput("HRU_ID",
                  label_with_help("help_HRU_ID",
                                  "HRU ID Field Name",
                                  "Select the HRU ID field from the shapefile."),
                  choices  = names(shp_data()),
                  selected = names(shp_data())[1],
                  multiple = FALSE)
    )
  })

    
  observeEvent(input$runButton, {
    req(input$ncFile)
    
    aggregation_active(TRUE)    # pause inactivity timeout
    append_log("Starting aggregation...")
    
    # immediately show the progress modal
    withProgress(message = "Aggregating NetCDF...", value = 0, {
      incProgress(0.1, detail = "Initializing…")
      
      # run the aggregator
      source("https://raw.githubusercontent.com/rarabzad/NetCDFZonalStatistics/refs/heads/main/spatial_aggregator.R")
      res <- spatial_aggregator(
        ncFile      = input$ncFile$datapath,
        weightsFile = if (is.null(input$weightsFile)) NULL else input$weightsFile$datapath,
        hrufile     = shp_path(),
        varnames    = input$varnames,
        dimnames    = input$dimnames,
        HRU_ID      = input$HRU_ID
      )
      
      incProgress(0.8, detail = "Finalizing…")
      Sys.sleep(0.1)  # optional small pause so the “80% → 100%” is visible
      
      # write out CSV path
      csv_out <- file.path(
        dirname(input$ncFile$datapath),
        paste0(tools::file_path_sans_ext(input$ncFile$name), "_aggregated.csv")
      )
      aggregated_csv_path(csv_out)
      
      append_log("Aggregation done.")
      append_log(paste("Aggregated file path:", csv_out))
      
      # store the result
      aggregated_result_val(res)
    })
    
    aggregation_active(FALSE)
  }, ignoreNULL = FALSE)
  
  output$dynamicTabs <- renderUI({
    req(aggregated_result_val())
    res        <- aggregated_result_val()
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
    req(aggregated_result_val())
    res   <- aggregated_result_val()
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
    req(aggregated_result_val())
    res       <- aggregated_result_val()
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
        res_list <- aggregated_result_val()
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
    res <- aggregated_result_val()
    req(res)
    if (length(res) > 0) {
      head_df <- head(res[[1]])
      datatable(head_df, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      datatable(data.frame(Message = "No data"))
    }
  })
  
  output$rawList <- renderText({
    res <- aggregated_result_val()
    if (is.null(res)) return("No result.")
    paste(capture.output(str(res)), collapse = "\n")
  })
  
  output$summaryText <- renderText({
    res <- aggregated_result_val()
    if (is.null(res)) return("No result.")
    paste0("Number of variables aggregated: ", length(res), "\n",
           "Dimensions of first variable: ", paste(dim(res[[1]]), collapse = " x "))
  })
}

shinyApp(ui, server)




