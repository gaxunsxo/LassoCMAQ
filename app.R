library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(cowplot)
library(rhandsontable)
library(shinycssloaders)
library(shinyjs)
library(shinydashboard)

# === Load spatial data ===
asia_map <- st_read("/ext_hdd_data1/hwlee/Climate/Data/Mapping_shp/Asia_county_map.shp")
mesh <- st_read("/ext_hdd_data1/hwlee/Climate/Data/Mapping_shp/Mesh_test_shift2.shp")
asia_map <- st_set_crs(asia_map, 102012)
mesh <- st_set_crs(mesh, 102012)

# === Load prediction-related data ===
load("/ext_hdd_data1/hwlee/Climate/O3/Adaptive_logit/Total/O3_CMAQ_UNIQUE.RData")
load("/ext_hdd_data1/hwlee/Climate/O3/Adaptive_logit/Total/O3_BIAS.RData")
load("/ext_hdd_data1/hwlee/Climate/O3/Adaptive_logit/Total/O3_ADAPT.RData")
load("/ext_hdd_data1/hwlee/Climate/O3/Adaptive_logit/Total/O3_WEIGHT.RData")

region_names <- c("Seoul", "Incheon", "Busan", "Daegu", "Gwangju", "Gyeonggi", "Gangwon", "Chung-Buk",
                  "Chung-Nam", "Gyeong-Buk", "Gyeong-Nam", "Jeon-Buk", "Jeon-Nam", "Jeju", "Daejun",
                  "Ulsan", "Sejong")

factor_names <- c("Power", "Industrial", "Mobile", "Residential", "Agriculture", "Solvent", "Others")

# === UI ===
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tagList(icon("wind"), span("LassoCMAQ"))
  ),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Scenario", tabName = "scenario", icon = icon("sliders")),
                menuItem("Results",  tabName = "results",  icon = icon("map")),
                menuItem("Download", tabName = "download", icon = icon("download")),
                menuItem("About",    tabName = "about",    icon = icon("circle-info"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$title("Ozone & PM2.5 Scenario Simulator"), 
      tags$style(HTML("
        /* Monotone palette */
        :root {
          --accent: #374151;
          --accent-contrast: #f3f4f6;
          --panel-border: #4b5563;
        }

        /* Box style */
        .box { border:1px solid var(--panel-border) !important; border-radius:12px; overflow:hidden; }
        .box>.box-header { background:var(--accent) !important; color:var(--accent-contrast) !important; }
        .box.box-primary, .box.box-info, .box.box-success, .box.box-warning, .box.box-danger {
          border:1px solid var(--panel-border) !important;
        }
        .box.box-primary>.box-header,
        .box.box-info>.box-header,
        .box.box-success>.box-header,
        .box.box-warning>.box-header,
        .box.box-danger>.box-header {
          background:var(--accent) !important; color:var(--accent-contrast) !important;
        }
        .box.box-solid, .box.box-solid>.box-header { border:1px solid var(--panel-border) !important; }
        .box.box-solid>.box-header+.box-body, .box .box-body { border-top:1px solid var(--panel-border) !important; }

        /* Buttons, tabs, labels */
        .btn-primary, .btn-primary:hover, .btn-primary:focus {
          background-color:var(--accent) !important; border-color:var(--panel-border) !important; color:var(--accent-contrast) !important;
        }
        .nav-tabs-custom>.nav-tabs>li.active>a, .nav-tabs-custom>.nav-tabs>li.active>a:focus {
          background:#f8f9fb; border-color:var(--panel-border) !important;
        }
        .nav-tabs-custom>.nav-tabs>li>a { color:var(--accent); }
        .progress-bar, .label-primary, .bg-light-blue { background-color:var(--accent) !important; color:var(--accent-contrast) !important; }

        /* Layout adjustments */
        .main-header { position:fixed; top:0; width:100%; z-index:1000; }
        .main-sidebar { position:fixed; height:100vh; }
        .content-wrapper, .right-side, .main-footer { margin-top:50px; }
        .content-wrapper, .right-side { margin-left:230px; }
        .content { max-width:1600px; margin:0 auto; }
      "))
    ),
    
    tabItems(
      # Scenario Tab
      tabItem(tabName = "scenario",
              fluidRow(
                box(
                  title = "How to Use", width = 12, status = "primary", solidHeader = TRUE,
                  h4("🔎 How to Use This App"),
                  tags$ul(
                    tags$li("Fill in emission control values by region and factor."),
                    tags$li("Or upload a scenario CSV file."),
                    tags$li("Use the top-left input to apply a global value to all cells."),
                    tags$li("Click 'Apply Row/Col' to fill based on headers."),
                    tags$li("Click 'Apply Table' after manual edits."),
                    tags$li("Finally, click 'Run Prediction' to generate air pollution maps.")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Scenario Controls", width = 5, status = "warning", solidHeader = TRUE,
                  fileInput("upload_scenario", "Upload Scenario CSV",
                            accept = c(".csv"), buttonLabel = "Browse..."),
                  numericInput("global_value", "Apply Value to All Cells", value = 0.5, min = 0.5, max = 1.5),
                  div(
                    actionButton("apply_all",  "Apply to All"),
                    actionButton("apply_rc",   "Apply Row/Column Inputs"),
                    actionButton("apply_table","Apply Edited Table"),
                    style = "display:flex; gap:8px; flex-wrap:wrap; margin-bottom:10px;"
                  ),
                  actionButton("predict_btn", "Run Prediction", class = "btn btn-primary")
                ),
                box(
                  title = "Scenario Input by Region and Factor", width = 7, status = "warning", solidHeader = TRUE,
                  rHandsontableOutput("scenario_table"), br()
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              box(
                width = 12, status = "success", solidHeader = TRUE, title = "Prediction Results",
                tabBox(width = 12, id = "plots_tab",
                       tabPanel("All",      withSpinner(plotOutput("plot_all", height = "700px", width = "100%"))),
                       tabPanel("January",  withSpinner(plotOutput("plot_jan", height = "700px", width = "100%"))),
                       tabPanel("April",    withSpinner(plotOutput("plot_apr", height = "700px", width = "100%"))),
                       tabPanel("July",     withSpinner(plotOutput("plot_jul", height = "700px", width = "100%"))),
                       tabPanel("October",  withSpinner(plotOutput("plot_oct", height = "700px", width = "100%")))
                )
              )
      ),
      
      # Download Tab
      tabItem(tabName = "download",
              box(
                title = "Export", width = 12, status = "info", solidHeader = TRUE,
                div(
                  downloadButton("download_scenario",   "Download Scenario CSV"),
                  downloadButton("download_prediction", "Download Prediction RDS"),
                  style = "display:flex; gap:12px; flex-wrap:wrap;"
                ),
                helpText("Generate predictions first, then download scenario and results.")
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              box(width = 12, title = "About", status = "primary", solidHeader = TRUE,
                  p("Ozone prediction simulator (Shiny + ggplot2 + sf + cowplot).")
              )
      )
    )
  )
)

# === Server ===
server <- function(input, output, session) {
  
  # Extended scenario table
  scenario_df <- reactiveVal({
    df <- matrix(NA, nrow = length(region_names) + 1, ncol = length(factor_names) + 1)
    df <- as.data.frame(df)
    colnames(df) <- c("Input", factor_names)
    rownames(df) <- c("Input", region_names)
    df
  })
  
  # Uploaded original matrix
  uploaded_mat <- reactiveVal(NULL)
  
  # Render table
  output$scenario_table <- renderRHandsontable({
    input$apply_rc
    input$apply_all
    input$apply_table
    df <- scenario_df()
    req(df)
    rhandsontable(df, useTypes = FALSE, stretchH = "all", rowHeaderWidth = 120) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  # Apply edited table via button
  observeEvent(input$apply_table, {
    req(input$scenario_table)
    tryCatch({
      updated_df <- hot_to_r(input$scenario_table)
      if (!is.null(updated_df) && is.data.frame(updated_df)) {
        scenario_df(updated_df)
      }
    }, error = function(e) {
      cat("hot_to_r error:", e$message, "\n")
    })
  })
  
  # Apply to all
  observeEvent(input$apply_all, {
    val <- input$global_value
    df <- scenario_df()
    df[2:nrow(df), 2:ncol(df)] <- val
    scenario_df(df)
  })
  
  # Apply row/col header values
  observeEvent(input$apply_rc, {
    req(input$scenario_table)
    
    df <- hot_to_r(input$scenario_table)
    
    for (j in 2:ncol(df)) {
      val <- df[1, j]
      if (!is.na(val)) {
        df[2:nrow(df), j] <- val
      }
    }
    
    for (i in 2:nrow(df)) {
      val <- df[i, 1]
      if (!is.na(val)) {
        df[i, 2:ncol(df)] <- val
      }
    }
    
    scenario_df(df)
  })
  
  # Upload CSV → Apply to Table & Store
  make_extended_scenario <- function(df_trimmed) {
    stopifnot(is.data.frame(df_trimmed))
    miss_rows <- setdiff(region_names, rownames(df_trimmed))
    miss_cols <- setdiff(factor_names, colnames(df_trimmed))
    if (length(miss_rows) || length(miss_cols)) {
      stop(paste0("CSV header mismatch.\nMissing rows: ",
                  paste(miss_rows, collapse=", "),
                  "\nMissing cols: ",
                  paste(miss_cols, collapse=", ")))
    }
    df_trimmed <- df_trimmed[region_names, factor_names, drop = FALSE]
    
    df_num <- as.data.frame(lapply(df_trimmed, as.numeric), check.names = FALSE)
    rownames(df_num) <- rownames(df_trimmed)
    if (any(!is.finite(as.matrix(df_num)))) stop("CSV contains non-numeric values.")
    if (any(df_num < 0.5 | df_num > 1.5))  stop("CSV values must be in [0.5, 1.5].")
    
    ext <- as.data.frame(matrix(NA, nrow = length(region_names)+1, ncol = length(factor_names)+1))
    colnames(ext) <- c("입력", factor_names)
    rownames(ext) <- c("입력", region_names)
    ext[-1, -1] <- as.matrix(df_num)
    
    attr(ext, "numeric_matrix") <- as.matrix(df_num)
    ext
  }
  
  observeEvent(input$upload_scenario, {
    req(input$upload_scenario)
    tryCatch({
      df_trimmed <- read.csv(input$upload_scenario$datapath, row.names = 1, check.names = FALSE)
      ext <- make_extended_scenario(df_trimmed)
      scenario_df(ext)
      uploaded_mat(attr(ext, "numeric_matrix"))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Upload Error",
        paste("Failed to apply CSV:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Get current scenario matrix (prefer uploaded)
  get_scenario_matrix <- reactive({
    if (!is.null(uploaded_mat())) return(uploaded_mat())
    df <- scenario_df()
    m <- suppressWarnings(apply(df[-1, -1], 2, as.numeric))
    if (any(is.na(m))) return(NULL)
    m
  })
  
  # Prediction function
  predict_pollutant <- function(control_vec) {
    linear_vec <- as.vector(control_vec %*% O3_WEIGHT)
    linear_arr <- array(linear_vec, dim = c(5494, 24, 123))
    linear_pred <- linear_arr + O3_BIAS
    
    Pred <- O3_Adapt / (1 + exp(-linear_pred))
    Pred[O3_CMAQ_UNIQUE] <- O3_BIAS[O3_CMAQ_UNIQUE]
    
    results <- Pred * 1000

    list(
      Jan = results[, , 1:31],
      Apr = results[, , 32:61],
      Jul = results[, , 62:92],
      Oct = results[, , 93:123]
    )
  }
  
  result_list <- reactiveVal(NULL)
  
  # Run prediction
  observeEvent(input$predict_btn, {
    df <- scenario_df()[-1, -1]  # exclude header row/col
    if (any(is.na(df))) {
      showModal(modalDialog("Please fill in all cells with numeric values.", easyClose = TRUE))
      return()
    }
    if (any(df < 0.5 | df > 1.5, na.rm = TRUE)) {
      showModal(modalDialog("All values must be between 0.5 and 1.5.", easyClose = TRUE))
      return()
    }
    
    control_vec <- as.numeric(t(as.matrix(df)))
    result <- predict_pollutant(control_vec)
    
    mesh$Jan <<- apply(result$Jan, 1, mean)
    mesh$Apr <<- apply(result$Apr, 1, mean)
    mesh$Jul <<- apply(result$Jul, 1, mean)
    mesh$Oct <<- apply(result$Oct, 1, mean)
    
    result_list(result)
  })
  
  # Shared fill scale for consistent color
  get_shared_fill_scale <- function() {
    all_vals <- c(mesh$Jan, mesh$Apr, mesh$Jul, mesh$Oct)
    
    rmse_min <- floor(min(all_vals, na.rm = TRUE) / 10) * 10
    rmse_max <- ceiling(max(all_vals, na.rm = TRUE) / 10) * 10
    rmse_breaks <- pretty(c(rmse_min, rmse_max), n = 5)
    
    scale_fill_gradient(
      low = "white", high = "red",
      limits = c(rmse_min, rmse_max),
      breaks = rmse_breaks,
      name = "Mean\n(ppb)"
    )
  }
  
  # Plot single map
  plot_map <- function(var_name, title_text, show_legend = TRUE) {
    if (!var_name %in% colnames(mesh)) {
      return(ggplot() + labs(title = paste(title_text, "(No data)")))
    }
    
    p <- ggplot() +
      geom_sf(data = asia_map, color = 'black', fill = NA) +
      geom_sf(data = mesh, aes(fill = .data[[var_name]]), alpha = 0.6, color = "gray") +
      coord_sf(xlim = c(124, 131), ylim = c(32.5, 39.5), expand = FALSE) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      get_shared_fill_scale() +
      labs(title = title_text) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12)
      )
    
    if (!show_legend) {
      p <- p + theme(legend.position = "none")
    }
    
    return(p)
  }
  
  # Render individual monthly plots
  output$plot_jan <- renderPlot({ req(result_list()); plot_map("Jan", "January") })
  output$plot_apr <- renderPlot({ req(result_list()); plot_map("Apr", "April") })
  output$plot_jul <- renderPlot({ req(result_list()); plot_map("Jul", "July") })
  output$plot_oct <- renderPlot({ req(result_list()); plot_map("Oct", "October") })
  
  # Render combined 2x2 plot
  output$plot_all <- renderPlot({
    req(result_list())
    
    Jan_PLOT <- plot_map("Jan", "January", FALSE)
    Apr_PLOT <- plot_map("Apr", "April", FALSE)
    Jul_PLOT <- plot_map("Jul", "July", FALSE)
    Oct_PLOT <- plot_map("Oct", "October", FALSE)
    
    legend_plot <- plot_map("Jan", "Legend", TRUE)
    legend <- cowplot::get_legend(legend_plot + theme(legend.position = "right"))
    
    plot_grid_main <- cowplot::plot_grid(
      Jan_PLOT, Apr_PLOT,
      Jul_PLOT, Oct_PLOT,
      ncol = 2, align = "hv"
    )
    
    final_plot <- cowplot::plot_grid(
      plot_grid_main, legend,
      rel_widths = c(1, 0.2),
      ncol = 2
    )
    
    final_plot_with_title <- ggdraw() +
      draw_label("Ozone Prediction Maps", fontface = 'bold', size = 20, hjust = 0.5, x = 0.5, y = 0.95) +
      draw_plot(final_plot, y = 0, height = 0.9)
    
    final_plot_with_title
  }, res = 96)
  
  # Scenario download handler
  output$download_scenario <- downloadHandler(
    filename = function() {
      paste0("scenario_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      df <- scenario_df()
      df_trimmed <- df[-1, -1]
      rownames(df_trimmed) <- rownames(df)[-1]
      colnames(df_trimmed) <- colnames(df)[-1]
      write.csv(df_trimmed, file, row.names = TRUE)
    }
  )
  
  # Prediction result download handler
  output$download_prediction <- downloadHandler(
    filename = function() {
      paste0("prediction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    },
    content = function(file) {
      result <- result_list()
      if (!is.null(result)) saveRDS(result, file)
    }
  )
  
  # Scenario download popup
  observeEvent(input$download_scenario, {
    shinyjs::delay(500, {
      showModal(modalDialog(
        title = "📥 Download Complete",
        "The scenario file has been successfully saved.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })
  
  # Prediction result download popup
  observeEvent(input$download_prediction, {
    shinyjs::delay(500, {
      showModal(modalDialog(
        title = "📥 Download Complete",
        "The prediction result file has been successfully saved.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })
}

shinyApp(ui, server)