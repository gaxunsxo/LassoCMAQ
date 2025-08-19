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
asia_map <- st_read("/home/geseo/LassoCMAQ_Data/Mapping_shp/Asia_county_map.shp")
mesh <- st_read("/home/geseo/LassoCMAQ_Data/Mapping_shp/Mesh_test_shift2.shp")
st_crs(asia_map) <- NA
st_crs(mesh)     <- NA
st_crs(asia_map) <- 4326
st_crs(mesh)     <- 4326

# === Load prediction-related data (O3) ===
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_CMAQ_UNIQUE.RData")
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_BIAS.RData")
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_ADAPT.RData")
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_WEIGHT.RData")

# === Load prediction-related data (PM2.5) ===
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_WEIGHT.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_CMAQ_UNIQUE.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_CMAQ.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_BIAS.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_ADAPT.RData")

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
                  radioButtons(
                    "display_pollutant", "Display pollutant",
                    choices = c("O3" = "o3", "PM2.5" = "pm"),
                    inline = TRUE, selected = "o3"
                  ),
                  checkboxInput("compute_both", "Compute both (O3 + PM2.5)", value = TRUE), br(),
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
                       tabPanel("October",  withSpinner(plotOutput("plot_oct", height = "700px", width = "100%"))),
                       tabPanel("Both (Compare)",
                                withSpinner(plotOutput("plot_compare_all", height = "1100px", width = "100%"))
                       ),
                       tabPanel("Compare by Month",
                                radioButtons(
                                  "compare_month", "Month",
                                  choices = c("January"="Jan","April"="Apr","July"="Jul","October"="Oct"),
                                  inline = TRUE, selected = "Jan"
                                ),
                                withSpinner(plotOutput("plot_compare_month", height = "700px", width = "100%"))
                       )
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
  
  # Models
  models <- list(
    o3 = list(
      WEIGHT = O3_WEIGHT,
      BIAS   = O3_BIAS,     # dims e.g. [N, 24, 123]
      ADAPT  = O3_Adapt,
      CMAQ_UNIQUE = O3_CMAQ_UNIQUE,
      SCALE  = 1000,        # ppb 스케일링(기존 코드 유지)
      MONTH_IDXS = list(Jan = 1:31, Apr = 32:61, Jul = 62:92, Oct = 93:123)
    ),
    pm = list(
      WEIGHT = PM_WEIGHT,
      BIAS   = PM_BIAS,
      ADAPT  = PM_Adapt,
      CMAQ_UNIQUE = PM_CMAQ_UNIQUE,
      SCALE  = 1,           # µg/m³ 가정
      MONTH_IDXS = list(Jan = 1:31, Apr = 32:61, Jul = 62:92, Oct = 93:123)
    )
  )
  
  # Prediction function
  predict_with_model <- function(control_vec, model) {
    linear_vec <- as.vector(control_vec %*% model$WEIGHT)
    dims <- dim(model$BIAS)  # e.g. c(N, 24, 123)
    linear_arr <- array(linear_vec, dim = dims)
    linear_pred <- linear_arr + model$BIAS
    Pred <- model$ADAPT / (1 + exp(-linear_pred))
    if (!is.null(model$CMAQ_UNIQUE)) {
      Pred[model$CMAQ_UNIQUE] <- model$BIAS[model$CMAQ_UNIQUE]
    }
    Pred <- Pred * model$SCALE
    list(
      Jan = Pred[, , model$MONTH_IDXS$Jan, drop = FALSE],
      Apr = Pred[, , model$MONTH_IDXS$Apr, drop = FALSE],
      Jul = Pred[, , model$MONTH_IDXS$Jul, drop = FALSE],
      Oct = Pred[, , model$MONTH_IDXS$Oct, drop = FALSE]
    )
  }
  
  result_store <- reactiveVal(list(o3 = NULL, pm = NULL))
  
  # Run prediction
  observeEvent(input$predict_btn, {
    df <- scenario_df()[-1, -1]
    if (any(is.na(df))) {
      showModal(modalDialog("Please fill in all cells with numeric values.", easyClose = TRUE)); return()
    }
    if (any(df < 0.5 | df > 1.5, na.rm = TRUE)) {
      showModal(modalDialog("All values must be between 0.5 and 1.5.", easyClose = TRUE)); return()
    }
    control_vec <- as.numeric(t(as.matrix(df)))
    
    need_o3 <- isTRUE(input$compute_both) || identical(input$display_pollutant, "o3")
    need_pm <- isTRUE(input$compute_both) || identical(input$display_pollutant, "pm")
    
    store <- result_store()
    t_o3 <- NA_real_
    t_pm <- NA_real_
    
    if (need_o3) {
      t_o3 <- system.time({
        store$o3 <- predict_with_model(control_vec, models$o3)
      })["elapsed"]
      print(paste("O3 prediction elapsed:", round(t_o3, 3), "sec"))
    }
    
    if (need_pm) {
      t_pm <- system.time({
        store$pm <- predict_with_model(control_vec, models$pm)
      })["elapsed"]
      print(paste("PM2.5 prediction elapsed:", round(t_pm, 3), "sec"))
    }
    
    result_store(store)
    
    total <- sum(na.omit(c(t_o3, t_pm)))
    print(paste("Total prediction time:", round(total, 3), "sec"))
  })
  
  # Map helpers
  month_means <- function(arr3) { apply(arr3, 1, mean) }  # [N, 24, D] → mean over 24*D by grid
  
  mesh_for <- function(pol, store = result_store()) {
    preds <- store[[pol]]
    validate(need(!is.null(preds), paste0("Run prediction for ", toupper(pol), " first.")))
    m <- mesh
    m$Jan <- month_means(preds$Jan)
    m$Apr <- month_means(preds$Apr)
    m$Jul <- month_means(preds$Jul)
    m$Oct <- month_means(preds$Oct)
    m
  }
  
  legend_label_for <- function(pol) {
    if (pol == "o3") "Mean (ppb)" else "Mean (µg/m³)"
  }
  
  # Shared fill scale for consistent color
  get_shared_fill_scale <- function(m, legend_title) {
    all_vals <- c(m$Jan, m$Apr, m$Jul, m$Oct)
    vmin <- floor(min(all_vals, na.rm = TRUE) / 10) * 10
    vmax <- ceiling(max(all_vals, na.rm = TRUE) / 10) * 10
    scale_fill_gradient(low = "white", high = "red",
                        limits = c(vmin, vmax),
                        breaks = pretty(c(vmin, vmax), n = 5),
                        name = legend_title)
  }
  
  # Plot single map
  plot_map <- function(m, var_name, legend_title, title_text, show_legend = TRUE) {
    p <- ggplot() +
      geom_sf(data = asia_map, color = 'black', fill = NA) +
      geom_sf(data = m, aes(fill = .data[[var_name]]), alpha = 0.6, color = "gray") +
      coord_sf(xlim = c(124, 131), ylim = c(32.5, 39.5), expand = FALSE) +
      scale_x_continuous(expand = c(0, 0), labels = ~ as.character(round(.))) +
      scale_y_continuous(expand = c(0, 0), labels = ~ as.character(round(.))) +
      get_shared_fill_scale(m, legend_title) +
      labs(title = title_text) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12),
        axis.text    = element_text(size = 12)
      )
    if (!show_legend) p <- p + theme(legend.position = "none")
    p
  }
  
  selected_mesh <- reactive({
    store <- result_store()
    pol   <- req(input$display_pollutant)
    mesh_for(pol, store)
  })
  
  # Render individual monthly plots
  output$plot_jan <- renderPlot({ m <- selected_mesh(); plot_map(m, "Jan", legend_label_for(input$display_pollutant), "January") })
  output$plot_apr <- renderPlot({ m <- selected_mesh(); plot_map(m, "Apr", legend_label_for(input$display_pollutant), "April") })
  output$plot_jul <- renderPlot({ m <- selected_mesh(); plot_map(m, "Jul", legend_label_for(input$display_pollutant), "July") })
  output$plot_oct <- renderPlot({ m <- selected_mesh(); plot_map(m, "Oct", legend_label_for(input$display_pollutant), "October") })
  
  # Render combined 2x2 plot
  output$plot_all <- renderPlot({
    m <- selected_mesh()
    Jan_PLOT <- plot_map(m, "Jan", legend_label_for(input$display_pollutant), "January", FALSE)
    Apr_PLOT <- plot_map(m, "Apr", legend_label_for(input$display_pollutant), "April",   FALSE)
    Jul_PLOT <- plot_map(m, "Jul", legend_label_for(input$display_pollutant), "July",    FALSE)
    Oct_PLOT <- plot_map(m, "Oct", legend_label_for(input$display_pollutant), "October", FALSE)
    
    legend_plot <- plot_map(m, "Jan", legend_label_for(input$display_pollutant), "Legend", TRUE)
    legend <- cowplot::get_legend(legend_plot + theme(legend.position = "right"))
    grid_main <- cowplot::plot_grid(Jan_PLOT, Apr_PLOT, Jul_PLOT, Oct_PLOT, ncol = 2, align = "hv")
    
    cowplot::plot_grid(
      ggdraw() + draw_label(
        paste0(toupper(input$display_pollutant), " Prediction Maps"),
        fontface = 'bold', size = 20, hjust = 0.5, x = 0.5, y = 0.95
      ) + draw_plot(grid_main, y = 0, height = 0.9),
      legend, rel_widths = c(1, 0.2), ncol = 2
    )
  }, res = 96)
  
  # Compare renders
  output$plot_compare_all <- renderPlot({
    store <- result_store()
    mo3 <- mesh_for("o3", store)
    mpm <- mesh_for("pm", store)
    
    o3_j <- plot_map(mo3, "Jan", legend_label_for("o3"), "January", FALSE)
    o3_a <- plot_map(mo3, "Apr", legend_label_for("o3"), "April",   FALSE)
    o3_jl<- plot_map(mo3, "Jul", legend_label_for("o3"), "July",    FALSE)
    o3_o <- plot_map(mo3, "Oct", legend_label_for("o3"), "October", FALSE)
    o3_grid <- cowplot::plot_grid(o3_j, o3_a, o3_jl, o3_o, ncol = 2, align = "hv")
    o3_leg  <- cowplot::get_legend(plot_map(mo3, "Jan", legend_label_for("o3"), "Legend", TRUE) +
                                     theme(legend.position = "right"))
    o3_all  <- cowplot::plot_grid(o3_grid, o3_leg, ncol = 2, rel_widths = c(1, 0.2))
    
    pm_j <- plot_map(mpm, "Jan", legend_label_for("pm"), "January", FALSE)
    pm_a <- plot_map(mpm, "Apr", legend_label_for("pm"), "April",   FALSE)
    pm_jl<- plot_map(mpm, "Jul", legend_label_for("pm"), "July",    FALSE)
    pm_o <- plot_map(mpm, "Oct", legend_label_for("pm"), "October", FALSE)
    pm_grid <- cowplot::plot_grid(pm_j, pm_a, pm_jl, pm_o, ncol = 2, align = "hv")
    pm_leg  <- cowplot::get_legend(plot_map(mpm, "Jan", legend_label_for("pm"), "Legend", TRUE) +
                                     theme(legend.position = "right"))
    pm_all  <- cowplot::plot_grid(pm_grid, pm_leg, ncol = 2, rel_widths = c(1, 0.2))
    
    cowplot::plot_grid(
      ggdraw() + draw_label("O3 (ppb) — All Months", fontface = 'bold', size = 18, x = 0.02, hjust = 0),
      o3_all,
      ggdraw() + draw_label("PM2.5 (µg/m³) — All Months", fontface = 'bold', size = 18, x = 0.02, hjust = 0),
      pm_all,
      ncol = 1, rel_heights = c(0.06, 0.44, 0.06, 0.44)
    )
  }, res = 96)
  
  output$plot_compare_month <- renderPlot({
    store <- result_store()
    pol_month <- req(input$compare_month)
    mo3 <- mesh_for("o3", store)
    mpm <- mesh_for("pm", store)
    o3 <- plot_map(mo3, pol_month, legend_label_for("o3"), paste0("O3 — ", pol_month), TRUE)
    pm <- plot_map(mpm, pol_month, legend_label_for("pm"), paste0("PM2.5 — ", pol_month), TRUE)
    cowplot::plot_grid(o3, pm, ncol = 2, align = "h")
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