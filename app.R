library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(cowplot)
library(rhandsontable)

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
ui <- fluidPage(
  titlePanel("Air Pollution Prediction Scenario Simulator"),
  fluidRow(
    column(
      width = 6,
      h4("Scenario Input by Region and Factor"),
      numericInput("global_value", "Apply Value to All Cells", value = 0.5, min = 0),
      actionButton("apply_all", "Apply to All"),
      br(), br(),
      rHandsontableOutput("scenario_table"),
      br(),
      actionButton("predict_btn", "Run Prediction")
    ),
    column(
      width = 6,
      h4("Prediction Results"),
      tabsetPanel(
        tabPanel("All", plotOutput("plot_all", height = "800px", width = "100%")),
        tabPanel("January", plotOutput("plot_jan", height = "800px", width = "100%")),
        tabPanel("April", plotOutput("plot_apr", height = "800px", width = "100%")),
        tabPanel("July", plotOutput("plot_jul", height = "800px", width = "100%")),
        tabPanel("October", plotOutput("plot_oct", height = "800px", width = "100%"))
      )
    )
  )
)

# === Server ===
server <- function(input, output, session) {
  
  # Initialize scenario table
  scenario_df <- reactiveVal({
    df <- data.frame(matrix(0.5, nrow = length(region_names), ncol = length(factor_names)))
    colnames(df) <- factor_names
    rownames(df) <- region_names
    df
  })
  
  # Render table
  output$scenario_table <- renderRHandsontable({
    rhandsontable(scenario_df(), useTypes = FALSE, stretchH = "all", rowHeaderWidth = 100) %>%
      hot_cols(colWidths = 80) %>%
      hot_col(col = factor_names, format = "0.0000000") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  # Update reactive table
  observeEvent(input$scenario_table, {
    if (!is.null(input$scenario_table)) {
      scenario_df(hot_to_r(input$scenario_table))
    }
  })
  
  # Apply global value to all cells
  observeEvent(input$apply_all, {
    val <- input$global_value
    df <- scenario_df()
    df[,] <- val
    scenario_df(df)
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
    df <- scenario_df()
    if (any(is.na(df))) {
      showModal(modalDialog("Please fill in all cells with numeric values.", easyClose = TRUE))
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
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
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
      draw_label("Ozone Prediction Maps", fontface = 'bold', size = 30, hjust = 0.5, x = 0.5, y = 0.95) +
      draw_plot(final_plot, y = 0, height = 0.9)
    
    final_plot_with_title
  }, res = 96)
}

shinyApp(ui, server)