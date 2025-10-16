suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(shinycssloaders)
  library(shinyjs)
  library(ggplot2)
  library(sf)
  library(cowplot)
})

# -------------------- Constants --------------------
region_names <- c(
  "Seoul","Incheon","Busan","Daegu","Gwangju","Gyeonggi","Gangwon","Chung-Buk",
  "Chung-Nam","Gyeong-Buk","Gyeong-Nam","Jeon-Buk","Jeon-Nam","Jeju","Daejun","Ulsan","Sejong"
)
factor_names <- c("Power","Industrial","Mobile","Residential","Agriculture","Solvent","Others")

# -------------------- Load Spatial & Model Data --------------------
asia_map <- st_read("/home/geseo/LassoCMAQ_Data/Mapping_shp/Asia_county_map.shp", quiet = TRUE)
mesh     <- st_read("/home/geseo/LassoCMAQ_Data/Mapping_shp/Mesh_test_shift2.shp", quiet = TRUE)
st_crs(asia_map) <- 4326
st_crs(mesh)     <- 4326

# O3
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_CMAQ_UNIQUE.RData")
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_BIAS.RData")
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_ADAPT.RData")
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_WEIGHT.RData")
# PM2.5
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_WEIGHT.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_CMAQ_UNIQUE.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_CMAQ.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_BIAS.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_ADAPT.RData")

# -------------------- Theme / CSS --------------------
theme <- bs_theme(
  version = 5, bootswatch = "flatly",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

custom_css <- HTML("
/* ===== Base layout ===== */
html { scroll-behavior: smooth; }
body { padding: 0 24px 48px 24px; }
.sticky-top { backdrop-filter: blur(6px); background: rgba(255,255,255,0.85); }
.section { padding-top: 64px; margin-top: -64px; margin-bottom: 44px; }
.section-block { margin-top: 18px; }
.hero { padding: 28px 0 8px; margin-bottom: 4px; }
.muted { color:#6c757d; }
.copyright { border-top: 1px solid #e9ecef; padding: 12px 0; margin-top: 24px; }

/* Global card padding unified */
.card {
  --bs-card-spacer-y: 10px;
  --bs-card-spacer-x: 12px;
}
.card .card-header, .card .card-body {
  padding: var(--bs-card-spacer-y) var(--bs-card-spacer-x) !important;
}
.card-tight { box-shadow:none; border:1px solid #e9ecef; }

/* ===== Run/Upload cards compact ===== */
.card-compact .form-check-label, .card-compact label,
.card-upload label { font-weight: 600; font-size: 0.95rem; }

/* ===== Policy table: card scroll only ===== */
.custom-table .card-body{
  max-height: 720px;        /* 필요시 조절 */
  overflow-y: auto;
  overflow-x: hidden;
  padding: 12px 12px 14px;
}

/* DT wrapper sizing */
.custom-table .dataTables_wrapper { width: 100%; }
table.dataTable { table-layout: fixed; width: 100% !important; }
table.dataTable td, table.dataTable th { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }

/* Remove DT chrome */
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate,
.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter { display: none !important; }

/* Header styles (compact) */
table.dataTable thead th {
  vertical-align: bottom;
  background: #fdebd0;
  border-color: #f3d2a2;
  font-weight: 700;
  padding: 3px 5px !important;
  height: 26px; line-height: 1.15; font-size: 0.88rem;
}
table.dataTable thead tr.header-inputs th {
  background: #ffefd5;
  font-weight: 600;
  height: 30px;
}
.header-input, .row-input {
  height: 22px !important; padding: 1px 4px !important; line-height: 1.1 !important; font-size: 0.86rem;
}

/* Body cells smaller */
table.dataTable tbody td { padding: 3px 5px !important; height: 24px; font-size: 0.90rem; }

/* First column (Region) */
td.rowhdr {
  background: #fffaf3; border-right: 2px solid #f0d9a8;
  width: 180px !important; max-width: 180px !important;
}
.rowhdr .rname { font-weight: 600; margin-bottom: 4px; display:block; }
.rowhdr .row-input { width: 160px; }

/* Bottom breathing room */
.custom-table .dataTables_wrapper table.dataTable tbody { padding-bottom: 12px; }

/* How-to small card width to content (optional) */
.card-tight.auto-width { display: inline-block; width: auto !important; max-width: 100%; }
")

# -------------------- UI --------------------
ui <- page_fluid(
  theme = theme,
  useShinyjs(),
  tags$head(tags$title("LassoCMAQ — Header-input Table (Compact)"), tags$style(custom_css)),
  
  # Sticky nav
  div(class = "sticky-top py-1",
      layout_column_wrap(width = 1, gap = "4px",
                         card(class="p-1", style="border:1px; box-shadow:none; background:transparent;",
                              layout_column_wrap(width = 1, fill = TRUE,
                                                 div(class="d-flex align-items-center justify-content-between",
                                                     div(tags$strong("LassoCMAQ Web Server • EP Project (2025)")),
                                                     div(class="d-flex gap-3",
                                                         tags$a(href="#home",     class="link-dark text-decoration-none", "Home"),
                                                         tags$a(href="#control",  class="link-dark text-decoration-none", "Control Policy"),
                                                         tags$a(href="#outputs",  class="link-dark text-decoration-none", "Results"),
                                                         tags$a(href="#download", class="link-dark text-decoration-none", "Download")
                                                     )
                                                 )
                              )
                         )
      )
  ),
  
  # Hero
  div(class="hero",
      h2("LassoCMAQ"),
      p(class="muted", "Emission-control based air pollution prediction web interface")
  ),
  
  # Home
  div(id="home", class="section",
      h3("Home"),
      layout_columns(col_widths = c(6,6),
                     card(header="What Is This", class="section-block",
                          p("A DataTables-based policy table where header inputs fill whole rows/columns. No extra Row/Column Input cells or apply buttons.")
                     ),
                     card(header="How to Use (Demo)", class="section-block",
                          tags$ol(
                            tags$li("Type under a column header to fill that entire column."),
                            tags$li("Type beside a region name to fill that entire row."),
                            tags$li("Type in the top-left 'All' input to fill all data cells."),
                            tags$li("Cells remain editable individually as well.")
                          )
                     )
      )
  ),
  
  # Control Policy
  div(id="control", class="section",
      h3("Control Policy"),
      # How-to (narrow)
      card(header="How to Use", class="section-block card-tight auto-width",
           tags$ul(
             tags$li(HTML("<b>Each cell</b> represents the emission adjustment ratio for a specific Region and Sector")),
             tags$li("Cells can be directly edited"),
             tags$li("Update all cells at once"),
             tags$li("Update a specific row or column at once"),
             tags$li("Upload a control policy (sample file provided)"),
             tags$li("Download the current control policy"),
             tags$li("Click Run to start pollutant concentration prediction")
           )
      ),
      layout_columns(col_widths = c(9,3),
                     # LEFT: Table
                     div(
                       card(header="Policy Table (17 × 7)", class="section-block custom-table",
                            DTOutput("policy_dt", width = "100%")
                       )
                     ),
                     # RIGHT: Upload + Run
                     div(
                       card(header="Upload / Download", class="section-block card-tight card-upload",
                            fileInput("policy_upload","Upload Policy (CSV/RDS)", buttonLabel="Upload",
                                      accept = c(".csv", ".rds")),
                            downloadButton("policy_download","Download Current Policy",
                                           class = "btn btn-outline-secondary btn-sm")
                       ),
                       card(header="Run Prediction", class="section-block card-tight card-compact",
                            checkboxGroupInput("pollutants","Select pollutants",
                                               choices = c("Ozone (O₃)" = "o3", "PM2.5" = "pm25"),
                                               selected = c("o3","pm25")),
                            actionButton("btn_run","Run", class="btn btn-primary w-100"),
                            p(class="muted mt-2 mb-0", "Runs LassoCMAQ prediction for selected pollutants.")
                       )
                     )
      )
  ),
  
  # Results
  div(id="outputs", class="section",
      h3("Results"),
      layout_columns(col_widths = c(12),
                     card(header="Ozone", class="section-block",
                          withSpinner(plotOutput("o3_plot", height = "680px"), type = 4),
                          layout_columns(col_widths = c(3,9),
                                         card(class="card-tight", header="Grid Average",
                                              h4(textOutput("o3_mean"), class="display-6")),
                                         card(class="card-tight", header="Summary",
                                              textOutput("o3_summary"))
                          )
                     ),
                     card(header="PM2.5", class="section-block",
                          withSpinner(plotOutput("pm_plot", height = "680px"), type = 4),
                          layout_columns(col_widths = c(3,9),
                                         card(class="card-tight", header="Grid Average",
                                              h4(textOutput("pm_mean"), class="display-6")),
                                         card(class="card-tight", header="Summary",
                                              textOutput("pm_summary"))
                          )
                     )
      )
  ),
  
  # Download
  div(id="download", class="section",
      h3("Download"),
      layout_columns(col_widths = c(6,6),
                     card(class="card-tight", header="Result File",
                          p("Download full prediction results as RDS."),
                          downloadButton("dl_results","Download Results (RDS)", class="btn btn-outline-primary")),
                     card(class="card-tight", header="Policy Template",
                          p("Download a blank 17×7 CSV template."),
                          downloadButton("dl_template","Download Template", class="btn btn-outline-secondary"))
      )
  ),
  
  div(class="copyright", "© Soongsil University Machine Learning Lab. All Rights Reserved.")
)

# -------------------- Server --------------------
server <- function(input, output, session) {
  
  # Underlying numeric matrix (17 x 7), default 1
  vals <- reactiveVal({
    m <- matrix(1, nrow = length(region_names), ncol = length(factor_names),
                dimnames = list(region_names, factor_names))
    m
  })
  
  # ---- helpers for DT ----
  to_numeric_matrix <- function(df) {
    num_df <- as.data.frame(lapply(df, function(x) suppressWarnings(as.numeric(x))), check.names = FALSE)
    m <- as.matrix(num_df)
    colnames(m) <- colnames(df); rownames(m) <- rownames(df)
    m
  }
  make_region_cell <- function(i) {
    as.character(
      tags$div(class="rowhdr",
               tags$span(class="rname", region_names[i]),
               tags$input(type="number", step="0.1", placeholder="All",
                          class="form-control form-control-sm row-input",
                          `data-row`=i)
      )
    )
  }
  make_table_data <- function(m) {
    df <- as.data.frame(m, check.names = FALSE)
    data.frame(
      Region = vapply(seq_len(nrow(df)), make_region_cell, character(1)),
      df,
      check.names = FALSE, row.names = NULL
    )
  }
  
  # custom 2-row header with inputs (All + per-column)
  sketch <- tags$table(
    class = "display",
    tags$thead(
      tags$tr(tags$th("Region"), lapply(factor_names, function(fn) tags$th(fn))),
      tags$tr(class = "header-inputs",
              tags$th(tags$input(type="number", step="0.1", placeholder="All",
                                 class="form-control form-control-sm header-input all-apply")),
              lapply(seq_along(factor_names), function(j)
                tags$th(tags$input(type="number", step="0.1", placeholder="All",
                                   class="form-control form-control-sm header-input col-apply",
                                   `data-col`=j)))
      )
    )
  )
  
  # render DT
  output$policy_dt <- renderDT({
    datatable(
      make_table_data(vals()),
      container = sketch,
      rownames  = FALSE,
      escape    = FALSE,
      selection = "none",
      options = list(
        dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE,
        autoWidth = FALSE, scrollX = FALSE,
        columnDefs = list(
          list(targets = 0, width = "180px", className = "rowhdr"),
          list(targets = 1:7, width = "80px")
        )
      ),
      callback = JS("
function bindRowInputs(api){
  var tbody = $(api.table().body());
  tbody.off('change', 'input.row-input');
  tbody.on('change', 'input.row-input', function(){
    var row = parseInt($(this).attr('data-row'), 10);
    var val = parseFloat(this.value);
    if(!isNaN(val)){
      Shiny.setInputValue('row_apply', {row: row, val: val, nonce: Math.random()});
    }
  });
}
var api = table;
// Header inputs: per-column and ALL
$(api.table().header())
  .on('change', 'input.col-apply', function(){
    var col = parseInt($(this).attr('data-col'), 10);
    var val = parseFloat(this.value);
    if(!isNaN(val)){
      Shiny.setInputValue('col_apply', {col: col, val: val, nonce: Math.random()});
    }
  })
  .on('change', 'input.all-apply', function(){
    var val = parseFloat(this.value);
    if(!isNaN(val)){
      Shiny.setInputValue('all_apply', {val: val, nonce: Math.random()});
    }
  });
// Rebind on draw (keep row inputs alive)
bindRowInputs(api);
api.on('draw.dt', function(){ bindRowInputs(api); });
")
    )
  })
  
  # apply events from header/row inputs
  observeEvent(input$col_apply, {
    info <- input$col_apply; j <- as.integer(info$col); v <- as.numeric(info$val)
    if (is.finite(v) && j >= 1 && j <= ncol(vals())) {
      m <- vals(); m[, j] <- v; vals(m)
      replaceData(dataTableProxy("policy_dt"), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
    }
  })
  observeEvent(input$row_apply, {
    info <- input$row_apply; i <- as.integer(info$row); v <- as.numeric(info$val)
    if (is.finite(v) && i >= 1 && i <= nrow(vals())) {
      m <- vals(); m[i, ] <- v; vals(m)
      replaceData(dataTableProxy("policy_dt"), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
    }
  })
  observeEvent(input$all_apply, {
    v <- as.numeric(input$all_apply$val)
    if (is.finite(v)) {
      m <- vals(); m[,] <- v; vals(m)
      replaceData(dataTableProxy("policy_dt"), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
    }
  })
  
  # upload/download (policy)
  observeEvent(input$policy_upload, {
    req(input$policy_upload)
    tryCatch({
      df <- if (grepl("\\.rds$", input$policy_upload$name, ignore.case = TRUE)) {
        readRDS(input$policy_upload$datapath)
      } else {
        read.csv(input$policy_upload$datapath, row.names = 1, check.names = FALSE)
      }
      df <- df[region_names, factor_names, drop = FALSE]
      m <- to_numeric_matrix(df)
      stopifnot(all(is.finite(m)))
      vals(m)
      replaceData(dataTableProxy('policy_dt'), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
    }, error = function(e) {
      showModal(modalDialog(title = 'Upload Error', paste('Failed to apply policy:', e$message), easyClose = TRUE))
    })
  })
  output$policy_download <- downloadHandler(
    filename = function() paste0('policy_', format(Sys.time(), '%Y%m%d_%H%M%S'), '.csv'),
    content = function(file) write.csv(as.data.frame(vals(), check.names = FALSE), file, row.names = TRUE)
  )
  output$dl_template <- downloadHandler(
    filename = 'policy_template.csv',
    content = function(file) {
      tpl <- matrix(1.0, nrow = length(region_names), ncol = length(factor_names),
                    dimnames = list(region_names, factor_names))
      write.csv(as.data.frame(tpl, check.names = FALSE), file, row.names = TRUE)
    }
  )
  
  # -------------------- Prediction models --------------------
  models <- list(
    o3 = list(
      WEIGHT = O3_WEIGHT, BIAS = O3_BIAS, ADAPT = O3_Adapt,
      CMAQ_UNIQUE = O3_CMAQ_UNIQUE, SCALE = 1000,
      MONTH_IDXS = list(Jan=1:31, Apr=32:61, Jul=62:92, Oct=93:123)
    ),
    pm = list(
      WEIGHT = PM_WEIGHT, BIAS = PM_BIAS, ADAPT = PM_Adapt,
      CMAQ_UNIQUE = PM_CMAQ_UNIQUE, SCALE = 1,
      MONTH_IDXS = list(Jan=1:31, Apr=32:61, Jul=62:92, Oct=93:123)
    )
  )
  
  predict_with_model <- function(control_vec, model) {
    linear_vec  <- as.vector(control_vec %*% model$WEIGHT)
    dims        <- dim(model$BIAS)               # [N, 24, 123]
    linear_arr  <- array(linear_vec, dim = dims)
    linear_pred <- linear_arr + model$BIAS
    Pred        <- model$ADAPT / (1 + exp(-linear_pred))
    if (!is.null(model$CMAQ_UNIQUE)) Pred[model$CMAQ_UNIQUE] <- model$BIAS[model$CMAQ_UNIQUE]
    Pred * model$SCALE
  }
  
  result_store <- reactiveVal(list(o3=NULL, pm=NULL))
  
  observeEvent(input$btn_run, {
    m <- vals()
    if (any(!is.finite(m))) { showModal(modalDialog("All cells must be numeric.", easyClose=TRUE)); return() }
    if (any(m < 0.5 | m > 1.5, na.rm = TRUE)) {
      showModal(modalDialog("All values must be between 0.5 and 1.5.", easyClose=TRUE)); return()
    }
    control_vec <- as.numeric(t(m))
    need_o3 <- "o3" %in% input$pollutants
    need_pm <- "pm25" %in% input$pollutants
    store <- list(o3=NULL, pm=NULL)
    if (need_o3) store$o3 <- predict_with_model(control_vec, models$o3)
    if (need_pm) store$pm <- predict_with_model(control_vec, models$pm)
    result_store(store)
  })
  
  # -------------------- Map helpers & plots --------------------
  month_means <- function(arr3) apply(arr3, 1, mean)  # [N,24,D] -> vector by grid
  mesh_with_vals <- function(preds, model) {
    m <- mesh
    m$Jan  <- month_means(preds[, , model$MONTH_IDXS$Jan, drop = FALSE])
    m$Apr  <- month_means(preds[, , model$MONTH_IDXS$Apr, drop = FALSE])
    m$Jul  <- month_means(preds[, , model$MONTH_IDXS$Jul, drop = FALSE])
    m$Oct  <- month_means(preds[, , model$MONTH_IDXS$Oct, drop = FALSE])
    m$Year <- (m$Jan + m$Apr + m$Jul + m$Oct) / 4
    m
  }
  get_shared_fill_scale <- function(m, legend_title) {
    all_vals <- c(m$Year, m$Jan, m$Apr, m$Jul, m$Oct)
    vmin <- floor(min(all_vals, na.rm = TRUE) / 10) * 10
    vmax <- ceiling(max(all_vals, na.rm = TRUE) / 10) * 10
    scale_fill_gradient(low = "white", high = "red",
                        limits = c(vmin, vmax),
                        breaks = pretty(c(vmin, vmax), n = 5),
                        name = legend_title)
  }
  plot_map <- function(m, var_name, legend_title, title_text, show_legend=TRUE) {
    p <- ggplot() +
      geom_sf(data=asia_map, color="black", fill=NA) +
      geom_sf(data=m, aes(fill=.data[[var_name]]), alpha=0.6, color="gray") +
      coord_sf(xlim=c(124,131), ylim=c(32.5,39.5), expand=FALSE) +
      get_shared_fill_scale(m, legend_title) +
      labs(title=title_text) +
      theme_minimal() +
      theme(
        plot.title=element_text(hjust=0.5,face="bold",size=16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text  = element_text(size = 10)
      )
    if (!show_legend) p <- p + theme(legend.position="none")
    p
  }
  
  # O3
  output$o3_plot <- renderPlot({
    store <- result_store(); req(store$o3)
    m <- mesh_with_vals(store$o3, models$o3)
    cowplot::plot_grid(
      plot_map(m,"Year","Mean (ppb)","Ozone — Annual",FALSE),
      plot_map(m,"Jan","Mean (ppb)","January",FALSE),
      plot_map(m,"Apr","Mean (ppb)","April",FALSE),
      plot_map(m,"Jul","Mean (ppb)","July",FALSE),
      plot_map(m,"Oct","Mean (ppb)","October",TRUE),
      ncol=3
    )
  }, res = 96)
  output$o3_mean <- renderText({
    store <- result_store(); req(store$o3)
    m <- mesh_with_vals(store$o3, models$o3)
    sprintf("%.1f", mean(m$Year, na.rm = TRUE))
  })
  output$o3_summary <- renderText({
    store <- result_store(); req(store$o3)
    m <- mesh_with_vals(store$o3, models$o3)
    rng <- range(m$Year, na.rm = TRUE)
    paste0("Annual range: ", sprintf("%.1f–%.1f ppb", rng[1], rng[2]),
           " | Grid mean: ", sprintf("%.1f ppb", mean(m$Year, na.rm = TRUE)))
  })
  
  # PM2.5
  output$pm_plot <- renderPlot({
    store <- result_store(); req(store$pm)
    m <- mesh_with_vals(store$pm, models$pm)
    cowplot::plot_grid(
      plot_map(m,"Year","Mean (µg/m³)","PM2.5 — Annual",FALSE),
      plot_map(m,"Jan","Mean (µg/m³)","January",FALSE),
      plot_map(m,"Apr","Mean (µg/m³)","April",FALSE),
      plot_map(m,"Jul","Mean (µg/m³)","July",FALSE),
      plot_map(m,"Oct","Mean (µg/m³)","October",TRUE),
      ncol=3
    )
  }, res = 96)
  output$pm_mean <- renderText({
    store <- result_store(); req(store$pm)
    m <- mesh_with_vals(store$pm, models$pm)
    sprintf("%.1f", mean(m$Year, na.rm = TRUE))
  })
  output$pm_summary <- renderText({
    store <- result_store(); req(store$pm)
    m <- mesh_with_vals(store$pm, models$pm)
    rng <- range(m$Year, na.rm = TRUE)
    paste0("Annual range: ", sprintf("%.1f–%.1f µg/m³", rng[1], rng[2]),
           " | Grid mean: ", sprintf("%.1f µg/m³", mean(m$Year, na.rm = TRUE)))
  })
  
  # -------------------- Downloads --------------------
  output$dl_results <- downloadHandler(
    filename=function() paste0("prediction_",format(Sys.time(),"%Y%m%d_%H%M%S"),".rds"),
    content=function(file) saveRDS(result_store(), file)
  )
}

# -------------------- Run --------------------
shinyApp(ui, server)