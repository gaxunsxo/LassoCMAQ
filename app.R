suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(shinycssloaders)
  library(shinyjs)
  library(sf)
  library(dplyr)
  library(waiter)
  library(shinyWidgets)
  library(leaflet)
})

# -------------------- Constants --------------------
region_names <- c(
  "Seoul","Incheon","Busan","Daegu","Gwangju","Gyeonggi","Gangwon","Chungbuk",
  "Chungnam","Gyeongbuk","Gyeongnam","Jeonbuk","Jeonnam","Jeju","Daejeon","Ulsan","Sejong"
)
factor_names <- c("Power","Industrial","Mobile","Residential","Agriculture","Solvent","Others")

# ------------------ Units & Labels ------------------
# O3
UNIT_O3_TEXT  <- "ppb"
O3_LABEL_TEXT <- "Ozone"

# PM2.5
PM25_LABEL_TEXT <- "PM<sub>2.5</sub>"
PM25_LABEL_HTML <- "PM<sub>2.5</sub>"

UNIT_PM_TEXT <- "µg/m³"
UNIT_PM_HTML <- "&micro;g/m<sup>3</sup>"

PM25_FULL_TEXT <- paste0(PM25_LABEL_TEXT, " (", UNIT_PM_TEXT, ")")
PM25_FULL_HTML <- paste0(PM25_LABEL_HTML, " (", UNIT_PM_HTML, ")")

# -------------------- Load spatial & model objects --------------------
asia_map <- st_read("/home/geseo/LassoCMAQ_Data/Mapping_shp/Asia_county_map.shp", quiet = TRUE)
mesh     <- st_read("/home/geseo/LassoCMAQ_Data/Mapping_shp/Mesh_test_shift2.shp", quiet = TRUE)
st_crs(asia_map) <- 4326
st_crs(mesh) <- 4326
asia_map <- st_make_valid(asia_map)
mesh     <- st_make_valid(mesh)

# -------------------- Load region map --------------------
region_map <- read.csv("/home/geseo/LassoCMAQ_Data/Grid-based Regional Allocation Ratio for 17 Municipalities_UPDATED.csv")
region_map_clean <- region_map %>%
  group_by(Column, Row) %>%
  slice_max(order_by = X., n = 1, with_ties = FALSE) %>%
  ungroup()

# -------------------- Compute CMAQ grid coordinates --------------------
nx <- 67
ny <- 82
mesh$Row    <- (mesh$FID_1 %/% nx) + 1
mesh$Column <- (mesh$FID_1 %%  nx) + 1

# -------------------- LEFT JOIN --------------------
mesh <- mesh %>%
  left_join(region_map_clean[, c("Column","Row","Region_Name")],
            by = c("Column","Row")) %>%
  st_make_valid()

# -------------------- Region outline (dissolve) --------------------
region_outline <- mesh %>%
  filter(!is.na(Region_Name), Region_Name != "") %>%
  group_by(Region_Name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid()

# -------------------- Load model objects --------------------
# Ozone
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_CMAQ_UNIQUE.RData")
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_BIAS.RData")
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_ADAPT.RData")
load("/home/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_WEIGHT.RData")

# PM2.5
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_WEIGHT.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_CMAQ_UNIQUE.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_BIAS.RData")
load("/home/geseo/LassoCMAQ_Data/PM/Total/PM_ADAPT.RData")

# -------------------- Theme & CSS --------------------
theme <- bs_theme(
  version = 5, bootswatch = "flatly",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

custom_css <- HTML("
html { scroll-behavior: smooth; scroll-padding-top: 20px; }
body { padding: 0 48px 48px 48px; }
.sticky-top { backdrop-filter: blur(6px); background: rgba(255,255,255,0.85); }
.section { scroll-margin-top: 80px; margin-top: 20px; }
.section-block { margin-top: 18px; }
.hero { padding: 28px 0 8px; margin-bottom: 4px; }
.muted { color:#6c757d; }
.copyright { border-top: 1px solid #e9ecef; padding: 12px 0; margin-top: 24px; }

.card-compact .card-header { padding: 6px 10px; }
.card-compact .card-body   { padding: 8px 10px; }
.card-compact .form-check-label,
.card-compact label { font-size: 0.92rem; }
.card-compact .form-control-sm { height: 28px; padding: 2px 6px; }

.custom-table .card-body{
  min-height: 720px;
  overflow-y: auto;
  overflow-x: hidden;
  padding: 20px;
}
.custom-table .dataTables_scrollBody{
  overflow: visible !important;
  max-height: none !important;
  height: auto !important;
}

.custom-table .dataTables_wrapper { width: 100%; }
table.dataTable { table-layout: fixed; width: 100% !important; }
table.dataTable td, table.dataTable th { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }

.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate,
.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter { display: none !important; }

table.dataTable thead th {
  vertical-align: bottom;
  background: #E5F0FB;
  border-color: #d0dcec;
  font-weight: 700;
  padding: 3px 5px !important;
  height: 26px;
  line-height: 1.15;
  font-size: 0.88rem;
}
table.dataTable thead tr.header-inputs th {
  background: #CCDFF7;
  font-weight: 600;
  height: 30px;
}
.header-input, .row-input {
  height: 22px !important;
  padding: 1px 4px !important;
  line-height: 1.1 !important;
  font-size: 0.86rem;
}
table.dataTable tbody td {
  padding: 3px 5px !important;
  height: 24px;
  font-size: 0.90rem;
  background-color: #ffffff;
}
td.rowhdr {
  background: #CCDFF7;
  border-right: 2px solid #c9d7ec;
  width: 180px !important;
  max-width: 180px !important;
}
.rowhdr .rname { font-weight: 600; margin-bottom: 4px; display:block; }
.rowhdr .row-input { width: 160px; }

.cell-wrapper { display: flex; flex-direction: column; justify-content: flex-end; height: 100%; }
.cell-wrapper .cell-input { margin-top: auto; }
.cell-input {
  height: 20px !important;
  font-size: 0.8rem !important;
  padding: 0 2px !important;
  border: 1px solid #dee2e6;
  border-radius: 3px;
}
.cell-input, .row-input, .header-input { background-color: transparent !important; }
.cell-input:focus, .row-input:focus, .header-input:focus {
  background-color: transparent !important;
  box-shadow: none;
}

.shiny-notification {
 position: fixed;
 top: 80px;
 right: 100px;
 font-size: 18px;
 padding: 16px 22px;
 border-radius: 8px;
 box-shadow: 0 4px 10px rgba(0,0,0,0.3);
 z-index: 9999;
}
")

# -------------------- UI --------------------
ui <- page_fluid(
  theme = theme,
  useShinyjs(),
  tags$head(
    tags$title("LassoCMAQ"),
    tags$style(custom_css),
    tags$script(HTML("
      window.__leafletRenderStart = {};
      Shiny.addCustomMessageHandler('markRenderStart', function(msg) {
        window.__leafletRenderStart[msg.map_id] = performance.now();
      });
      Shiny.addCustomMessageHandler('probeLeafletRender', function(msg) {
        var mapId = msg.map_id;
        requestAnimationFrame(function() {
          requestAnimationFrame(function() {
            var t0 = window.__leafletRenderStart[mapId];
            if (t0) {
              var dt = (performance.now() - t0) / 1000;
              console.log('[Leaflet browser render] ' + mapId + ': ' + dt.toFixed(3) + ' sec');
              Shiny.setInputValue('leaflet_render_done', {
                map_id: mapId,
                elapsed: dt,
                nonce: Math.random()
              }, {priority: 'event'});
            }
          });
        });
      });
    "))
  ),
  
  div(class = "sticky-top",
      layout_column_wrap(width = 1,
                         card(
                           style = "border:0; box-shadow:none; background:transparent;",
                           card_body(
                             style = "padding: 36px 0;",
                             layout_column_wrap(width = 1, fill = TRUE, gap = "0",
                                                div(class="d-flex align-items-center justify-content-between",
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
      )
  ),
  
  div(class="hero", h2("LassoCMAQ", class = "fw-semibold mb-2")),
  
  div(id = "home", class = "section",
      h3("Home", class = "fw-semibold mb-2"),
      layout_columns(col_widths = c(4,4,4),
                     card(class = "section-block",
                          card_body(
                            h5("What Is This", class="fw-bold mb-2"),
                            tags$ul(
                              tags$li("LassoCMAQ is a computationally efficient surrogate for CMAQ, developed using LASSO with an adaptive logit transformation."),
                              tags$li("It estimates Ozone or PM2.5 concentrations from regional emission-control scenarios in about 10 seconds each.")
                            )
                          )
                     ),
                     card(class = "section-block",
                          card_body(
                            h5("How to Use", class="fw-bold mb-2"),
                            tags$ul(
                              tags$li("1. Enter a 17 × 7 control policy matrix (Region × Source)."),
                              tags$li("2. Select pollutant(s) and click Run."),
                              tags$li("3. Inspect maps and summary metrics; adjust and rerun."),
                              tags$li("4. Download results as needed.")
                            )
                          )
                     ),
                     card(class = "section-block",
                          card_body(
                            h5("Citation", class="fw-bold mb-2"),
                            tags$blockquote(
                              "D.-B. Lee et al., Development of a fast and interpretable machine learning emulator for CMAQ: application to ozone and PM2.5 policy support (submitted)"
                            )
                          )
                     )
      )
  ),
  
  div(id="control", class="section",
      h3("Control Policy", class = "fw-semibold mb-2"),
      card(class = "section-block", style = "width:40%",
           card_body(
             h5("How to Set a Control Policy", class = "fw-bold mb-2"),
             tags$ul(
               tags$li("Use the control policy matrix to define emission change ratios."),
               tags$ul(
                 tags$li("Each cell = emission change ratio (Region × Source)."),
                 tags$li("Edit cells directly."),
                 tags$li("Update all cells at once."),
                 tags$li("Update a row or column at once."),
                 tags$li("Upload a control policy file.")
               )
             )
           )
      ),
      layout_columns(col_widths = c(9,3),
                     div(
                       card(header="Policy Table (17 × 7)", class="section-block custom-table",
                            DTOutput("policy_dt", width = "100%")
                       )
                     ),
                     div(
                       card(header="Upload a control policy file (.csv)", class="section-block card-upload",
                            tags$label("Upload a control policy file (.csv)", class = "form-label fw-semibold"),
                            tags$small("Example: ",
                                       tags$a(href = "sample_policy.csv", "sample_policy.csv", download = NA)
                            ),
                            fileInput("policy_upload", NULL, buttonLabel="Upload", accept = ".csv")
                       ),
                       card(header="Run Prediction", class="section-block card-compact",
                            checkboxGroupInput("pollutants","Select pollutant(s)",
                                               choices = c("Ozone" = "o3", "PM2.5" = "pm25"),
                                               selected = c("o3","pm25")),
                            actionButton("btn_run","Run", class="btn btn-outline-primary btn-sm w-100")
                       )
                     )
      )
  ),
  
  div(id = "outputs", class = "section",
      h3("Results", class = "fw-semibold mb-2"),
      div(style = "width:60%; margin-left:0; margin-top: 15px",
          progressBar(id = "pb", value = 0, total = 100, display_pct = TRUE, striped = TRUE, status = "primary")
      ),
      layout_columns(col_widths = c(6, 6),
                     card(class = "section-block",
                          h4("Ozone", class = "fw-bold mb-3"),
                          leafletOutput("o3_plot", height = "680px") %>% withSpinner(),
                          layout_columns(col_widths = c(6, 6),
                                         card(header = "Grid Average", textOutput("o3_mean")),
                                         card(header = "Summary", textOutput("o3_summary"))
                          )
                     ),
                     card(class = "section-block",
                          h4("PM2.5", class = "fw-bold mb-3"),
                          leafletOutput("pm_plot", height = "680px") %>% withSpinner(),
                          layout_columns(col_widths = c(6, 6),
                                         card(header = "Grid Average", textOutput("pm_mean")),
                                         card(header = "Summary", textOutput("pm_summary"))
                          )
                     )
      )
  ),
  
  div(id="download", class="section",
      h3("Download", class = "fw-semibold mb-2"),
      layout_columns(col_widths = c(6,6),
                     card(header="Control Policy",
                          downloadButton("dl_policy", "Download current control policy (.csv)",
                                         class="btn btn-outline-primary", style="font-size:16px")
                     ),
                     card(header="Result File",
                          downloadButton("dl_results", "Download CMAQ approximation results (.rds)",
                                         class="btn btn-outline-primary", style="font-size:16px")
                     )
      )
  ),
  
  div(class="copyright", "© Soongsil University Machine Learning Lab All Rights Reserved.")
)

# -------------------- Server --------------------
server <- function(input, output, session) {
  
  # -------------------- Logging --------------------
  log_file <- "run.log"
  log_message <- function(fmt, ...) {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    line <- sprintf(paste0("[%s] ", fmt, "\n"), ts, ...)
    cat(line)
    flush.console()
    cat(line, file = log_file, append = TRUE)
  }
  
  # Loading overlay
  w <- Waiter$new(
    id = c("o3_plot","pm_plot"),
    html = tagList(spin_fading_circles(), h4("Running prediction, please wait...")),
    color = "#ffffff"
  )
  
  # Policy matrix
  vals <- reactiveVal({
    matrix(1, nrow = length(region_names), ncol = length(factor_names),
           dimnames = list(region_names, factor_names))
  })
  
  # ---- DT helpers ----
  to_numeric_matrix <- function(df) {
    num_df <- as.data.frame(lapply(df, function(x) suppressWarnings(as.numeric(x))), check.names = FALSE)
    m <- as.matrix(num_df)
    colnames(m) <- colnames(df)
    rownames(m) <- rownames(df)
    m
  }
  
  make_region_cell <- function(i) {
    as.character(
      tags$div(class="rowhdr",
               tags$span(class="rname", region_names[i]),
               tags$input(type = "number", step = "0.1", placeholder = "All",
                          min = "0.5", max = "1.5",
                          class = "form-control form-control-sm row-input", `data-row` = i)
      )
    )
  }
  
  make_cell_input <- function(i, j, value) {
    as.character(
      tags$div(class = "cell-wrapper",
               tags$input(type = "number", step = "0.1", min = "0.5", max = "1.5",
                          value = format(value, trim = TRUE),
                          class = "form-control form-control-sm cell-input",
                          `data-row` = i, `data-col` = j)
      )
    )
  }
  
  make_table_data <- function(m) {
    df <- as.data.frame(m, check.names = FALSE)
    cell_cols <- lapply(seq_len(ncol(df)), function(j) {
      vapply(seq_len(nrow(df)), function(i) make_cell_input(i, j, df[i, j]), character(1))
    })
    names(cell_cols) <- colnames(df)
    data.frame(
      Region = vapply(seq_len(nrow(df)), make_region_cell, character(1)),
      cell_cols, check.names = FALSE, row.names = NULL
    )
  }
  
  sketch <- tags$table(
    class = "display",
    tags$thead(
      tags$tr(
        tags$th("Region"),
        lapply(factor_names, function(fn) tags$th(fn))
      ),
      tags$tr(class = "header-inputs",
              tags$th(tags$input(type = "number", step = "0.1", placeholder = "All",
                                 min = "0.5", max = "1.5",
                                 class = "form-control form-control-sm header-input all-apply")),
              lapply(seq_along(factor_names), function(j) {
                tags$th(tags$input(type = "number", step = "0.1", placeholder = "All",
                                   min = "0.5", max = "1.5",
                                   class = "form-control form-control-sm header-input col-apply",
                                   `data-col` = j))
              })
      )
    )
  )
  
  output$policy_dt <- renderDT({
    datatable(
      make_table_data(vals()),
      container = sketch, rownames = FALSE, escape = FALSE, selection = "none",
      options = list(
        dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE,
        autoWidth = FALSE, scrollX = FALSE,
        columnDefs = list(
          list(targets = 0, width = "200px", className = "rowhdr"),
          list(targets = 1:7, width = "80px")
        )
      ),
      callback = JS("
function inRange(v){ return (v >= 0.5 && v <= 1.5); }
function isNum(v){ return !isNaN(v) && isFinite(v); }

function bindRowInputs(api){
  var tbody = $(api.table().body());

  tbody.off('focusin', 'input.row-input, input.cell-input')
       .on('focusin', 'input.row-input, input.cell-input', function(){
         this.dataset.prev = this.value;
       });

  tbody.off('change', 'input.row-input')
       .on('change', 'input.row-input', function(){
         var row = parseInt($(this).attr('data-row'), 10);
         var val = parseFloat(this.value);
         if(!isNum(val) || !inRange(val)){
           var prev = (this.dataset.prev ?? '');
           this.value = prev;
           Shiny.setInputValue('range_warning', {
             message: !isNum(val) ? 'Only numeric values are allowed.'
                                   : 'Allowed range is 0.5–1.5.',
             where: 'row', row: row, prev: prev, tried: isNum(val)? val : null,
             nonce: Math.random()
           });
           return;
         }
         Shiny.setInputValue('row_apply', {row: row, val: val, nonce: Math.random()});
       });

  tbody.off('change', 'input.cell-input')
       .on('change', 'input.cell-input', function(){
         var row = parseInt($(this).attr('data-row'), 10);
         var col = parseInt($(this).attr('data-col'), 10);
         var val = parseFloat(this.value);
         if(!isNum(val) || !inRange(val)){
           var prev = (this.dataset.prev ?? '');
           this.value = prev;
           Shiny.setInputValue('range_warning', {
             message: !isNum(val) ? 'Only numeric values are allowed.'
                                   : 'Allowed range is 0.5–1.5.',
             where: 'cell', row: row, col: col, prev: prev, tried: isNum(val)? val : null,
             nonce: Math.random()
           });
           return;
         }
         Shiny.setInputValue('cell_edit', {row: row, col: col, val: val, nonce: Math.random()});
       });
}

var api = table;

$(api.table().header())
  .off('focusin', 'input.col-apply, input.all-apply')
  .on('focusin', 'input.col-apply, input.all-apply', function(){
    this.dataset.prev = this.value;
  })
  .off('change', 'input.col-apply')
  .on('change', 'input.col-apply', function(){
    var col = parseInt($(this).attr('data-col'), 10);
    var val = parseFloat(this.value);
    if(!isNum(val) || !inRange(val)){
      var prev = (this.dataset.prev ?? '');
      this.value = prev;
      Shiny.setInputValue('range_warning', {
        message: !isNum(val) ? 'Only numeric values are allowed.'
                              : 'Allowed range is 0.5–1.5.',
        where: 'col', col: col, prev: prev, tried: isNum(val)? val : null,
        nonce: Math.random()
      });
      return;
    }
    Shiny.setInputValue('col_apply', {col: col, val: val, nonce: Math.random()});
  })
  .off('change', 'input.all-apply')
  .on('change', 'input.all-apply', function(){
    var val = parseFloat(this.value);
    if(!isNum(val) || !inRange(val)){
      var prev = (this.dataset.prev ?? '');
      this.value = prev;
      Shiny.setInputValue('range_warning', {
        message: !isNum(val) ? 'Only numeric values are allowed.'
                              : 'Allowed range is 0.5–1.5.',
        where: 'all', prev: prev, tried: isNum(val)? val : null,
        nonce: Math.random()
      });
      return;
    }
    Shiny.setInputValue('all_apply', {val: val, nonce: Math.random()});
  });

bindRowInputs(api);
api.on('draw.dt', function(){ bindRowInputs(api); });
")
    )
  })
  
  observeEvent(input$range_warning, {
    showNotification(
      paste0(input$range_warning$message, " Input rejected."),
      type = "warning", duration = 4
    )
  })
  
  observeEvent(input$cell_edit, {
    info <- input$cell_edit
    i <- as.integer(info$row)
    j <- as.integer(info$col)
    v <- as.numeric(info$val)
    if (is.finite(v) && i >= 1 && j >= 1 && i <= nrow(vals()) && j <= ncol(vals())) {
      m <- vals(); m[i, j] <- v; vals(m)
    }
  })
  
  observeEvent(input$col_apply, {
    info <- input$col_apply
    j <- as.integer(info$col)
    v <- as.numeric(info$val)
    if (is.finite(v) && j >= 1 && j <= ncol(vals())) {
      m <- vals(); m[, j] <- v; vals(m)
      replaceData(dataTableProxy("policy_dt"), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
    }
  })
  
  observeEvent(input$row_apply, {
    info <- input$row_apply
    i <- as.integer(info$row)
    v <- as.numeric(info$val)
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
  
  # Upload policy
  observeEvent(input$policy_upload, {
    req(input$policy_upload)
    ext <- tolower(tools::file_ext(input$policy_upload$name))
    if (ext != "csv") {
      showModal(modalDialog(title = "Upload Error", "Only CSV files are allowed.", easyClose = TRUE))
      return()
    }
    
    tryCatch({
      df <- read.csv(input$policy_upload$datapath, row.names = 1, check.names = FALSE)
      
      if (!setequal(rownames(df), region_names) || !setequal(colnames(df), factor_names)) {
        showModal(modalDialog(
          title = "Upload Error",
          "The structure of the uploaded CSV does not match the existing policy table format.",
          easyClose = TRUE
        ))
        return()
      }
      
      df <- df[region_names, factor_names, drop = FALSE]
      m  <- to_numeric_matrix(df)
      if (!all(is.finite(m))) stop("Non-numeric values detected.")
      
      if (any(m < 0.5 | m > 1.5, na.rm = TRUE)) {
        showModal(modalDialog(title = "Upload Error", "All values must be between 0.5 and 1.5.", easyClose = TRUE))
        return()
      }
      
      vals(m)
      replaceData(dataTableProxy("policy_dt"), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
      
    }, error = function(e) {
      showModal(modalDialog(title = "Upload Error", paste("Failed to apply policy:", e$message), easyClose = TRUE))
    })
  })
  
  # -------------------- Models & prediction --------------------
  models <- list(
    o3 = list(
      WEIGHT = O3_WEIGHT, BIAS = O3_BIAS, ADAPT = O3_Adapt,
      CMAQ_UNIQUE = O3_CMAQ_UNIQUE, SCALE = 1000
    ),
    pm = list(
      WEIGHT = PM_WEIGHT, BIAS = PM_BIAS, ADAPT = PM_Adapt,
      CMAQ_UNIQUE = PM_CMAQ_UNIQUE, SCALE = 1
    )
  )
  
  linear_cache <- reactiveVal(list(
    o3 = list(control = NULL, linear = NULL),
    pm = list(control = NULL, linear = NULL)
  ))
  
  DELTA_THRESHOLD <- 10L
  
  fast_linear_vec <- function(control_vec, model, key) {
    cache <- linear_cache()[[key]]
    
    if (is.null(cache$control) || is.null(cache$linear)) {
      t0 <- Sys.time()
      linear_vec <- as.vector(matrix(control_vec, nrow = 1) %*% model$WEIGHT)
      t1 <- Sys.time()
      log_message("%s linear(full) computed: %.3f sec", key, as.numeric(difftime(t1, t0, units = "secs")))
      
      new_cache <- linear_cache()
      new_cache[[key]] <- list(control = control_vec, linear = linear_vec)
      linear_cache(new_cache)
      return(linear_vec)
    }
    
    delta <- control_vec - cache$control
    idx <- which(delta != 0)
    
    if (length(idx) == 0) {
      log_message("%s linear reused (no change)", key)
      return(cache$linear)
    }
    
    if (length(idx) <= DELTA_THRESHOLD) {
      t0 <- Sys.time()
      add <- as.vector(matrix(delta[idx], nrow = 1) %*% model$WEIGHT[idx, , drop = FALSE])
      linear_vec <- cache$linear + add
      t1 <- Sys.time()
      log_message("%s linear(delta=%d) updated: %.3f sec", key, length(idx), as.numeric(difftime(t1, t0, units = "secs")))
    } else {
      t0 <- Sys.time()
      linear_vec <- as.vector(matrix(control_vec, nrow = 1) %*% model$WEIGHT)
      t1 <- Sys.time()
      log_message("%s linear(full, delta=%d) computed: %.3f sec", key, length(idx), as.numeric(difftime(t1, t0, units = "secs")))
    }
    
    new_cache <- linear_cache()
    new_cache[[key]] <- list(control = control_vec, linear = linear_vec)
    linear_cache(new_cache)
    linear_vec
  }
  
  month_means_fast <- function(arr) {
    d <- dim(arr)
    if (is.null(d)) stop("Pred has no dim.")
    ncell <- d[1]
    mat <- matrix(arr, nrow = ncell)
    rowMeans(mat)
  }
  
  predict_with_model_fast <- function(control_vec, model, key) {
    linear_vec  <- fast_linear_vec(control_vec, model, key)
    
    t0 <- Sys.time()
    dims        <- dim(model$BIAS)
    linear_arr  <- array(linear_vec, dim = dims)
    linear_pred <- linear_arr + model$BIAS
    Pred        <- model$ADAPT / (1 + exp(-linear_pred))
    if (!is.null(model$CMAQ_UNIQUE)) Pred[model$CMAQ_UNIQUE] <- model$BIAS[model$CMAQ_UNIQUE]
    Pred <- Pred * model$SCALE
    t1 <- Sys.time()
    log_message("%s postprocess computed: %.3f sec", key, as.numeric(difftime(t1, t0, units = "secs")))
    Pred
  }
  
  result_store <- reactiveVal(list(o3=NULL, pm=NULL))
  o3_sf <- reactiveVal(NULL)
  pm_sf <- reactiveVal(NULL)
  
  # -------------------- Run prediction --------------------
  init_leaflet <- function() {
    leaflet(mesh, options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 127.8, lat = 36.2, zoom = 6) %>%
      
      addPolygons(
        layerId = ~FID_1,
        fillColor = "white",
        fillOpacity = 0.65,
        color = NA,
        stroke = FALSE,
        group = "mesh"
      ) %>%
      
      addPolygons(
        data = asia_map,
        fill = FALSE,
        color = "#444444",
        weight = 1,
        opacity = 0.9,
        group = "boundary"
      )
  }
  
  reset_leaflet <- function(map_id) {
    leafletProxy(map_id) %>%
      clearGroup("mesh") %>%
      clearControls() %>%
      setView(lng = 127.8, lat = 36.2, zoom = 6)
  }
  
  blend_white_red <- function(x, alpha = 0.6) {
    rgb(1, 1 - alpha * x, 1 - alpha * x)
  }
  
  make_red_pal <- function(x) {
    vmin <- floor(min(x, na.rm = TRUE) / 10) * 10
    vmax <- ceiling(max(x, na.rm = TRUE) / 10) * 10
    
    pal <- leaflet::colorNumeric(
      palette = "Reds",
      domain = c(vmin, vmax),
      na.color = "transparent"
    )
    
    list(pal = pal, vmin = vmin, vmax = vmax)
  }
  
  update_leaflet_map <- function(map_id, m, legend_title_html) {
    
    if (is.null(m) || nrow(m) == 0) {
      reset_leaflet(map_id)
      return(invisible(NULL))
    }
    
    pal_info <- make_red_pal(m$Year)
    pal  <- pal_info$pal
    vmin <- pal_info$vmin
    vmax <- pal_info$vmax
    
    leafletProxy(map_id) %>%
      clearGroup("mesh") %>%
      clearControls() %>%
      addPolygons(
        data = m,
        fillColor = ~pal(Year),
        fillOpacity = 0.65,
        color = NA,
        weight = 0,
        stroke = FALSE,
        smoothFactor = 0,
        group = "mesh",
        options = pathOptions(clickable = FALSE),
        
        label = ~htmltools::HTML(
          paste0(
            "<b>Region:</b> ",
            ifelse(is.na(Region_Name) | Region_Name == "", "NA", Region_Name),
            "<br><b>Value:</b> ",
            sprintf("%.2f", Year)
          )
        )
      ) %>%
      
      # mesh boundary
      addPolygons(
        data = m,
        fill = FALSE,
        color = "#777777",
        weight = 0.12,
        opacity = 0.6,
        group = "mesh"
      ) %>%
      
      # legend
      addLegend(
        pal = pal,
        values = c(vmin, vmax),
        title = htmltools::HTML(legend_title_html),
        position = "bottomright",
        opacity = 1
      )
  }
  
  output$o3_plot <- renderLeaflet(init_leaflet())
  output$pm_plot <- renderLeaflet(init_leaflet())
  
  observeEvent(input$btn_run, {
    
    runjs("document.getElementById('outputs').scrollIntoView({behavior:'smooth', block:'start'});")
    req(input$pollutants)
    
    start_time <- Sys.time()
    log_message("Run clicked: start prediction")
    
    w$show()
    updateProgressBar(session, "pb", value = 0,  title = "Initializing...")
    updateProgressBar(session, "pb", value = 5,  title = "Validating input...")
    
    m <- vals()
    if (any(!is.finite(m))) { showModal(modalDialog("All cells must be numeric.", easyClose=TRUE)); w$hide(); return() }
    if (any(m < 0.5 | m > 1.5, na.rm = TRUE)) {
      showModal(modalDialog("All values must be between 0.5 and 1.5.", easyClose=TRUE)); w$hide(); return()
    }
    
    control_vec <- as.numeric(t(m))
    need_o3 <- "o3" %in% input$pollutants
    need_pm <- "pm25" %in% input$pollutants
    
    store <- list(o3=NULL, pm=NULL)
    
    if (need_o3) {
      updateProgressBar(session, "pb", value = 20, title = "Running Ozone prediction...")
      t1 <- Sys.time()
      store$o3 <- predict_with_model_fast(control_vec, models$o3, "o3")
      t2 <- Sys.time()
      log_message("Ozone total(pred+post): %.3f sec", as.numeric(difftime(t2, t1, units = "secs")))
      
      t3 <- Sys.time()
      m_o3 <- mesh
      m_o3$Year <- month_means_fast(store$o3)
      m_o3 <- st_make_valid(m_o3)
      o3_sf(m_o3)
      t4 <- Sys.time()
      log_message("Ozone mean+sf attach: %.3f sec", as.numeric(difftime(t4, t3, units = "secs")))
      
      updateProgressBar(session, "pb", value = if (need_pm) 45 else 80, title = "Ozone prediction finished")
    } else {
      o3_sf(NULL)
    }
    
    if (need_pm) {
      updateProgressBar(
        session, "pb",
        value = if (need_o3) 50 else 20,
        title = paste0("Running ", PM25_LABEL_TEXT, " prediction...")
      )
      
      t1 <- Sys.time()
      store$pm <- predict_with_model_fast(control_vec, models$pm, "pm")
      t2 <- Sys.time()
      log_message("PM2.5 total(pred+post): %.3f sec", as.numeric(difftime(t2, t1, units = "secs")))
      
      t3 <- Sys.time()
      m_pm <- mesh
      m_pm$Year <- month_means_fast(store$pm)
      m_pm <- st_make_valid(m_pm)
      pm_sf(m_pm)
      t4 <- Sys.time()
      log_message("PM2.5 mean+sf attach: %.3f sec", as.numeric(difftime(t4, t3, units = "secs")))
      
      updateProgressBar(session, "pb", value = if (need_o3) 75 else 80, title = paste0(PM25_LABEL_TEXT, " prediction finished"))
    } else {
      pm_sf(NULL)
    }
    
    result_store(store)
    updateProgressBar(session, "pb", value = 100, title = "Completed!")
    w$hide()
    
    end_time <- Sys.time()
    log_message("Total run time: %.3f sec", as.numeric(difftime(end_time, start_time, units = "secs")))
  })
  
  # -------------------- Leaflet helpers & maps --------------------
  observeEvent(o3_sf(), ignoreInit = TRUE, {
    m <- o3_sf()
    if (is.null(m)) {
      reset_leaflet("o3_plot")
      return()
    }
    
    session$sendCustomMessage("markRenderStart", list(map_id = "o3_plot"))
    t0 <- Sys.time()
    update_leaflet_map("o3_plot", m, paste0("Ozone (", UNIT_O3_TEXT, ")"))
    t1 <- Sys.time()
    log_message("Server leaflet build: o3 = %.3f sec", as.numeric(difftime(t1, t0, units = "secs")))
    session$sendCustomMessage("probeLeafletRender", list(map_id = "o3_plot"))
  })
  
  observeEvent(pm_sf(), ignoreInit = TRUE, {
    m <- pm_sf()
    if (is.null(m)) {
      reset_leaflet("pm_plot")
      return()
    }
    
    session$sendCustomMessage("markRenderStart", list(map_id = "pm_plot"))
    t0 <- Sys.time()
    update_leaflet_map("pm_plot", m, PM25_FULL_HTML)
    t1 <- Sys.time()
    log_message("Server leaflet build: pm = %.3f sec", as.numeric(difftime(t1, t0, units = "secs")))
    session$sendCustomMessage("probeLeafletRender", list(map_id = "pm_plot"))
  })
  
  observeEvent(input$leaflet_render_done, {
    info <- input$leaflet_render_done
    log_message("Plot finished rendering in browser: %s (%.3f sec)", info$map_id, as.numeric(info$elapsed))
  })
  
  output$o3_mean <- renderText({
    m <- o3_sf()
    req(!is.null(m))
    paste0("Annual average across all cells: ",
           sprintf("%.1f %s", mean(m$Year, na.rm = TRUE), UNIT_O3_TEXT))
  })
  
  output$o3_summary <- renderText({
    m <- o3_sf()
    req(!is.null(m))
    rng <- range(m$Year, na.rm = TRUE)
    paste0("Annual range across all cells: ",
           sprintf("%.1f – %.1f %s", rng[1], rng[2], UNIT_O3_TEXT))
  })
  
  output$pm_mean <- renderText({
    m <- pm_sf()
    req(!is.null(m))
    paste0("Annual average across all cells: ",
           sprintf("%.1f %s", mean(m$Year, na.rm = TRUE), UNIT_PM_TEXT))
  })
  
  output$pm_summary <- renderText({
    m <- pm_sf()
    req(!is.null(m))
    rng <- range(m$Year, na.rm = TRUE)
    paste0("Annual range across all cells: ",
           sprintf("%.1f – %.1f %s", rng[1], rng[2], UNIT_PM_TEXT))
  })
  
  # -------------------- Downloads --------------------
  output$dl_policy <- downloadHandler(
    filename = function() paste0("control_policy_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      m  <- vals()
      df <- as.data.frame(m, check.names = FALSE)
      df_out <- cbind(Region = rownames(df), df)
      utils::write.csv(df_out, file, row.names = FALSE, na = "")
    }
  )
  
  output$dl_results <- downloadHandler(
    filename = function() paste0("prediction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content  = function(file) { saveRDS(result_store(), file) }
  )
}

# -------------------- Run --------------------
shinyApp(ui, server)