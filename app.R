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
  library(ggplot2)
  library(base64enc)
  library(scales)
  library(RColorBrewer)
  library(filelock)
  library(future)
  library(promises)
})

plan(multisession, workers = 1)

# -------------------- Constants --------------------
region_names <- c(
  "Incheon","Gyeonggi","Seoul","Gangwon","Chungbuk","Sejong","Daejeon",
  "Chungnam","Gyeongbuk","Daegu","Ulsan","Busan","Gyeongnam","Jeonbuk","Gwangju","Jeonnam","Jeju"
)
factor_names <- c("Power","Industry","Residential","Solvent","Mobile","Agriculture","Others")

region_names_model <- c(
  "Seoul","Incheon","Busan","Daegu","Gwangju","Gyeonggi","Gangwon","Chungbuk",
  "Chungnam","Gyeongbuk","Gyeongnam","Jeonbuk","Jeonnam","Jeju","Daejeon","Ulsan","Sejong"
)
factor_names_model <- c("Power","Industrial","Mobile","Residential","Agriculture","Solvent","Others")

factor_name_map <- c(
  "Power"="Power", "Industry"="Industrial", "Residential"="Residential",
  "Solvent"="Solvent", "Mobile"="Mobile", "Agriculture"="Agriculture", "Others"="Others"
)

# -------------------- Global execution lock --------------------
LOCK_PATH   <- "/tmp/lassocmaq_prediction.lock"
STATUS_PATH <- "/tmp/lassocmaq_status.txt"

writeLines("IDLE", STATUS_PATH)

set_global_status <- function(status) {
  try(writeLines(status, STATUS_PATH), silent = TRUE)
}

read_global_status <- function() {
  tryCatch(readLines(STATUS_PATH, n = 1, warn = FALSE), error = function(e) "IDLE")
}

acquire_global_lock <- function(timeout = 0) {
  dir.create(dirname(LOCK_PATH), recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(LOCK_PATH)) {
    ok <- file.create(LOCK_PATH)
    if (!ok) stop("Failed to create lock file.")
  }
  
  result <- tryCatch(
    filelock::lock(LOCK_PATH, timeout = timeout),
    error = function(e) NULL
  )
  result
}

release_global_lock <- function(lock_obj) {
  if (!is.null(lock_obj)) {
    try(filelock::unlock(lock_obj), silent = TRUE)
  }
}

# ------------------ Units & Labels ------------------
# O3
UNIT_O3_TEXT  <- "ppb"
O3_LABEL_TEXT <- "Ozone"

# PM₂.₅ 
PM25_LABEL_TEXT <- "PM₂.₅"
PM25_LABEL_HTML <- "PM₂.₅"

UNIT_PM_TEXT <- "µg/m³"
UNIT_PM_HTML <- "&micro;g/m<sup>3</sup>"

PM25_FULL_TEXT <- paste0(PM25_LABEL_TEXT, " (", UNIT_PM_TEXT, ")")
PM25_FULL_HTML <- paste0(PM25_LABEL_HTML, " (", UNIT_PM_HTML, ")")

# -------------------- Load spatial & model objects --------------------
asia_map <- st_read("/ext_hdd_data1/geseo/LassoCMAQ_Data/Mapping_shp/Asia_county_map.shp", quiet = TRUE)
mesh     <- st_read("/ext_hdd_data1/geseo/LassoCMAQ_Data/Mapping_shp/Mesh_test_shift2.shp", quiet = TRUE)
st_crs(asia_map) <- 4326
st_crs(mesh) <- 4326
asia_map <- st_make_valid(asia_map)
mesh     <- st_make_valid(mesh)

# -------------------- Load region map --------------------
region_map <- read.csv("/ext_hdd_data1/geseo/LassoCMAQ_Data/Grid-based Regional Allocation Ratio for 17 Municipalities_UPDATED.csv")
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

# -------------------- Load weight summary --------------------
O3_weight_summary <- read.csv(
  "/ext_hdd_data1/geseo/LassoCMAQ_Data/O3_Weight_Summary.csv",
  stringsAsFactors = FALSE
)

PM_weight_summary <- read.csv(
  "/ext_hdd_data1/geseo/LassoCMAQ_Data/PM_Weight_Summary.csv",
  stringsAsFactors = FALSE
)

# -------------------- Sector mapping --------------------
sector_map <- c(
  "POW" = "Power",
  "IND" = "Industry",
  "MO"  = "Mobile",
  "RE"  = "Residential",
  "ARG"  = "Agriculture",
  "SOL" = "Solvent",
  "OTH" = "Others"
)

sector_colors <- c(
  "Power"       = "#D3D3E8",
  "Industry"    = "#FFC300",
  "Mobile"      = "#FFFFB3",
  "Residential" = "#A2D9CE",
  "Agriculture" = "#E3B8EA",
  "Solvent"     = "#FF9999",
  "Others"      = "#C2B280"
)

# -------------------- Load model objects --------------------
# Ozone
load("/ext_hdd_data1/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_CMAQ_UNIQUE.RData")
load("/ext_hdd_data1/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_BIAS.RData")
load("/ext_hdd_data1/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_ADAPT.RData")
load("/ext_hdd_data1/geseo/LassoCMAQ_Data/O3/Adaptive_logit/Total/O3_WEIGHT.RData")

# PM₂.₅ 
load("/ext_hdd_data1/geseo/LassoCMAQ_Data/PM/Total/PM_WEIGHT.RData")
load("/ext_hdd_data1/geseo/LassoCMAQ_Data/PM/Total/PM_CMAQ_UNIQUE.RData")
load("/ext_hdd_data1/geseo/LassoCMAQ_Data/PM/Total/PM_BIAS.RData")
load("/ext_hdd_data1/geseo/LassoCMAQ_Data/PM/Total/PM_ADAPT.RData")

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

sub {
  line-height: 0;
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
      document.addEventListener('DOMContentLoaded', function() {
        window.scrollTo(0, 0);
      });
      $(document).on('shiny:connected', function() {
        window.scrollTo(0, 0);
      });
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

      window.__policyScroll = { pageY: 0, tableY: 0 };

      Shiny.addCustomMessageHandler('savePolicyScroll', function(msg) {
        window.__policyScroll.pageY = window.scrollY || window.pageYOffset || 0;
      });
      
      Shiny.addCustomMessageHandler('restorePolicyScroll', function(msg) {
        var savedY = window.__policyScroll.pageY || 0;
        requestAnimationFrame(function() {
          requestAnimationFrame(function() {
            window.scrollTo(0, savedY);
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
                                                        tags$a(href="#control",  class="link-dark text-decoration-none", "Control Scenario"),
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
                            h5("What Is LassoCMAQ", class="fw-bold mb-2"),
                            tags$ul(
                              tags$li("LassoCMAQ is a computationally efficient reduced-form CMAQ model, developed using the least absolute shrinkage and selection operator (LASSO) together with an adaptive logit transformation of the response variable."),
                              tags$li("It estimates ozone and PM₂.₅ concentrations from regional emission-control scenarios in about 30 seconds per scenario. The model computes concentrations for every grid cell at every hour, enabling rapid what-if exploration without running CMAQ.")
                            )
                          )
                     ),
                     card(class = "section-block",
                          card_body(
                            h5("How to Use LassoCMAQ", class="fw-bold mb-2"),
                            tags$ol(
                              tags$li("Enter a 17 × 7 emission scenario matrix (Region × Emission Sector) specifying emission change ratios (e.g., 0.9 = 10% reduction from the baseline)."),
                              tags$li("Select pollutant(s) and click Run to estimate CMAQ-equivalent concentrations for the selected scenario."),
                              tags$li("Inspect maps and summary metrics; click a grid cell to view the top five influential variables for the corresponding region."),
                              tags$li("Download the scenario inputs and the full model results as needed.")
                            )
                          )
                     ),
                     card(class = "section-block",
                          card_body(
                            h5("Citation", class="fw-bold mb-2"),
                            tags$blockquote(
                              "D.-B. Lee et al., A LASSO-based reduced-form CMAQ model for predicting ozone and PM₂.₅ responses to emission changes in South Korea (submitted)"
                            )
                          )
                     )
      )
  ),
  
  div(id="control", class="section",
      h3("Control Scenario", class = "fw-semibold mb-2"),
      card(class = "section-block", style = "width:40%",
           card_body(
             h5("How to Set an Emission Scenario", class = "fw-bold mb-2"),
             tags$ul(
               tags$li("Use the emission scenario matrix to define emission change ratios. Each cell represents an emission change ratio (Region × Emission Sector)."),
               tags$ul(
                 tags$li("Edit cells directly."),
                 tags$li("Update all cells at once."),
                 tags$li("Update a row or column at once."),
                 tags$li("Upload an emission scenario file.")
               )
             )
           )
      ),
      layout_columns(col_widths = c(9,3),
                     div(
                       card(header="Scenario Table (17 × 7)", class="section-block custom-table",
                            DTOutput("policy_dt", width = "100%")
                       )
                     ),
                     div(
                       card(header="Upload a control scenario file (.csv)", class="section-block card-upload",
                            tags$label("Upload a control scenario file (.csv)", class = "form-label fw-semibold"),
                            tags$small("Example: ",
                                       tags$a(href = "sample_scenario.csv", "sample_scenario.csv", download = NA)
                            ),
                            fileInput("scenario_upload", NULL, buttonLabel="Upload", accept = ".csv")
                       ),
                       card(header="Run Prediction", class="section-block card-compact",
                            checkboxGroupInput("pollutants","Select pollutant(s)",
                                               choices = c("Ozone" = "o3", "PM₂.₅" = "pm25"),
                                               selected = c("o3","pm25")),
                            actionButton("btn_run","Run", class="btn btn-outline-primary btn-sm w-100")
                       )
                     )
      )
  ),
  
  div(id = "outputs", class = "section",
      h3("Results", class = "fw-semibold mb-2"),
      div(style = "width:60%; margin-left:0; margin-top: 15px",
          progressBar(id = "pb", value = 0, display_pct = TRUE, striped = TRUE, status = "primary")
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
                          h4(HTML("PM₂.₅"), class = "fw-bold mb-3"),
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
                     card(header="Control Scenario",
                          downloadButton("dl_scenario", "Download current control scenario (.csv)",
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
  # -------------------- Session --------------------
  is_running <- reactiveVal(FALSE)
  
  pending_run <- reactiveVal(NULL)
  observe({
    shinyjs::toggleState("btn_run", condition = !is_running())
  })
  
  global_status <- reactivePoll(
    intervalMillis = 500,
    session        = session,
    checkFunc      = function() read_global_status(),
    valueFunc      = function() read_global_status()
  )
  
  observe({
    busy <- (global_status() == "BUSY") || is_running()
    shinyjs::toggleState("btn_run", condition = !busy)
  })
  
  observeEvent(global_status(), ignoreInit = TRUE, {
    if (global_status() == "IDLE" && !is.null(pending_run()) && !is_running()) {
      req_data <- pending_run()
      pending_run(NULL)
      removeModal()
      log_message("IDLE detected: firing pending prediction")
      do_prediction(req_data$control_vec, req_data$need_o3, req_data$need_pm)
    }
  })
  
  # Logging
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
                          value = if (value == 1) "1.0" else format(value, trim = TRUE),
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
      make_table_data(isolate(vals())),  # add isolate
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
  
  dt_trigger <- reactiveVal(0)
  
  observe({
    vals()
    dt_trigger(isolate(dt_trigger()) + 1)
  })
  
  observeEvent(dt_trigger(), ignoreInit = FALSE, {
    proxy <- dataTableProxy("policy_dt")
    replaceData(proxy, make_table_data(isolate(vals())), resetPaging = FALSE, rownames = FALSE)
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
      session$sendCustomMessage("savePolicyScroll", list())
      m <- vals(); m[i, j] <- v; vals(m)
      session$sendCustomMessage("restorePolicyScroll", list())
    }
  })
  
  observeEvent(input$col_apply, {
    info <- input$col_apply
    j <- as.integer(info$col)
    v <- as.numeric(info$val)
    if (is.finite(v) && j >= 1 && j <= ncol(vals())) {
      session$sendCustomMessage("savePolicyScroll", list())
      m <- vals(); m[, j] <- v; vals(m)
      session$sendCustomMessage("restorePolicyScroll", list())
      runjs(sprintf(
        "$('input.col-apply[data-col=\"%d\"]').val('');", j
      ))
    }
  })
  
  observeEvent(input$row_apply, {
    info <- input$row_apply
    i <- as.integer(info$row)
    v <- as.numeric(info$val)
    if (is.finite(v) && i >= 1 && i <= nrow(vals())) {
      session$sendCustomMessage("savePolicyScroll", list())
      m <- vals(); m[i, ] <- v; vals(m)
      session$sendCustomMessage("restorePolicyScroll", list())
      runjs(sprintf(
        "$('input.row-input[data-row=\"%d\"]').val('');", i
      ))
    }
  })
  
  observeEvent(input$all_apply, {
    v <- as.numeric(input$all_apply$val)
    if (is.finite(v)) {
      session$sendCustomMessage("savePolicyScroll", list())
      m <- vals(); m[, ] <- v; vals(m)
      session$sendCustomMessage("restorePolicyScroll", list())
      runjs("$('input.all-apply').val('');")
    }
  })
  
  observeEvent(input$scenario_upload, {
    req(input$scenario_upload)
    ext <- tolower(tools::file_ext(input$scenario_upload$name))
    if (ext != "csv") {
      showModal(modalDialog(title = "Upload Error", "Only CSV files are allowed.", easyClose = TRUE))
      return()
    }
    
    tryCatch({
      df <- read.csv(input$scenario_upload$datapath, row.names = 1, check.names = FALSE)
      
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
      
      session$sendCustomMessage("savePolicyScroll", list())
      vals(m)
      session$sendCustomMessage("restorePolicyScroll", list())
      
    }, error = function(e) {
      showModal(modalDialog(title = "Upload Error", paste("Failed to apply policy:", e$message), easyClose = TRUE))
    })
  })
  
  # ── Models & reactive stores ───────────────────────────────────────────────
  models <- list(
    o3 = list(WEIGHT=O3_WEIGHT, BIAS=O3_BIAS, ADAPT=O3_Adapt, CMAQ_UNIQUE=O3_CMAQ_UNIQUE, SCALE=1000),
    pm = list(WEIGHT=PM_WEIGHT, BIAS=PM_BIAS, ADAPT=PM_Adapt, CMAQ_UNIQUE=PM_CMAQ_UNIQUE, SCALE=1)
  )
  
  linear_cache <- reactiveVal(list(
    o3 = list(control=NULL, linear=NULL),
    pm = list(control=NULL, linear=NULL)
  ))
  
  result_store <- reactiveVal(list(o3=NULL, pm=NULL))
  
  o3_sf        <- reactiveVal(NULL)
  pm_sf        <- reactiveVal(NULL)
  
  selected_o3 <- reactiveVal(FALSE)
  selected_pm <- reactiveVal(FALSE)
  
  do_prediction <- function(control_vec, need_o3, need_pm) {
    lock_obj <- acquire_global_lock(timeout = 0)
    if (is.null(lock_obj)) {
      log_message("do_prediction: lock contention, re-queuing")
      pending_run(list(control_vec=control_vec, need_o3=need_o3, need_pm=need_pm))
      return()
    }
    
    log_message("Global lock acquired")
    is_running(TRUE)
    set_global_status("BUSY")
    w$show()
    updateProgressBar(session, "pb", value=0,  title="Initializing...")
    updateProgressBar(session, "pb", value=10, title="Running prediction...")
    
    .models    <- models
    .mesh      <- mesh
    start_time <- Sys.time()
    
    fut <- future({
      run_result <- tryCatch({
        store <- list(o3=NULL, pm=NULL)
        if (need_o3) {
          t1       <- Sys.time()
          lv       <- as.vector(matrix(control_vec, nrow=1) %*% .models$o3$WEIGHT)
          la       <- array(lv, dim=dim(.models$o3$BIAS))
          pred     <- .models$o3$ADAPT / (1 + exp(-(la + .models$o3$BIAS)))
          if (!is.null(.models$o3$CMAQ_UNIQUE)) pred[.models$o3$CMAQ_UNIQUE] <- .models$o3$BIAS[.models$o3$CMAQ_UNIQUE]
          pred     <- pred * .models$o3$SCALE
          store$o3 <- rowMeans(matrix(pred, nrow=dim(pred)[1]))
          message(sprintf("[worker] O3: %.3f sec", as.numeric(difftime(Sys.time(), t1, units="secs"))))
        }
        if (need_pm) {
          t1       <- Sys.time()
          lv       <- as.vector(matrix(control_vec, nrow=1) %*% .models$pm$WEIGHT)
          la       <- array(lv, dim=dim(.models$pm$BIAS))
          pred     <- .models$pm$ADAPT / (1 + exp(-(la + .models$pm$BIAS)))
          if (!is.null(.models$pm$CMAQ_UNIQUE)) pred[.models$pm$CMAQ_UNIQUE] <- .models$pm$BIAS[.models$pm$CMAQ_UNIQUE]
          pred     <- pred * .models$pm$SCALE
          store$pm <- rowMeans(matrix(pred, nrow=dim(pred)[1]))
          message(sprintf("[worker] PM₂.₅: %.3f sec", as.numeric(difftime(Sys.time(), t1, units="secs"))))
        }
        list(ok=TRUE, store=store)
      }, error = function(e) list(ok=FALSE, msg=e$message))
      run_result
    }, globals = list(control_vec=control_vec, need_o3=need_o3, need_pm=need_pm, .models=.models),
    packages = c("sf","dplyr"))
    
    promises::then(fut,
                   onFulfilled = function(res) {
                     release_global_lock(lock_obj)
                     set_global_status("IDLE")
                     log_message("Global lock released")
                     is_running(FALSE)
                     w$hide()
                     
                     if (!res$ok) {
                       log_message("Async prediction failed: %s", res$msg)
                       showModal(modalDialog(title="Prediction Error", paste("An error occurred:", res$msg), easyClose=TRUE))
                       return()
                     }
                     
                     store <- res$store
                     
                     if (need_o3 && !is.null(store$o3)) {
                       updateProgressBar(session, "pb", value=70, title="Prediction done. Building map...")
                       m_o3 <- .mesh; m_o3$Year <- store$o3; o3_sf(sf::st_make_valid(m_o3))
                     } else { o3_sf(NULL) }
                     
                     if (need_pm && !is.null(store$pm)) {
                       m_pm <- .mesh; m_pm$Year <- store$pm; pm_sf(sf::st_make_valid(m_pm))
                     } else { pm_sf(NULL) }
                     
                     result_store(store)
                     
                     nc <- linear_cache()
                     if (need_o3) nc$o3 <- list(control=control_vec, linear=as.vector(matrix(control_vec,nrow=1) %*% models$o3$WEIGHT))
                     if (need_pm) nc$pm <- list(control=control_vec, linear=as.vector(matrix(control_vec,nrow=1) %*% models$pm$WEIGHT))
                     linear_cache(nc)
                     
                     result_store(store)
                     log_message("Total run time: %.3f sec", as.numeric(difftime(Sys.time(), start_time, units="secs")))
                   },
                   onRejected = function(err) {
                     release_global_lock(lock_obj)
                     set_global_status("IDLE")
                     log_message("Global lock released (on rejection)")
                     is_running(FALSE)
                     w$hide()
                     log_message("Future rejected: %s", conditionMessage(err))
                     showModal(modalDialog(title="Prediction Error", paste("An unexpected error occurred:", conditionMessage(err)), easyClose=TRUE))
                   }
    )
    
    NULL
  }
  
  # ── btn_run ────────────────────────────────────────────────────────────────
  observeEvent(input$btn_run, {
    req(input$pollutants)
    
    m <- vals()
    if (any(!is.finite(m))) { showModal(modalDialog("All cells must be numeric.", easyClose=TRUE)); return() }
    if (any(m < 0.5 | m > 1.5, na.rm=TRUE)) { showModal(modalDialog("All values must be between 0.5 and 1.5.", easyClose=TRUE)); return() }
    
    m_model <- m[region_names_model, ]
    colnames(m_model) <- factor_name_map[colnames(m_model)]
    m_model <- m_model[, factor_names_model]
    control_vec <- as.numeric(t(m_model))
    
    need_o3     <- "o3"   %in% input$pollutants
    need_pm     <- "pm25" %in% input$pollutants
    
    selected_o3(need_o3)
    selected_pm(need_pm)
    
    if (read_global_status() == "BUSY") {
      pending_run(list(control_vec=control_vec, need_o3=need_o3, need_pm=need_pm))
      log_message("Run queued: another user is running a prediction")
      showModal(modalDialog(
        title = "Another Prediction in Progress",
        tagList(
          tags$p("Another user is running a prediction."),
          tags$p(
            "Your scenario has been saved and will run automatically ",
            "once the current prediction is complete."
          ),
          tags$p("You can track its progress using the progress bar.")
        ),
        footer    = modalButton("Close"),
        easyClose = TRUE
      ))
      return()
    }
    
    reset_leaflet("o3_plot")
    reset_leaflet("pm_plot")
    
    runjs("document.getElementById('outputs').scrollIntoView({behavior:'smooth', block:'start'});")
    do_prediction(control_vec, need_o3, need_pm)
  })
  
  # ── Leaflet ────────────────────────────────────────────────────────────────
  init_leaflet <- function() {
    leaflet(options = leafletOptions(preferCanvas=FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=127.8, lat=36.2, zoom=6) %>%
      addPolygons(data=asia_map, fill=FALSE, color="#444444", weight=1, opacity=0.9,
                  group="boundary", options=pathOptions(interactive=FALSE))
  }
  
  reset_leaflet <- function(map_id) {
    leafletProxy(map_id) %>%
      clearGroup("mesh") %>% clearControls() %>% clearPopups() %>%
      setView(lng=127.8, lat=36.2, zoom=6)
  }
  
  make_red_pal <- function(x) {
    vmin <- floor(min(x, na.rm=TRUE) / 10) * 10
    vmax <- ceiling(max(x, na.rm=TRUE) / 10) * 10
    pal  <- leaflet::colorNumeric(palette=RColorBrewer::brewer.pal(9,"Reds"), domain=c(vmin,vmax), na.color="transparent")
    list(pal=pal, vmin=vmin, vmax=vmax)
  }
  
  update_leaflet_map <- function(map_id, m, legend_title_html) {
    m <- st_make_valid(m); m <- m[!sf::st_is_empty(m), ]
    if (nrow(m) == 0 || all(is.na(m$Year))) { reset_leaflet(map_id); return(invisible(NULL)) }
    
    bb       <- st_bbox(m)
    pal_info <- make_red_pal(m$Year)
    pal      <- pal_info$pal; vmin <- pal_info$vmin; vmax <- pal_info$vmax
    pal_rev  <- leaflet::colorNumeric(palette=rev(RColorBrewer::brewer.pal(9,"Reds")), domain=c(vmin,vmax))
    
    leafletProxy(map_id, data = m) %>%
      clearGroup("mesh") %>%
      clearControls() %>%
      clearPopups() %>%
      fitBounds(
        lng1 = bb["xmin"], lat1 = bb["ymin"],
        lng2 = bb["xmax"], lat2 = bb["ymax"]
      ) %>%
      addPolygons(
        # Fill
        fillColor   = ~pal(Year),
        fillOpacity = 0.7,
        # Border
        color   = "#555555",
        weight  = 0.4,
        opacity = 0.5,
        # Identity
        group   = "mesh",
        layerId = ~paste0(Row, "_", Column),
        # Hover label
        label        = ~sprintf("Region: %s, Value: %.1f", Region_Name, Year),
        labelOptions = labelOptions(
          direction = "auto",
          textsize  = "13px",
          noHide    = FALSE,
          sticky    = FALSE,
          style     = list("font-weight" = "normal", "padding" = "4px 8px")
        ),
        # Hover highlight
        highlightOptions = highlightOptions(
          weight      = 0.4,
          opacity     = 0.5,
          fillOpacity = 0.7,
          bringToFront = FALSE
        )
      ) %>%
      addLegend(
        pal      = pal_rev,
        values   = c(vmin, vmax),
        title    = htmltools::HTML(legend_title_html),
        position = "bottomright",
        opacity  = 1,
        labFormat = labelFormat(
          transform = function(x) sort(x, decreasing = TRUE)
        )
      )
  }
  
  output$o3_plot <- renderLeaflet(init_leaflet())
  output$pm_plot <- renderLeaflet(init_leaflet())
  
  observeEvent(o3_sf(), ignoreInit=TRUE, ignoreNULL=FALSE, {
    m <- o3_sf(); if (is.null(m)) { reset_leaflet("o3_plot"); return() }
    updateProgressBar(session, "pb", value=80, title="Ozone: rendering map...")  # 추가
    session$sendCustomMessage("markRenderStart", list(map_id="o3_plot"))
    t0 <- Sys.time()
    update_leaflet_map("o3_plot", m, paste0("Ozone (", UNIT_O3_TEXT, ")"))
    log_message("Server leaflet build: o3 = %.3f sec", as.numeric(difftime(Sys.time(), t0, units="secs")))
    session$sendCustomMessage("probeLeafletRender", list(map_id="o3_plot"))
  })
  
  observeEvent(pm_sf(), ignoreInit=TRUE, ignoreNULL=FALSE, {
    m <- pm_sf(); if (is.null(m)) { reset_leaflet("pm_plot"); return() }
    val <- if (selected_o3()) 90 else 80
    updateProgressBar(session, "pb", value=val, title="PM₂.₅: rendering map...")  # 추가
    session$sendCustomMessage("markRenderStart", list(map_id="pm_plot"))
    t0 <- Sys.time()
    update_leaflet_map("pm_plot", m, PM25_FULL_HTML)
    log_message("Server leaflet build: pm = %.3f sec", as.numeric(difftime(Sys.time(), t0, units="secs")))
    session$sendCustomMessage("probeLeafletRender", list(map_id="pm_plot"))
  })
  
  observeEvent(input$leaflet_render_done, {
    info <- input$leaflet_render_done
    if (!selected_o3() || !selected_pm() ||
        (selected_o3() && selected_pm() && info$map_id == "pm_plot")) {
      updateProgressBar(session, "pb", value=100, title="Completed!")  # 추가
    }
    log_message("Plot finished rendering in browser: %s (%.3f sec)", info$map_id, as.numeric(info$elapsed))
  })
  
  # ── Weight popup ───────────────────────────────────────────────────────────
  get_weight_top5 <- function(region, pollutant) {
    df <- if (pollutant == "o3") O3_weight_summary else PM_weight_summary
    df %>%
      dplyr::filter(Target_Region == region) %>%
      dplyr::arrange(desc(Weight_Ratio)) %>%
      dplyr::slice_head(n=5) %>%
      dplyr::mutate(
        SectorFull = unname(sector_map[Input_Sector]),
        SectorFull = ifelse(is.na(SectorFull), "Others", SectorFull),
        Label      = paste(SectorFull, Input_Region, sep="\n"),
        TextColor2 = ifelse(tolower(TextColor) == "red", "#FF0000", "#2E8B57")
      )
  }
  
  make_weight_plot <- function(region, pollutant) {
    df <- get_weight_top5(region, pollutant)
    if (nrow(df) == 0) return(NULL)
    top5_sum <- sum(df$Weight_Ratio, na.rm=TRUE)
    xmax     <- max(df$Weight_Ratio, na.rm=TRUE) * 1.40
    ggplot(df, aes(x=Weight_Ratio, y=reorder(Label, Weight_Ratio), fill=SectorFull)) +
      geom_col(width=0.7, color="black") +
      geom_text(aes(label=sprintf("%.1f%%", Weight_Ratio), color=TextColor2),
                hjust=-0.08, size=5, show.legend=FALSE, fontface="bold") +
      scale_fill_manual(values=sector_colors) + scale_color_identity() +
      scale_x_continuous(limits=c(0,xmax), expand=expansion(mult=c(0,0.02))) +
      labs(title=paste0(region), x="Ratio (%)", y="Sector-Region") +
      annotate("label", x=xmax*0.95, y=0.56, label=sprintf("%.1f%%", top5_sum), size=5, fontface="bold") +
      theme_bw(base_size=16) +
      theme(legend.position="none",
            plot.title=element_text(face="bold", hjust=0.5),
            axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"),
            axis.text.x=element_text(face="bold"),  axis.text.y=element_text(face="bold"),
            panel.grid.major.y=element_blank())
  }
  
  plot_to_popup <- function(plot_obj) {
    if (is.null(plot_obj)) return(htmltools::HTML("<div>No data</div>"))
    tmp <- tempfile(fileext=".png")
    png(tmp, width=900, height=520, res=110); print(plot_obj); dev.off()
    htmltools::HTML(paste0("<img src='", base64enc::dataURI(file=tmp, mime="image/png"), "' width='500px'>"))
  }
  
  observeEvent(input$o3_plot_shape_click, ignoreInit=TRUE, {
    id <- input$o3_plot_shape_click$id; req(id)
    rc <- strsplit(id,"_")[[1]]; r <- as.numeric(rc[1]); c <- as.numeric(rc[2])
    region <- mesh %>% dplyr::filter(Row==r, Column==c) %>% dplyr::pull(Region_Name) %>% unique()
    req(length(region) > 0)
    leafletProxy("o3_plot") %>% clearPopups() %>%
      addPopups(lng=input$o3_plot_shape_click$lng, lat=input$o3_plot_shape_click$lat,
                popup=plot_to_popup(make_weight_plot(region[1],"o3")), options=popupOptions(maxWidth=560))
  })
  
  observeEvent(input$pm_plot_shape_click, ignoreInit=TRUE, {
    id <- input$pm_plot_shape_click$id; req(id)
    rc <- strsplit(id,"_")[[1]]; r <- as.numeric(rc[1]); c <- as.numeric(rc[2])
    region <- mesh %>% dplyr::filter(Row==r, Column==c) %>% dplyr::pull(Region_Name) %>% unique()
    req(length(region) > 0)
    leafletProxy("pm_plot") %>% clearPopups() %>%
      addPopups(lng=input$pm_plot_shape_click$lng, lat=input$pm_plot_shape_click$lat,
                popup=plot_to_popup(make_weight_plot(region[1],"pm")), options=popupOptions(maxWidth=560))
  })
  
  # ── Text outputs ───────────────────────────────────────────────────────────
  output$o3_mean <- renderText({
    m <- o3_sf(); req(!is.null(m))
    paste0("Annual average across all cells: ", sprintf("%.1f %s", mean(m$Year, na.rm=TRUE), UNIT_O3_TEXT))
  })
  output$o3_summary <- renderText({
    m <- o3_sf(); req(!is.null(m)); rng <- range(m$Year, na.rm=TRUE)
    paste0("Annual range across all cells: ", sprintf("%.1f - %.1f %s", rng[1], rng[2], UNIT_O3_TEXT))
  })
  output$pm_mean <- renderText({
    m <- pm_sf(); req(!is.null(m))
    paste0("Annual average across all cells: ", sprintf("%.1f %s", mean(m$Year, na.rm=TRUE), UNIT_PM_TEXT))
  })
  output$pm_summary <- renderText({
    m <- pm_sf(); req(!is.null(m)); rng <- range(m$Year, na.rm=TRUE)
    paste0("Annual range across all cells: ", sprintf("%.1f - %.1f %s", rng[1], rng[2], UNIT_PM_TEXT))
  })
  
  # ── Downloads ──────────────────────────────────────────────────────────────
  output$dl_scenario <- downloadHandler(
    filename = function() paste0("control_scenario_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      m <- vals(); df <- as.data.frame(m, check.names=FALSE)
      utils::write.csv(cbind(Region=rownames(df), df), file, row.names=FALSE, na="")
    }
  )
  output$dl_results <- downloadHandler(
    filename = function() paste0("prediction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content  = function(file) saveRDS(result_store(), file)
  )
}

# -------------------- Run --------------------
shinyApp(ui, server)