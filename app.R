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
})

# -------------------- Constants --------------------
region_names <- c(
  "Seoul","Incheon","Busan","Daegu","Gwangju","Gyeonggi","Gangwon","Chungbuk",
  "Chungnam","Gyeongbuk","Gyeongnam","Jeonbuk","Jeonnam","Jeju","Daejeon","Ulsan","Sejong"
)
factor_names <- c("Power","Industrial","Mobile","Residential","Agriculture","Solvent","Others")

# -------------------- Global execution lock --------------------
LOCK_PATH <- "/tmp/lassocmaq_prediction.lock"

acquire_global_lock <- function(timeout = 0) {
  dir.create(dirname(LOCK_PATH), recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(LOCK_PATH)) {
    ok <- file.create(LOCK_PATH)
    if (!ok) stop("Failed to create lock file.")
  }
  
  filelock::lock(LOCK_PATH, timeout = timeout)
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

# PM2.5
PM25_LABEL_TEXT <- "PM<sub>2.5</sub>"
PM25_LABEL_HTML <- "PM<sub>2.5</sub>"

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
  "Industrial"  = "#FFC300",
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

# PM2.5
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

        var body = document.querySelector('#policy_dt .dataTables_scrollBody');
        if (body) {
          window.__policyScroll.tableY = body.scrollTop || 0;
        } else {
          window.__policyScroll.tableY = 0;
        }
      });

      Shiny.addCustomMessageHandler('restorePolicyScroll', function(msg) {
        setTimeout(function() {
          window.scrollTo(0, window.__policyScroll.pageY || 0);

          var body = document.querySelector('#policy_dt .dataTables_scrollBody');
          if (body) {
            body.scrollTop = window.__policyScroll.tableY || 0;
          }
        }, 0);
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
                            tags$ul(
                              tags$li("1. Enter a 17 × 7 emission scenario matrix (Region × Emission Sector) specifying emission change ratios (e.g., 0.9 = 10% reduction from the baseline)."),
                              tags$li("2. Select pollutant(s) and click Run to estimate CMAQ-equivalent concentrations for the selected scenario."),
                              tags$li("3. Inspect maps and summary metrics; click a grid cell to view the top five influential variables for the corresponding region."),
                              tags$li("4. Download the scenario inputs and the full model results as needed.")
                            )
                          )
                     ),
                     card(class = "section-block",
                          card_body(
                            h5("Citation", class="fw-bold mb-2"),
                            tags$blockquote(
                              "D.-B. Lee et al., A LASSO-based reduced-form CMAQ model for predicting ozone and PM2.5 responses to emission changes in South Korea (submitted)"
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
                          h4(HTML("PM<sub>2.5</sub>"), class = "fw-bold mb-3"),
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
  observe({
    shinyjs::toggleState("btn_run", condition = !is_running())
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
  
  preserve_policy_scroll <- function(expr) {
    session$sendCustomMessage("savePolicyScroll", list())
    on.exit(session$sendCustomMessage("restorePolicyScroll", list()), add = TRUE)
    force(expr)
  }
  
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
         
         Shiny.setInputValue('js_save_scroll', {nonce: Math.random()});
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
  
  observeEvent(input$js_save_scroll, {
    session$sendCustomMessage("savePolicyScroll", list())
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
      preserve_policy_scroll({
        replaceData(dataTableProxy("policy_dt"), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
      })
    }
  })
  
  observeEvent(input$row_apply, {
    info <- input$row_apply
    i <- as.integer(info$row)
    v <- as.numeric(info$val)
    if (is.finite(v) && i >= 1 && i <= nrow(vals())) {
      m <- vals(); m[i, ] <- v; vals(m)
      preserve_policy_scroll({
        replaceData(dataTableProxy("policy_dt"), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
      })
    }
  })
  
  observeEvent(input$all_apply, {
    v <- as.numeric(input$all_apply$val)
    if (is.finite(v)) {
      m <- vals(); m[,] <- v; vals(m)
      preserve_policy_scroll({
        replaceData(dataTableProxy("policy_dt"), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
      })
    }
  })
  
  # Upload policy
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
      
      vals(m)
      preserve_policy_scroll({
        replaceData(dataTableProxy("policy_dt"), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
      })
      
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
  
  # -------------------- Weight popup helpers --------------------
  get_weight_top5 <- function(region, pollutant) {
    
    df <- if (pollutant == "o3") O3_weight_summary else PM_weight_summary
    
    out <- df %>%
      dplyr::filter(Target_Region == region) %>%
      dplyr::arrange(desc(Weight_Ratio)) %>%
      dplyr::slice_head(n = 5) %>%
      dplyr::mutate(
        SectorFull = unname(sector_map[Input_Sector]),
        SectorFull = ifelse(is.na(SectorFull), "Others", SectorFull),
        Label = paste(Input_Region, SectorFull, sep = "\n"),
        TextColor2 = ifelse(tolower(TextColor) == "red", "#FF0000", "#2E8B57")
      )
    
    out
  }
  
  make_weight_plot <- function(region, pollutant) {
    
    df <- get_weight_top5(region, pollutant)
    if (nrow(df) == 0) return(NULL)
    
    top5_sum <- sum(df$Weight_Ratio, na.rm = TRUE)
    xmax <- max(df$Weight_Ratio, na.rm = TRUE) * 1.40
    
    ggplot(
      df,
      aes(
        x = Weight_Ratio,
        y = reorder(Label, Weight_Ratio),
        fill = SectorFull
      )
    ) +
      geom_col(width = 0.7, color = "black") +
      geom_text(
        aes(
          label = sprintf("%.1f%%", Weight_Ratio),
          color = TextColor2
        ),
        hjust = -0.08,
        size = 5,
        show.legend = FALSE,
        fontface = "bold"
      ) +
      scale_fill_manual(values = sector_colors) +
      scale_color_identity() +
      scale_x_continuous(
        limits = c(0, xmax),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        title = paste0(region),
        x = "Ratio (%)",
        y = "Region-Sector"
      ) +
      annotate(
        "label",
        x = xmax * 0.95,
        y = 0.56,
        label = sprintf("%.1f%%", top5_sum),
        size = 5,
        fontface = "bold"
      ) +
      theme_bw(base_size = 16) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        panel.grid.major.y = element_blank()
      )
  }
  
  plot_to_popup <- function(plot_obj) {
    if (is.null(plot_obj)) return(htmltools::HTML("<div>No data</div>"))
    
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 900, height = 520, res = 110)
    print(plot_obj)
    dev.off()
    
    img <- base64enc::dataURI(file = tmp, mime = "image/png")
    htmltools::HTML(
      paste0("<img src='", img, "' width='500px'>")
    )
  }
  
  # -------------------- Run prediction --------------------
  init_leaflet <- function() {
    leaflet(options = leafletOptions(preferCanvas = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 127.8, lat = 36.2, zoom = 6) %>%
      addPolygons(
        data = asia_map,
        fill = FALSE,
        color = "#444444",
        weight = 1,
        opacity = 0.9,
        group = "boundary",
        options = pathOptions(interactive = FALSE)
      )
  }
  
  reset_leaflet <- function(map_id) {
    leafletProxy(map_id) %>%
      clearGroup("mesh") %>%
      clearGroup("mesh_boundary") %>%
      clearControls() %>%
      clearPopups() %>%
      setView(lng = 127.8, lat = 36.2, zoom = 6)
  }
  
  make_red_pal <- function(x) {
    vmin <- floor(min(x, na.rm = TRUE) / 10) * 10
    vmax <- ceiling(max(x, na.rm = TRUE) / 10) * 10
    
    pal <- leaflet::colorNumeric(
      palette = RColorBrewer::brewer.pal(9, "Reds"),
      domain = c(vmin, vmax),
      na.color = "transparent"
    )
    
    list(pal = pal, vmin = vmin, vmax = vmax)
  }
  
  update_leaflet_map <- function(map_id, m, legend_title_html) {
    
    m <- st_make_valid(m)
    m <- m[!sf::st_is_empty(m), ]
    
    if (nrow(m) == 0 || all(is.na(m$Year))) {
      reset_leaflet(map_id)
      return(invisible(NULL))
    }
    
    bb <- st_bbox(m)
    
    pal_info <- make_red_pal(m$Year)
    pal <- pal_info$pal
    vmin <- pal_info$vmin
    vmax <- pal_info$vmax
    
    pal_rev <- leaflet::colorNumeric(
      palette = rev(RColorBrewer::brewer.pal(9, "Reds")),
      domain = c(vmin, vmax)
    )
    
    leafletProxy(map_id, data = m) %>%
      clearGroup("mesh") %>%
      clearGroup("mesh_boundary") %>%
      clearControls() %>%
      clearPopups() %>%
      fitBounds(
        lng1 = bb["xmin"],
        lat1 = bb["ymin"],
        lng2 = bb["xmax"],
        lat2 = bb["ymax"]
      ) %>%
      addPolygons(
        fillColor = ~pal(Year),
        fillOpacity = 0.7,
        color = "#00000020",
        weight = 0.2,
        group = "mesh",
        layerId = ~paste0(Row, "_", Column),
        label = ~sprintf(
          "Region: %s, Value: %.1f",
          Region_Name,
          Year
        ),
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "13px",
          noHide = FALSE,
          style = list(
            "font-weight" = "normal",
            "padding" = "4px 8px"
          )
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000",
          bringToFront = TRUE
        )
      ) %>%
      addPolygons(
        fill = FALSE,
        color = "#777777",
        weight = 0.12,
        opacity = 0.6,
        group = "mesh_boundary",
        options = pathOptions(interactive = FALSE)
      ) %>%
      addLegend(
        pal = pal_rev,
        values = c(vmin, vmax),
        title = htmltools::HTML(legend_title_html),
        position = "bottomright",
        opacity = 1,
        labFormat = labelFormat(
          transform = function(x) sort(x, decreasing = TRUE)
        )
      )
  }
  
  output$o3_plot <- renderLeaflet(init_leaflet())
  output$pm_plot <- renderLeaflet(init_leaflet())
  
  observeEvent(input$btn_run, {
    runjs("document.getElementById('outputs').scrollIntoView({behavior:'smooth', block:'start'});")
    req(input$pollutants)
    
    lock_obj <- NULL
    
    tryCatch({
      # -------------------- Global lock acquire --------------------
      lock_obj <- acquire_global_lock(timeout = 0)
      
      if (is.null(lock_obj)) {
        showModal(modalDialog(
          title = "Prediction Busy",
          "Another user is currently running a prediction. Please try again after the current run finishes.",
          easyClose = TRUE
        ))
        log_message("Run rejected: another user already holds the global lock")
        return()
      }
      
      on.exit({
        release_global_lock(lock_obj)
        log_message("Global lock released")
      }, add = TRUE)
      
      start_time <- Sys.time()
      log_message("Run clicked: start prediction (global lock acquired)")
      
      w$show()
      on.exit(w$hide(), add = TRUE)
      
      updateProgressBar(session, "pb", value = 0,  title = "Initializing...")
      updateProgressBar(session, "pb", value = 5,  title = "Validating input...")
      
      m <- vals()
      if (any(!is.finite(m))) {
        showModal(modalDialog("All cells must be numeric.", easyClose = TRUE))
        return()
      }
      if (any(m < 0.5 | m > 1.5, na.rm = TRUE)) {
        showModal(modalDialog("All values must be between 0.5 and 1.5.", easyClose = TRUE))
        return()
      }
      
      control_vec <- as.numeric(t(m))
      need_o3 <- "o3" %in% input$pollutants
      need_pm <- "pm25" %in% input$pollutants
      
      store <- list(o3 = NULL, pm = NULL)
      
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
      
      end_time <- Sys.time()
      log_message("Total run time: %.3f sec", as.numeric(difftime(end_time, start_time, units = "secs")))
      
    }, error = function(e) {
      log_message("Run failed: %s", e$message)
      showModal(modalDialog(
        title = "Prediction Error",
        paste("An error occurred during prediction:", e$message),
        easyClose = TRUE
      ))
    })
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
  
  observeEvent(input$o3_plot_shape_click, ignoreInit = TRUE, {
    id <- input$o3_plot_shape_click$id
    req(id)
    
    rc <- strsplit(id, "_")[[1]]
    r <- as.numeric(rc[1])
    c <- as.numeric(rc[2])
    
    region <- mesh %>%
      dplyr::filter(Row == r, Column == c) %>%
      dplyr::pull(Region_Name) %>%
      unique()
    
    req(length(region) > 0)
    
    p <- make_weight_plot(region[1], "o3")
    popup <- plot_to_popup(p)
    
    leafletProxy("o3_plot") %>%
      clearPopups() %>%
      addPopups(
        lng = input$o3_plot_shape_click$lng,
        lat = input$o3_plot_shape_click$lat,
        popup = popup,
        options = popupOptions(maxWidth = 560)
      )
  })
  
  observeEvent(input$pm_plot_shape_click, ignoreInit = TRUE, {
    id <- input$pm_plot_shape_click$id
    req(id)
    
    rc <- strsplit(id, "_")[[1]]
    r <- as.numeric(rc[1])
    c <- as.numeric(rc[2])
    
    region <- mesh %>%
      dplyr::filter(Row == r, Column == c) %>%
      dplyr::pull(Region_Name) %>%
      unique()
    
    req(length(region) > 0)
    
    p <- make_weight_plot(region[1], "pm")
    popup <- plot_to_popup(p)
    
    leafletProxy("pm_plot") %>%
      clearPopups() %>%
      addPopups(
        lng = input$pm_plot_shape_click$lng,
        lat = input$pm_plot_shape_click$lat,
        popup = popup,
        options = popupOptions(maxWidth = 560)
      )
  })
  
  last_hover_o3 <- reactiveVal(NULL)
  
  observeEvent(input$o3_plot_shape_mouseover, {
    id <- input$o3_plot_shape_mouseover$id
    if (!identical(id, last_hover_o3())) {
      last_hover_o3(id)
    }
  })
  
  last_hover_pm <- reactiveVal(NULL)
  
  observeEvent(input$pm_plot_shape_mouseover, {
    id <- input$pm_plot_shape_mouseover$id
    if (!identical(id, last_hover_pm())) {
      last_hover_pm(id)
    }
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
  output$dl_scenario <- downloadHandler(
    filename = function() paste0("control_scenario_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
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