suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(shinycssloaders)
  library(shinyjs)
  library(ggplot2)
  library(sf)
  library(dplyr)
  library(waiter)
  library(shinyWidgets)
})

# -------------------- Constants --------------------
region_names <- c(
  "Seoul","Incheon","Busan","Daegu","Gwangju","Gyeonggi","Gangwon","Chungbuk",
  "Chungnam","Gyeongbuk","Gyeongnam","Jeonbuk","Jeonnam","Jeju","Daejeon","Ulsan","Sejong"
)
factor_names <- c("Power","Industrial","Mobile","Residential","Agriculture","Solvent","Others")

# ------------------ Units ------------------
UNIT_O3_TEXT <- "ppb"
UNIT_PM_TEXT <- "µg/m³"                   
UNIT_PM_HTML <- "&micro;g/m<sup>3</sup>"

# -------------------- Load spatial & model objects --------------------
asia_map <- st_read("/home/geseo/LassoCMAQ_Data/Mapping_shp/Asia_county_map.shp", quiet = TRUE)
mesh     <- st_read("/home/geseo/LassoCMAQ_Data/Mapping_shp/Mesh_test_shift2.shp", quiet = TRUE)
st_crs(asia_map) <- 4326
st_crs(mesh)     <- 4326

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
            by = c("Column","Row"))

# -------------------- Region outline (dissolve) --------------------
region_outline <- mesh %>%
  filter(!is.na(Region_Name), Region_Name != "") %>%
  group_by(Region_Name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_make_valid()

# Ozone
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

# -------------------- Theme & CSS --------------------
theme <- bs_theme(
  version = 5, bootswatch = "flatly",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

custom_css <- HTML("
/* ===== Base layout ===== */
html { scroll-behavior: smooth; scroll-padding-top: 20px; }
body { padding: 0 48px 48px 48px; }
.sticky-top { backdrop-filter: blur(6px); background: rgba(255,255,255,0.85); }
.section { scroll-margin-top: 80px; margin-top: 20px; }
.section-block { margin-top: 18px; }
.hero { padding: 28px 0 8px; margin-bottom: 4px; }
.muted { color:#6c757d; }
.copyright { border-top: 1px solid #e9ecef; padding: 12px 0; margin-top: 24px; }

/* ===== Compact cards (right column) ===== */
.card-compact .card-header { padding: 6px 10px; }
.card-compact .card-body   { padding: 8px 10px; }
.card-compact .form-check-label,
.card-compact label { font-size: 0.92rem; }
.card-compact .form-control-sm { height: 28px; padding: 2px 6px; }

/* ===== Table card: DT scroll & spacing ===== */
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

/* ===== DataTables compact look ===== */
.custom-table .dataTables_wrapper { width: 100%; }
table.dataTable { table-layout: fixed; width: 100% !important; }
table.dataTable td, table.dataTable th { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.custom-table .dataTables_wrapper table.dataTable tbody { padding-bottom: 12px; }
.custom-table .dataTables_wrapper .dataTables_scrollBody,
.custom-table .dataTables_wrapper { padding-bottom: 12px; }

/* Hide default DT chrome */
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate,
.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter { display: none !important; }

/* ===== Table headers ===== */
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

/* Header / row inputs */
.header-input, .row-input {
  height: 22px !important;
  padding: 1px 4px !important;
  line-height: 1.1 !important;
  font-size: 0.86rem;
}

/* ===== Body cells ===== */
table.dataTable tbody td {
  padding: 3px 5px !important;
  height: 24px;
  font-size: 0.90rem;
  background-color: #ffffff;
}

/* ===== First column (Region) as row header ===== */
td.rowhdr {
  background: #CCDFF7;
  border-right: 2px solid #c9d7ec;
  width: 180px !important;
  max-width: 180px !important;
}
.rowhdr .rname { font-weight: 600; margin-bottom: 4px; display:block; }
.rowhdr .row-input { width: 160px; }

/* Auto-width tight cards */
.card-tight.auto-width { display: inline-block; width: auto !important; max-width: 100%; }

/* Card title */
.card .card-title {
  color: #212529 !important;
  font-weight: 600 !important;
  font-size: 1rem !important;
  margin-bottom: .5rem !important;
}

/* Cell input placement */
.cell-wrapper { display: flex; flex-direction: column; justify-content: flex-end; height: 100%; }
.cell-wrapper .cell-input { margin-top: auto; }
.cell-input {
  height: 20px !important;
  font-size: 0.8rem !important;
  padding: 0 2px !important;
  border: 1px solid #dee2e6;
  border-radius: 3px;
}

/* Transparent inputs (normal & focus) */
.cell-input, .row-input, .header-input { background-color: transparent !important; }
.cell-input:focus, .row-input:focus, .header-input:focus {
  background-color: transparent !important;
  box-shadow: none;
}

/* ===== Shiny notification ===== */
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

/* ===== Hover tooltip panel ===== */
.hover-box {
  position: absolute;
  top: 12px;
  right: 12px;
  z-index: 999;
  background: rgba(255,255,255,0.92);
  border: 1px solid #dee2e6;
  border-radius: 10px;
  padding: 10px 12px;
  box-shadow: 0 6px 18px rgba(0,0,0,0.15);
  min-width: 220px;
  max-width: 340px;
  font-size: 0.92rem;
  line-height: 1.35;
}
.hover-box .title { font-weight: 700; margin-bottom: 6px; }
.hover-box .muted { color:#6c757d; font-size: 0.85rem; }
")

# -------------------- UI --------------------
ui <- page_fluid(
  theme = theme,
  useShinyjs(),
  tags$head(tags$title("LassoCMAQ"), tags$style(custom_css)),
  
  # Sticky nav
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
  
  # Hero
  div(class="hero", h2("LassoCMAQ", class = "fw-semibold mb-2")),
  
  # Home
  div(id = "home", class = "section",
      h3("Home", class = "fw-semibold mb-2"),
      layout_columns(col_widths = c(4,4,4),
                     card(class = "section-block",
                          card_body(
                            h5("What Is This", class="fw-bold mb-2"),
                            tags$ul(
                              tags$li("LassoCMAQ is a computationally efficient surrogate for CMAQ, developed using the least absolute shrinkage and selection operator (LASSO) together with an adaptive logit transformation of the response variable."),
                              tags$li("It estimates Ozone or PM₂.₅ concentrations from regional emission-control scenarios in about 10 seconds each, providing a full surrogate of CMAQ by computing concentrations for every cell at every hour, and enabling rapid what-if exploration without running CMAQ.")
                            )
                          )
                     ),
                     card(class = "section-block",
                          card_body(
                            h5("How to Use", class="fw-bold mb-2"),
                            tags$ul(
                              tags$li("1. Enter a 17 × 7 control policy matrix (Region × Emission Source Category) specifying emission change ratios (e.g., 0.9 = 10% reduction from the baseline scenario)."),
                              tags$li("2. Select pollutant(s) and click Run to approximate a CMAQ simulation for the selected control policy."),
                              tags$li("3. Inspect maps and summary metrics; adjust the table and rerun to compare alternative scenarios."),
                              tags$li("4. Download the control policy and the full CMAQ approximation results as needed.")
                            )
                          )
                     ),
                     card(class = "section-block",
                          card_body(
                            h5("Citation", class="fw-bold mb-2"),
                            tags$blockquote(
                              "D.-B. Lee et al., Development of a fast and interpretable machine learning emulator for the Community Multiscale Air Quality Modeling System: application to ozone and PM2.5 policy support (submitted)",
                            )
                          )
                     )
      )
  ),
  
  # Control Policy
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
                                               choices = c("Ozone" = "o3", "PM₂.₅" = "pm25"),
                                               selected = c("o3","pm25")),
                            actionButton("btn_run","Run", class="btn btn-outline-primary btn-sm w-100")
                       )
                     )
      )
  ),
  
  # Results
  div(id = "outputs", class = "section",
      h3("Results", tags$span("(Hover to inspect cells)",
                              class = "text-muted",
                              style = "font-size: 0.85rem; font-weight: normal;")
         ,class = "fw-semibold mb-2"),
      div(style = "width:60%; margin-left:0; margin-top: 15px",
          progressBar(id = "pb", value = 0, total = 100, display_pct = TRUE, striped = TRUE, status = "primary")
      ),
      layout_columns(col_widths = c(6, 6),
                     card(class = "section-block",
                          h4("Ozone", class = "fw-bold mb-3"),
                          div(style = "position: relative;",
                              plotOutput(
                                "o3_plot",
                                height = "680px",
                                hover = hoverOpts("o3_hover", delay = 400, delayType = "debounce")
                              ) %>% withSpinner() %>% tagAppendAttributes(id = "o3_plot"),
                              uiOutput("o3_hover_box")
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         card(header = "Grid Average", textOutput("o3_mean")),
                                         card(header = "Summary", textOutput("o3_summary"))
                          )
                     ),
                     card(class = "section-block",
                          h4("PM₂.₅", class = "fw-bold mb-3"),
                          div(style = "position: relative;",
                              plotOutput(
                                "pm_plot",
                                height = "680px",
                                hover = hoverOpts("pm_hover", delay = 400, delayType = "debounce")
                              ) %>% withSpinner() %>% tagAppendAttributes(id = "pm_plot"),
                              uiOutput("pm_hover_box")
                          ),
                          layout_columns(col_widths = c(6, 6),
                                         card(header = "Grid Average", textOutput("pm_mean")),
                                         card(header = "Summary", textOutput("pm_summary"))
                          )
                     )
      )
  ),
  
  # Plot-done hook
  tags$script(HTML("
    $(document).on('shiny:value', function(event) {
      if (event.target.id === 'o3_plot') {
        Shiny.setInputValue('plot_done', 'o3', {priority: 'event'});
      }
      if (event.target.id === 'pm_plot') {
        Shiny.setInputValue('plot_done', 'pm', {priority: 'event'});
      }
    });
  ")),
  
  # Download
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
  
  # Hover state
  o3_hover_info <- reactiveVal(NULL)
  pm_hover_info <- reactiveVal(NULL)
  
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
        return
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
  
  predict_with_model <- function(control_vec, model) {
    linear_vec  <- as.vector(control_vec %*% model$WEIGHT)
    dims        <- dim(model$BIAS)
    linear_arr  <- array(linear_vec, dim = dims)
    linear_pred <- linear_arr + model$BIAS
    Pred        <- model$ADAPT / (1 + exp(-linear_pred))
    if (!is.null(model$CMAQ_UNIQUE)) Pred[model$CMAQ_UNIQUE] <- model$BIAS[model$CMAQ_UNIQUE]
    Pred * model$SCALE
  }
  
  result_store <- reactiveVal(list(o3=NULL, pm=NULL))
  
  o3_sf <- reactiveVal(NULL)
  pm_sf <- reactiveVal(NULL)
  
  # Logging
  log_file <- "run.log"
  log_message <- function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(sprintf("[%s] %s\n", timestamp, msg), file = log_file, append = TRUE)
  }
  
  # Map helpers
  month_means <- function(arr3) apply(arr3, 1, mean)
  
  # Run prediction
  observeEvent(input$btn_run, {
    
    o3_hover_info(NULL)
    pm_hover_info(NULL)
    
    runjs("document.getElementById('outputs').scrollIntoView({behavior:'smooth', block:'start'});")
    req(input$pollutants)
    
    start_time <- Sys.time()
    log_message("Run button clicked: start prediction")
    
    w$show()
    updateProgressBar(session, "pb", value = 0,  title = "Initializing...")
    updateProgressBar(session, "pb", value = 5,  title = "Validating input...")
    
    m <- vals()
    if (any(!is.finite(m))) { showModal(modalDialog("All cells must be numeric.", easyClose=TRUE)); return() }
    if (any(m < 0.5 | m > 1.5, na.rm = TRUE)) {
      showModal(modalDialog("All values must be between 0.5 and 1.5.", easyClose=TRUE)); return()
    }
    
    control_vec <- as.numeric(t(m))
    need_o3 <- "o3" %in% input$pollutants
    need_pm <- "pm25" %in% input$pollutants
    
    store <- list(o3=NULL, pm=NULL)
    
    if (need_o3) {
      t1 <- Sys.time()
      updateProgressBar(session, "pb", value = 20, title = "Running Ozone prediction...")
      store$o3 <- predict_with_model(control_vec, models$o3)
      
      updateProgressBar(session, "pb", value = if (need_pm) 45 else 80, title = "Ozone prediction finished")
      t2 <- Sys.time()
      log_message(sprintf("Ozone prediction finished in %.2f sec", as.numeric(difftime(t2, t1, units = "secs"))))
      
      m_o3 <- mesh
      m_o3$Year <- month_means(store$o3)
      o3_sf(m_o3)
    } else {
      o3_sf(NULL)
    }
    
    if (need_pm) {
      t1 <- Sys.time()
      updateProgressBar(session, "pb", value = if (need_o3) 50 else 20, title = "Running PM₂.₅ prediction...")
      store$pm <- predict_with_model(control_vec, models$pm)
      updateProgressBar(session, "pb", value = if (need_o3) 75 else 80, title = "PM₂.₅ prediction finished")
      t2 <- Sys.time()
      log_message(sprintf("PM₂.₅ prediction finished in %.2f sec", as.numeric(difftime(t2, t1, units = "secs"))))
      
      m_pm <- mesh
      m_pm$Year <- month_means(store$pm)
      pm_sf(m_pm)
    } else {
      pm_sf(NULL)
    }
    
    result_store(store)
    updateProgressBar(session, "pb", value = 90, title = "Preparing plots...")
    
    end_time <- Sys.time()
    log_message(sprintf("Total prediction time: %.2f sec", as.numeric(difftime(end_time, start_time, units = "secs"))))
  })
  
  # Plot-done -> complete progress & hide overlay
  observeEvent(input$plot_done, {
    updateProgressBar(session, "pb", value = 100, title = "Completed!")
    Sys.sleep(0.5)
    w$hide()
  })
  
  # -------------------- Plot helpers & plots --------------------
  get_shared_fill_scale <- function(m, legend_title) {
    vmin <- floor(min(m$Year, na.rm = TRUE) / 10) * 10
    vmax <- ceiling(max(m$Year, na.rm = TRUE) / 10) * 10
    scale_fill_gradient(
      low = "white", high = "red",
      limits = c(vmin, vmax),
      breaks = pretty(c(vmin, vmax), n = 5),
      name = legend_title
    )
  }
  plot_map <- function(m, var_name, legend_title, title_text) {
    ggplot() +
      geom_sf(data = asia_map, color = "black", fill = NA) +
      geom_sf(data = m, aes(fill = .data[[var_name]]), alpha = 0.6, color = "gray") +
      coord_sf(xlim = c(124, 131), ylim = c(32.5, 39.5), expand = FALSE) +
      get_shared_fill_scale(m, legend_title) +
      labs(title = title_text) +
      theme_minimal() +
      theme(
        plot.title   = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12),
        axis.text    = element_text(size = 12)
      )
  }
  
  output$o3_plot <- renderPlot({
    m <- o3_sf(); req(m)
    plot_map(
      m,
      "Year",
      paste0("Ozone (", UNIT_O3_TEXT, ")"),
      "Ozone Annual Mean"
    )
  }, res = 60)
  
  output$pm_plot <- renderPlot({
    m <- pm_sf(); req(m)
    plot_map(
      m,
      "Year",
      paste0("PM₂.₅ (", UNIT_PM_TEXT, ")"),
      "PM₂.₅ Annual Mean"
    )
  }, res = 60)
  
  output$o3_mean <- renderText({
    m <- o3_sf(); req(m)
    paste0("Annual average across all cells: ",
           sprintf("%.1f %s", mean(m$Year, na.rm = TRUE), UNIT_O3_TEXT))
  })
  
  output$o3_summary <- renderText({
    m <- o3_sf(); req(m)
    rng <- range(m$Year, na.rm = TRUE)
    paste0("Annual range across all cells: ",
           sprintf("%.1f – %.1f %s", rng[1], rng[2], UNIT_O3_TEXT))
  })
  
  output$pm_mean <- renderText({
    m <- pm_sf(); req(m)
    paste0("Annual average across all cells: ",
           sprintf("%.1f %s", mean(m$Year, na.rm = TRUE), UNIT_PM_TEXT))
  })
  
  output$pm_summary <- renderText({
    m <- pm_sf(); req(m)
    rng <- range(m$Year, na.rm = TRUE)
    paste0("Annual range across all cells: ",
           sprintf("%.1f – %.1f %s", rng[1], rng[2], UNIT_PM_TEXT))
  })
  
  # -------------------- Hover --------------------
  observeEvent(input$o3_hover, {
    m <- o3_sf()
    if (is.null(m)) return()
    
    h <- input$o3_hover
    if (is.null(h$x) || is.null(h$y)) return()
    
    lon <- h$x; lat <- h$y
    lon_r <- round(lon, 2); lat_r <- round(lat, 2)
    
    pt <- st_sfc(st_point(c(lon, lat)), crs = st_crs(m))
    idx <- which(st_intersects(m, pt, sparse = FALSE))
    if (length(idx) == 0) { return() }
    idx <- idx[1]
    
    prev <- o3_hover_info()
    if (!is.null(prev) && !is.null(prev$idx) && identical(prev$idx, idx)) return()
    
    region_name <- m$Region_Name[idx]
    if (is.na(region_name) || region_name == "") region_name <- "NA"
    value <- m$Year[idx]
    
    o3_hover_info(list(idx = idx, region = region_name, lon = lon_r, lat = lat_r, value = value))
  }, ignoreInit = TRUE)
  
  observeEvent(input$pm_hover, {
    m <- pm_sf()
    if (is.null(m)) return()
    
    h <- input$pm_hover
    if (is.null(h$x) || is.null(h$y)) return()
    
    lon <- h$x; lat <- h$y
    lon_r <- round(lon, 2); lat_r <- round(lat, 2)
    
    pt <- st_sfc(st_point(c(lon, lat)), crs = st_crs(m))
    idx <- which(st_intersects(m, pt, sparse = FALSE))
    if (length(idx) == 0) { return() }
    idx <- idx[1]
    
    prev <- pm_hover_info()
    if (!is.null(prev) && !is.null(prev$idx) && identical(prev$idx, idx)) return()
    
    region_name <- m$Region_Name[idx]
    if (is.na(region_name) || region_name == "") region_name <- "NA"
    value <- m$Year[idx]
    
    pm_hover_info(list(idx = idx, region = region_name, lon = lon_r, lat = lat_r, value = value))
  }, ignoreInit = TRUE)
  
  output$o3_hover_box <- renderUI({
    info <- o3_hover_info()
    m    <- o3_sf()
    if (is.null(info) || is.null(m)) return(NULL)
    
    tags$div(
      class = "hover-box",
      tags$div(class = "title", "Grid Cell Info (Ozone)"),
      HTML(sprintf(
        "<b>Region:</b> %s<br>
         <b>Longitude:</b> %.2f<br>
         <b>Latitude:</b> %.2f<br>
         <b>Annual Mean:</b> %.2f %s<br>",
        info$region, info$lon, info$lat, info$value, UNIT_O3_TEXT
      ))
    )
  })
  
  output$pm_hover_box <- renderUI({
    info <- pm_hover_info()
    m    <- pm_sf()
    if (is.null(info) || is.null(m)) return(NULL)
    
    tags$div(
      class = "hover-box",
      tags$div(class = "title", "Grid Cell Info (PM₂.₅)"),
      HTML(sprintf(
        "<b>Region:</b> %s<br>
         <b>Longitude:</b> %.2f<br>
         <b>Latitude:</b> %.2f<br>
         <b>Annual Mean:</b> %.2f %s<br>",
        info$region, info$lon, info$lat, info$value, UNIT_PM_HTML
      ))
    )
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