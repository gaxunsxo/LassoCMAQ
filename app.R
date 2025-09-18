suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(shinycssloaders)
  library(shinyjs)
  library(ggplot2)
  library(sf)
  library(cowplot)
  library(future)
  library(promises)
})

plan(multisession)

# -------------------- Constants --------------------
region_names <- c(
  "Seoul","Incheon","Busan","Daegu","Gwangju","Gyeonggi","Gangwon","Chungbuk",
  "Chungnam","Gyeongbuk","Gyeongnam","Jeonbuk","Jeonnam","Jeju","Daejeon","Ulsan","Sejong"
)
factor_names <- c("Power","Industrial","Mobile","Residential","Agriculture","Solvent","Others")

# -------------------- Load Spatial & Model Data --------------------
asia_map <- st_read("/home/geseo/LassoCMAQ_Data/Mapping_shp/Asia_county_map.shp", quiet = TRUE)
mesh     <- st_read("/home/geseo/LassoCMAQ_Data/Mapping_shp/Mesh_test_shift2.shp", quiet = TRUE)
st_crs(asia_map) <- 4326
st_crs(mesh)     <- 4326

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

# -------------------- Theme / CSS --------------------
theme <- bs_theme(
  version = 5, bootswatch = "flatly",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

custom_css <- HTML("
/* ===== Base layout ===== */
html { 
  scroll-behavior: smooth; 
  scroll-padding-top: 20px;
}
body { padding: 0 24px 48px 24px; }
.sticky-top { backdrop-filter: blur(6px); background: rgba(255,255,255,0.85); }
.section { 
  scroll-margin-top: 80px;
  padding-top: 0; 
  margin-top: 0; 
  margin-bottom: 20px;
}
.section-block { margin-top: 18px; }
.hero { padding: 28px 0 8px; margin-bottom: 4px; }
.muted { color:#6c757d; }
.copyright { border-top: 1px solid #e9ecef; padding: 12px 0; margin-top: 24px; }
.card-tight { box-shadow:none; border:1px solid #e9ecef; }

/* ===== Compact cards (right column) ===== */
.card-compact .card-header { padding: 6px 10px; }
.card-compact .card-body   { padding: 8px 10px; }
.card-compact .form-check-label,
.card-compact label { font-size: 0.92rem; }
.card-compact .form-control-sm { height: 28px; padding: 2px 6px; }

/* ===== Table card: only DT scrolls, no cut at bottom ===== */
.custom-table .card-body{
  min-height: 720px;     /* 필요 높이로 조절 (예: 320~420px) */
  overflow-y: auto;      /* 세로 스크롤 ON */
  overflow-x: hidden;    /* 가로 스크롤 숨김 */
  padding: 20px;
}

/* Upload / Download, Select pollutants label 동일하게 bold + 크기 통일 */
.card-upload label,
.card-compact label {
  font-weight: 600;      /* bold */
  font-size: 0.95rem;    /* 카드 헤더랑 조화되게 */
}

/* When DT uses scrollY, ensure bottom is not clipped */
.custom-table .dataTables_scrollBody{
  overflow: visible !important;
  max-height: none !important;
  height: auto !important;
}

/* ===== DataTables layout & compact look ===== */
.custom-table .dataTables_wrapper { width: 100%; }
table.dataTable { table-layout: fixed; width: 100% !important; }
table.dataTable td,
table.dataTable th { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }

/* DataTable 하단 여백 추가 */
.custom-table .dataTables_wrapper table.dataTable tbody {
  padding-bottom: 12px;   /* 마지막 행 밑으로 여유 */
}

/* 혹시 tbody에 padding이 안 먹히면 scrollBody에 여백 */
.custom-table .dataTables_wrapper .dataTables_scrollBody,
.custom-table .dataTables_wrapper {
  padding-bottom: 12px;
}

/* Remove DT chrome (paging/search/info) */
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate,
.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter { display: none !important; }

/* ===== Header styles ===== */
table.dataTable thead th {
  vertical-align: bottom;
  background: #E5F0FB;           /* label row: very light blue */
  border-color: #d0dcec;
  font-weight: 700;
  padding: 3px 5px !important;
  height: 26px;
  line-height: 1.15;
  font-size: 0.88rem;
}

table.dataTable thead tr.header-inputs th {
  background: #CCDFF7;           /* input row: slightly stronger blue */
  font-weight: 600;
  height: 30px;
}

/* Header / row inputs smaller */
.header-input,
.row-input {
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
  background: #CCDFF7;            /* 가로 헤더 input row와 동일 색상 */
  border-right: 2px solid #c9d7ec;
  width: 180px !important;
  max-width: 180px !important;
}
.rowhdr .rname {
  font-weight: 600;
  margin-bottom: 4px;
  display:block;
}
.rowhdr .row-input {
  width: 160px;
}


/* 카드 폭을 내용에 맞게(auto) */
.card-tight.auto-width {
  display: inline-block;
  width: auto !important;
  max-width: 100%;    /* 반응형 안전 */
}

/* Force card title text to be visible */
.card .card-title {
  color: #212529 !important;    /* 본문색 강제 */
  font-weight: 600 !important;  /* 두껍게 */
  font-size: 1rem !important;   /* 크기 */
  margin-bottom: .5rem !important;
}

/* 셀 내부를 flex 컨테이너로 만들어 input을 하단 배치 */
.cell-wrapper {
  display: flex;
  flex-direction: column;
  justify-content: flex-end;  /* 아래쪽으로 붙임 */
  height: 100%;               /* td 전체 높이 차지 */
}

/* input 크기 조금 줄이기 (필요시) */
.cell-wrapper .cell-input {
  margin-top: auto;   /* 위 공간 밀어내고 아래 정렬 */
}
.cell-input {
  height: 20px !important;
  font-size: 0.8rem !important;
  padding: 0 2px !important;
  border: 1px solid #dee2e6;
  border-radius: 3px;
}
/* 셀 입력창 배경 투명하게 */
.cell-input,
.row-input,
.header-input {
  background-color: transparent !important;
}

/* focus(선택) 되었을 때도 투명 유지 */
.cell-input:focus,
.row-input:focus,
.header-input:focus {
  background-color: transparent !important;
  box-shadow: none;          /* 파란 테두리 glow 제거 */
}

/* ===== Shiny notification custom ===== */
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
  tags$head(tags$title("LassoCMAQ"), tags$style(custom_css)),
  
  # Sticky nav
  div(class = "sticky-top py-1",
      layout_column_wrap(width = 1, gap = "4px",
                         card(class="p-1", style="border:1px; box-shadow:none; background:transparent;",
                              layout_column_wrap(width = 1, fill = TRUE,
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
  ),
  
  # Hero
  div(class="hero",
      h2("LassoCMAQ", class = "fw-semibold mb-2"),
  ),
  
  # Home
  div(id = "home", class = "section",
      h3("Home", class = "fw-semibold mb-2"),
      layout_columns(
        col_widths = c(4,4,4),
        
        # What Is This
        card(
          class = "section-block",
          card_body(
            h5("What Is This", class="fw-bold mb-2"),
            tags$ul(
              tags$li("LassoCMAQ is a computationally efficient for CMAQ, developed using the least absolute shrinkage and selection operator (LASSO) together with an adaptive logit transformation of the response variable."),
              tags$li("It estimates Ozone or PM₂.₅ concentrations from regional emission-control scenarios in about 10 seconds each, providing a full surrogate of CMAQ by computing concentrations for every cell at every hour, and enabling rapid what-if exploration without running CMAQ.")
            )
          )
        ),
        
        # How to Use
        card(
          class = "section-block",
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
        
        # Citation
        card(
          class = "section-block",
          card_body(
            h5("Citation", class="fw-bold mb-2"),
            tags$blockquote(
              "D.-B. Lee et al., A fast CMAQ approximation method using LASSO with adaptive logit-transformed response (in preparation)."
            )
          )
        )
      )
  ),
  
  # Control Policy
  div(id="control", class="section",
      h3("Control Policy", class = "fw-semibold mb-2"),
      
      # How to Use
      card(
        class = "section-block card-tight auto-width",
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
                     # LEFT: Table
                     div(
                       card(header="Policy Table (17 × 7)", class="section-block custom-table",
                            DTOutput("policy_dt", width = "100%")
                       )
                     ),
                     # RIGHT: Upload + Run
                     div(
                       card(header="Upload a control policy file (.csv)", class="section-block card-tight card-upload",
                            tags$label("Upload a control policy file (.csv)", class = "form-label fw-semibold"),
                            tags$small(
                              "Example: ",
                              tags$a(href = "sample_policy.csv",
                                     "sample_policy.csv",
                                     download = NA)
                            ),
                            fileInput("policy_upload", NULL, buttonLabel="Upload",
                                      accept = c(".csv", ".rds")),
                       ),
                       card(header="Run Prediction", class="section-block card-tight card-compact",
                            checkboxGroupInput("pollutants","Select pollutant(s)",
                                               choices = c("Ozone" = "o3", "PM₂.₅" = "pm25"),
                                               selected = c("o3","pm25")),
                            actionButton("btn_run","Run", class="btn btn-outline-primary btn-sm w-100"),
                       )
                     )
      )
  ),
  
  # Results
  div(
    id = "outputs", class = "section",
    h3("Results", class = "fw-semibold mb-2"),
    layout_columns(
      col_widths = c(6, 6),
      
      # Ozone
      card(
        class = "section-block",
        h4("Ozone", class = "fw-bold mb-3"),
        plotOutput("o3_plot", height = "680px") %>% withSpinner() %>%
          tagAppendAttributes(id = "o3_plot"),
        layout_columns(
          col_widths = c(6, 6),
          card(class = "card-tight", header = "Grid Average",
               textOutput("o3_mean")),
          card(class = "card-tight", header = "Summary",
               textOutput("o3_summary"))
        )
      ),
      
      # PM₂.₅
      card(
        class = "section-block",
        h4("PM₂.₅", class = "fw-bold mb-3"),
        plotOutput("pm_plot", height = "680px") %>% withSpinner() %>%
          tagAppendAttributes(id = "pm_plot"),
        layout_columns(
          col_widths = c(6, 6),
          card(class = "card-tight", header = "Grid Average",
               textOutput("pm_mean")),
          card(class = "card-tight", header = "Summary",
               textOutput("pm_summary"))
        )
      )
    )
  ),
  
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
                     card(class="card-tight", header="Control Policy",
                          # download current control policy
                          downloadButton("dl_policy",
                                         "Download current control policy (.csv)", 
                                         class="btn btn-outline-primary",
                                         style="font-size:16px")),
                     card(class="card-tight", header="Result File",
                          # download CMAQ approximation results
                          downloadButton("dl_results",
                                         "Download CMAQ approximation results (.rds)", 
                                         class="btn btn-outline-primary",
                                         style="font-size:16px"))
      )
  ),
  
  div(class="copyright", "© Soongsil University Machine Learning Lab All Rights Reserved.")
)

# -------------------- Server --------------------
server <- function(input, output, session) {
  notif_id <- reactiveVal(NULL)
  
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
    colnames(m) <- colnames(df)
    rownames(m) <- rownames(df)
    m
  }
  
  make_region_cell <- function(i) {
    as.character(
      tags$div(class="rowhdr",
               tags$span(class="rname", region_names[i]),
               tags$input(
                 type = "number", step = "0.1", placeholder = "All",
                 class = "form-control form-control-sm row-input",
                 `data-row` = i
               )
      )
    )
  }
  
  make_cell_input <- function(i, j, value) {
    as.character(
      tags$div(
        class = "cell-wrapper",
        tags$input(
          type = "number", step = "0.1",
          value = format(value, trim = TRUE),
          class = "form-control form-control-sm cell-input",
          `data-row` = i, `data-col` = j
        )
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
      cell_cols,
      check.names = FALSE, row.names = NULL
    )
  }
  
  sketch <- tags$table(
    class = "display",
    tags$thead(
      # row 1: labels
      tags$tr(
        tags$th("Region"),
        lapply(factor_names, function(fn) tags$th(fn))
      ),
      # row 2: inputs
      tags$tr(
        class = "header-inputs",
        tags$th(
          tags$input(
            type = "number", step = "0.1", placeholder = "All",
            class = "form-control form-control-sm header-input all-apply"
          )
        ),
        lapply(seq_along(factor_names), function(j) {
          tags$th(
            tags$input(
              type = "number", step = "0.1", placeholder = "All",
              class = "form-control form-control-sm header-input col-apply",
              `data-col` = j
            )
          )
        })
      )
    )
  )
  
  output$policy_dt <- renderDT({
    datatable(
      make_table_data(vals()),
      container = sketch,
      rownames  = FALSE,
      escape    = FALSE,
      selection = "none",
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        autoWidth = FALSE,
        scrollX = FALSE,
        columnDefs = list(
          list(targets = 0, width = "200px", className = "rowhdr"),
          list(targets = 1:7, width = "80px")
        )
      ),
      callback = JS("
function bindRowInputs(api){
  var tbody = $(api.table().body());
  // row apply
  tbody.off('change', 'input.row-input');
  tbody.on('change', 'input.row-input', function(){
    var row = parseInt($(this).attr('data-row'), 10);
    var val = parseFloat(this.value);
    if(!isNaN(val)){
      Shiny.setInputValue('row_apply', {row: row, val: val, nonce: Math.random()});
    }
  });
  // cell apply
  tbody.off('change', 'input.cell-input');
  tbody.on('change', 'input.cell-input', function(){
    var row = parseInt($(this).attr('data-row'), 10);
    var col = parseInt($(this).attr('data-col'), 10);
    var val = parseFloat(this.value);
    if(!isNaN(val)){
      Shiny.setInputValue('cell_edit', {row: row, col: col, val: val, nonce: Math.random()});
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

// Rebind on draw
bindRowInputs(api);
api.on('draw.dt', function(){ bindRowInputs(api); });
")
    )
  })
  
  # 개별 셀 이벤트 처리
  observeEvent(input$cell_edit, {
    info <- input$cell_edit
    i <- as.integer(info$row); j <- as.integer(info$col); v <- as.numeric(info$val)
    if (is.finite(v) && i >= 1 && j >= 1 && i <= nrow(vals()) && j <= ncol(vals())) {
      m <- vals(); m[i, j] <- v; vals(m)
      replaceData(dataTableProxy("policy_dt"), make_table_data(m), resetPaging = FALSE, rownames = FALSE)
    }
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
  
  # upload policy
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
  
  # 로그 파일 경로
  log_file <- "run.log"
  
  log_message <- function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(sprintf("[%s] %s\n", timestamp, msg), file = log_file, append = TRUE)
  }
  
  observeEvent(input$btn_run, {
    start_time <- Sys.time()
    log_message("Run button clicked: start prediction")
    
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
      store$o3 <- predict_with_model(control_vec, models$o3)
      t2 <- Sys.time()
      log_message(sprintf("Ozone prediction finished in %.2f sec", 
                          as.numeric(difftime(t2, t1, units="secs"))))
    }
    
    if (need_pm) {
      t1 <- Sys.time()
      store$pm <- predict_with_model(control_vec, models$pm)
      t2 <- Sys.time()
      log_message(sprintf("PM2.5 prediction finished in %.2f sec", 
                          as.numeric(difftime(t2, t1, units="secs"))))
    }
    
    result_store(store)
    
    end_time <- Sys.time()
    log_message(sprintf("Total prediction time: %.2f sec", 
                        as.numeric(difftime(end_time, start_time, units="secs"))))
    
    # === 알림 표시 ===
    id <- showNotification(
      "Prediction completed. Preparing the map plot, please wait...",
      type = "message", duration = NULL, closeButton = FALSE
    )
    notif_id(id)
  })
  
  # -------------------- Map helpers & plots --------------------
  month_means <- function(arr3) apply(arr3, 1, mean)  # [N,24,D] -> vector by grid
  mesh_with_vals <- function(preds, model) {
    m <- mesh
    m$Year <- month_means(preds)  # Annual only
    m
  }
  get_shared_fill_scale <- function(m, legend_title) {
    vmin <- floor(min(m$Year, na.rm = TRUE) / 10) * 10
    vmax <- ceiling(max(m$Year, na.rm = TRUE) / 10) * 10
    scale_fill_gradient(low = "white", high = "red",
                        limits = c(vmin, vmax),
                        breaks = pretty(c(vmin, vmax), n = 5),
                        name = legend_title)
  }
  plot_map <- function(m, var_name, legend_title, title_text) {
    ggplot() +
      geom_sf(data = asia_map, color = "black", fill = NA) +
      geom_sf(data = m, aes(fill = .data[[var_name]]), alpha = 0.6, color = "gray") +
      coord_sf(xlim = c(124,131), ylim = c(32.5,39.5), expand = FALSE) +
      get_shared_fill_scale(m, legend_title) +
      labs(title = title_text) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12),
        axis.text = element_text(size = 12)
      )
  }
  
  # Ozone Annual
  output$o3_plot <- renderPlot({
    t1 <- Sys.time()
    store <- result_store(); req(store$o3)
    m <- mesh_with_vals(store$o3, models$o3)
    p <- plot_map(m, "Year", "Mean (ppb)", "Ozone Annual Mean")
    t2 <- Sys.time()
    log_message(sprintf("Ozone plot rendered in %.2f sec", as.numeric(difftime(t2, t1, units="secs"))))
    p
  }, res = 60)
  
  output$o3_mean <- renderText({
    store <- result_store(); req(store$o3)
    m <- mesh_with_vals(store$o3, models$o3)
    paste0("Annual average across all cells: ",
           sprintf("%.1f ppb", mean(m$Year, na.rm = TRUE)))
  })
  
  output$o3_summary <- renderText({
    store <- result_store(); req(store$o3)
    m <- mesh_with_vals(store$o3, models$o3)
    rng <- range(m$Year, na.rm = TRUE)
    paste0("Annual range across all cells: ",
           sprintf("%.1f – %.1f ppb", rng[1], rng[2]))
  })
  
  # PM2.5 Annual
  output$pm_plot <- renderPlot({
    t1 <- Sys.time()
    store <- result_store(); req(store$pm)
    m <- mesh_with_vals(store$pm, models$pm)
    p <- plot_map(m, "Year", "Mean (µg/m³)", "PM₂.₅ Annual Mean")
    t2 <- Sys.time()
    log_message(sprintf("PM2.5 plot rendered in %.2f sec", as.numeric(difftime(t2, t1, units="secs"))))
    p
  }, res = 60)
  
  output$pm_mean <- renderText({
    store <- result_store(); req(store$pm)
    m <- mesh_with_vals(store$pm, models$pm)
    paste0("Annual average across all cells: ",
           sprintf("%.1f µg/m³", mean(m$Year, na.rm = TRUE)))
  })
  
  output$pm_summary <- renderText({
    store <- result_store(); req(store$pm)
    m <- mesh_with_vals(store$pm, models$pm)
    rng <- range(m$Year, na.rm = TRUE)
    paste0("Annual range across all cells: ",
           sprintf("%.1f – %.1f µg/m³", rng[1], rng[2]))
  })
  
  observeEvent(input$plot_done, {
    id <- notif_id()
    if (!is.null(id)) {
      removeNotification(id)
      notif_id(NULL)
    }
  })
  
  # -------------------- Downloads --------------------
  # 1) Control policy (.csv)
  output$dl_policy <- downloadHandler(
    filename = function() paste0("control_policy_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      m  <- vals()                                   # 17x7 numeric matrix
      df <- as.data.frame(m, check.names = FALSE)    # 열 이름 유지 (factor_names)
      df_out <- cbind(Region = rownames(df), df)     # Region 컬럼 추가
      utils::write.csv(df_out, file, row.names = FALSE, na = "")
    }
  )
  
  # 2) CMAQ approximation results (.rds)
  output$dl_results <- downloadHandler(
    filename = function() paste0("prediction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content  = function(file) {
      saveRDS(result_store(), file)                  # list(o3=..., pm=...) 구조 그대로 저장
    }
  )
}

# -------------------- Run --------------------
shinyApp(ui, server)