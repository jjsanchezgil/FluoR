library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

source("R/standards_editor.R")
source("R/samples_editor.R")
source("R/pipetting_order.R")
source("R/quantification.R")

# ============================================================
# FluoR - Fluorescence Quantification & Plate Analysis
# Version 0.1
# ============================================================

# ------------------------------------------------------------
# IMPORT FUNCTIONS
# ------------------------------------------------------------

read_raw_sheet <- function(path, sheet) {
  readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text"
  )
}

detect_columns <- function(raw) {
  
  for (r in seq_len(min(nrow(raw), 200))) {
    
    values <- trimws(tolower(
      as.character(unlist(raw[r, ], use.names = FALSE))
    ))
    
    well <- which(values == "well")
    rfu <- grep("measurement|rfu|fluorescence", values)
    
    if (length(well) > 0 && length(rfu) > 0) {
      return(list(
        header = r,
        well = well[1],
        rfu = rfu[1]
      ))
    }
  }
  
  NULL
}

parse_plate <- function(raw, header, well_col, rfu_col) {
  
  if (header >= nrow(raw)) {
    stop("No data rows found after the header.")
  }
  
  if (max(well_col, rfu_col) > ncol(raw)) {
    stop("Selected column is outside the data range.")
  }
  
  dat <- raw[seq.int(header + 1, nrow(raw)), , drop = FALSE]
  
  well <- toupper(trimws(as.character(dat[[well_col]])))
  
  # Accept A1 and A01, for example
  valid <- !is.na(well) &
    grepl("^[A-H](0?[1-9]|1[0-2])$", well)
  
  well <- well[valid]
  
  rfu_text <- as.character(dat[[rfu_col]][valid])
  rfu <- suppressWarnings(
    as.numeric(gsub(",", ".", rfu_text, fixed = TRUE))
  )
  
  row <- sub("[0-9]+$", "", well)
  column <- as.integer(sub("^[A-H]", "", well))
  
  data.frame(
    Well = paste0(row, column),
    Row = row,
    Column = column,
    RFU = rfu
  )
}

# ------------------------------------------------------------
# USER INTERFACE
# ------------------------------------------------------------

source_controls <- function(id, label, choices = NULL, selected = NULL,
                            mode = "auto", header = 29, wellcol = 2, rfucol = 3) {
  
  tagList(
    if (!is.null(label)) h5(label),
    
    selectInput(
      paste0(id, "_source"),
      "File / worksheet",
      choices = choices,
      selected = selected
    ),
    
    selectInput(
      paste0(id, "_mode"),
      "Import mode",
      choices = c(
        "Automatic detection" = "auto",
        "Manual configuration" = "manual"
      ),
      selected = mode
    ),
    
    conditionalPanel(
      condition = sprintf(
        "input.%s_mode == 'manual'", id
      ),
      
      numericInput(
        paste0(id, "_header"),
        "Header row",
        value = header,
        min = 1
      ),
      
      numericInput(
        paste0(id, "_wellcol"),
        "Well column number",
        value = wellcol,
        min = 1
      ),
      
      numericInput(
        paste0(id, "_rfucol"),
        "RFU column number",
        value = rfucol,
        min = 1
      )
    )
  )
}

ui <- page_sidebar(
  
  
  title = tags$h1(
    class = "bslib-page-title",
    
    style = paste(
      "display:flex;",
      "align-items:baseline;",
      "gap:12px;",
      "flex-wrap:wrap;"
    ),
    
    tags$strong(
      "FluoR",
      style = "font-size:22px;font-weight:700;"
    ),
    
    tags$span(
      "v0.1",
      style = "font-size:12px;opacity:0.8;"
    ),
    
    tags$span(
      "Fluorescence-based DNA quantification and plate analysis",
      style = "font-size:13px;font-weight:400;opacity:0.85;"
    )
  ),
  
  fillable = FALSE,
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  tags$head(
    tags$style(HTML("
    #std_table,
    .sample-measurements {
      max-width: 100%;
    }

    #std_table .dataTables_filter,
    .sample-measurements .dataTables_filter {
      float: left !important;
      text-align: left !important;
      margin-bottom: 10px;
    }

    #std_table .dataTables_filter input,
    .sample-measurements .dataTables_filter input {
      width: 160px;
      margin-left: 8px;
    }

    #std_table table,
    .sample-measurements table {
      width: 100% !important;
    }
    .sample-source-card { border:1px solid #dce3eb; border-radius:8px;
      padding:10px; margin-bottom:10px; }
    .sample-source-card .form-group { margin-bottom:8px; }
    .sample-section { padding:10px 0 18px; border-bottom:1px solid #dce3eb; }
    .sample-section h4 { margin:0 0 3px; }
    .sample-section .sample-file-name { color:#64748b; margin-bottom:12px;
      overflow-wrap:anywhere; font-size:14px; }
      /* Move sample measurements panel upwards */
      /* Compact fluorescence correction control */
.decay-setting .shiny-input-container {
  margin-bottom: 0 !important;
}
.decay-setting .checkbox {
  margin-bottom: 0 !important;
}
  "))
  ),
  
  sidebar = sidebar(
    
    selectInput(
      "assay",
      "Qubit assay",
      choices = c(
        "dsDNA High Sensitivity" = "HS",
        "dsDNA Broad Range" = "BR"
      )
    ),
    
    hr(),
    
    uiOutput("sample_source_cards"),
    
    actionButton(
      "add_sample_plate",
      "Add sample plates",
      icon = icon("plus"),
      class = "btn-primary"
    ),
    
    width = 250
  ),
  
  navset_card_tab(
    
    # ==========================================================
    # IMPORT
    # ==========================================================
    
    nav_panel(
      "Import",
      
      h4("Import measurements"),
      
      tags$p(
        "Upload fluorescence measurements from the BioTek Synergy H1 and configure the standards data source.",
        style = "color:#64748b;margin-bottom:20px;"
      ),
      
      fluidRow(
        
        column(
          width = 6,
          
          h5("Upload files"),
          
          fileInput(
            "files",
            "Upload Synergy H1 files",
            multiple = TRUE,
            accept = c(".xlsx", ".xls")
          )
        ),
        
        column(
          width = 6,
          
          source_controls(
            "std",
            "Standards"
          )
        )
      )
    ),
    
    # ==========================================================
    # STANDARDS
    # ==========================================================
    
    nav_panel(
      "Standards",
      
      tags$div(
        class = "decay-setting",
        
        checkboxInput(
          "decay_correction",
          "Correct fluorescence decay",
          value = TRUE
        ),
        
        tags$p(
          "Disable to quantify using initial ST1 and ST2 only, without decay correction or final standards.",
          style = "font-size:13px;color:#64748b;margin:2px 0 12px 0;"
        )
      ),
      
      standards_editor_ui("std_layout")
    ),
    
    # ==========================================================
    # SAMPLES
    # ==========================================================
    
    nav_panel(
      "Samples",
      uiOutput("sample_sections")
    ),
    
    nav_panel(
      "Pipetting order",
      pipetting_order_ui("pipetting")
    ),
    
    nav_panel(
      "Data processing",
      
      h4("Quantification settings"),
      
      fluidRow(
        column(
          width = 3,
          numericInput(
            "standard_volume",
            "Standard volume (µL)",
            value = 10,
            min = 0.5,
            step = 0.5
          )
        ),
        column(
          width = 3,
          numericInput(
            "sample_volume",
            "Sample volume (µL)",
            value = 2,
            min = 0.5,
            step = 0.5
          )
        ),
        column(
          width = 3,
          numericInput(
            "well_volume",
            "Final assay volume (µL)",
            value = 200,
            min = 1,
            step = 10
          )
        ),
        
        column(
          width = 3,
          uiOutput("quant_status")
        )
        ),
      
      tags$hr(),
      
      conditionalPanel(
        condition = "input.decay_correction === true",
        
        tags$hr(),
        
        h4("Multiplicative fluorescence decay"),
        
        uiOutput("decay_summary"),
        
        fluidRow(
          column(
            width = 6,
            h5("Before correction"),
            plotlyOutput("decay_before", height = "520px")
          ),
          column(
            width = 6,
            h5("After correction"),
            plotlyOutput("decay_after", height = "520px")
          )
        ),
        
        tags$hr()
      ),
      
      h4("DNA calibration in assay wells"),
      
      tags$p(
        "Corrected RFU vs DNA concentration and DNA amount",
        "in the configured final assay volume.",
        style = "color:#64748b;"
      ),
      
      plotlyOutput(
        "calibration_plot",
        height = "480px"
      )
    ),
    
    nav_panel(
      "Results",
      
      h4("DNA quantification results"),
      
      downloadButton(
        "download_results",
        "Export results",
        icon = icon("download"),
        class = "btn-primary"
      ),
      
      tags$div(
        style = "margin-top:12px;",
        DTOutput("results_table")
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Catalog all sheets across all uploaded files
  sources <- reactive({
    
    req(input$files)
    
    result <- list()
    
    for (i in seq_len(nrow(input$files))) {
      
      sheets <- excel_sheets(input$files$datapath[i])
      
      for (sheet in sheets) {
        
        result[[length(result) + 1]] <- list(
          file = input$files$datapath[i],
          name = input$files$name[i],
          sheet = sheet
        )
      }
    }
    
    result
  })
  
  # Stable identifiers: removing plate 2 does not rename plate 3.
  sample_plate_ids <- reactiveVal("sample_1")
  sample_next_id <- reactiveVal(1L)
  # Store each sample plate layout reactiveVal.
  sample_layout_registry <- new.env(parent = emptyenv())
  
  source_choices <- reactive({
    
    if (is.null(input$files)) {
      return(character(0))
    }
    
    src <- sources()
    
    labels <- vapply(
      src,
      function(x) paste(x$name, x$sheet, sep = " / "),
      character(1)
    )
    
    # Unique sequential identifier for every imported worksheet
    labels <- paste0(
      labels,
      " #",
      seq_along(labels)
    )
    
    setNames(
      as.character(seq_along(src)),
      labels
    )
  })
  
  
  # Detect worksheets containing plate measurements
  compatible_plate_indices <- reactive({
    
    if (is.null(input$files)) {
      return(integer(0))
    }
    
    src <- sources()
    
    which(vapply(src, function(s) {
      tryCatch({
        
        raw <- read_raw_sheet(s$file, s$sheet)
        detected <- detect_columns(raw)
        
        if (is.null(detected)) {
          return(FALSE)
        }
        
        plate <- parse_plate(
          raw,
          detected$header,
          detected$well,
          detected$rfu
        )
        
        nrow(plate) > 0L && any(is.finite(plate$RFU))
        
      }, error = function(e) FALSE)
    }, logical(1)))
  })
  
  # All compatible plates except the selected standards
  sample_sheet_indices <- reactive({
    
    compatible <- compatible_plate_indices()
    
    if (!length(compatible)) {
      return(integer(0))
    }
    
    std_id <- suppressWarnings(
      as.integer(input$std_source)
    )
    
    if (
      length(std_id) != 1L ||
      is.na(std_id) ||
      !(std_id %in% compatible)
    ) {
      std_id <- compatible[1]
    }
    
    compatible[compatible != std_id]
  })
  
  
  observeEvent(source_choices(), {
    
    choices <- source_choices()
    compatible <- compatible_plate_indices()
    
    selected <- if (length(compatible)) {
      as.character(compatible[1])
    } else if (length(choices)) {
      unname(choices[1])
    } else {
      character(0)
    }
    
    updateSelectInput(
      session,
      "std_source",
      choices = choices,
      selected = selected
    )
  })
  
  # Import cards are recreated only when plates or uploaded sources change.
  # Existing selections and manual-import settings are retained.
  output$sample_source_cards <- renderUI({
    ids <- sample_plate_ids()
    choices <- source_choices()
    tagList(lapply(seq_along(ids), function(k) {
      id <- ids[k]
      
      selection <- isolate(input[[paste0(id, "_source")]])
      
      if (is.null(selection) || !selection %in% unname(choices)) {
        
        sample_indices <- sample_sheet_indices()
        
        if (k <= length(sample_indices)) {
          selection <- as.character(sample_indices[k])
        } else if (length(choices)) {
          selection <- unname(choices[min(k + 1L, length(choices))])
        } else {
          selection <- character(0)
        }
      }
      val <- function(suffix, default) {
        x <- isolate(input[[paste0(id, suffix)]])
        if (is.null(x)) default else x
      }
      tags$div(class = "sample-source-card",
               tags$div(style = "display:flex;align-items:center;justify-content:space-between;",
                        tags$strong(paste("Sample plate", k)),
                        if (length(ids) > 1) actionButton(paste0("remove_", id),
                                                          "Remove", class = "btn-sm btn-outline-danger")
               ),
               source_controls(id, NULL, choices = choices, selected = selection,
                               mode = val("_mode", "auto"), header = val("_header", 29),
                               wellcol = val("_wellcol", 2), rfucol = val("_rfucol", 3))
      )
    }))
  })
  
  observeEvent(input$add_sample_plate, {
    n <- sample_next_id() + 1L
    sample_next_id(n)
    id <- paste0("sample_", n)
    register_sample_plate(id)
    sample_plate_ids(c(sample_plate_ids(), id))
  })
  
  # Shared importer for standards and samples
  get_plate <- function(prefix) {
    
    source_id <- input[[paste0(prefix, "_source")]]
    req(source_id)
    
    src <- sources()[[as.integer(source_id)]]
    req(src)
    
    raw <- read_raw_sheet(src$file, src$sheet)
    
    mode <- input[[paste0(prefix, "_mode")]]
    
    if (identical(mode, "manual")) {
      
      header <- input[[paste0(prefix, "_header")]]
      well_col <- input[[paste0(prefix, "_wellcol")]]
      rfu_col <- input[[paste0(prefix, "_rfucol")]]
      
    } else {
      
      detected <- detect_columns(raw)
      
      validate(need(
        !is.null(detected),
        paste(
          "Could not detect Well and RFU columns.",
          "Select Manual configuration in the sidebar."
        )
      ))
      
      header <- detected$header
      well_col <- detected$well
      rfu_col <- detected$rfu
    }
    
    parse_plate(raw, header, well_col, rfu_col)
  }
  
  standards <- reactive({
    get_plate("std")
  })
  
  standard_layout <- standards_editor_server(
    id = "std_layout",
    standard_data = standards,
    correction_enabled = reactive(
      !identical(input$decay_correction, FALSE)
    )
  )
  
  # ============================================================
  # FILTER STANDARDS ACCORDING TO THE LAYOUT
  # ============================================================
  
  standards_filtered <- reactive({
    
    assigned_wells <- standard_layout() |>
      dplyr::filter(Type != "Unassigned") |>
      dplyr::select(Well, Type)
    
    standards() |>
      dplyr::inner_join(
        assigned_wells,
        by = "Well"
      )
  })
  
  # Each plate is a separate reactive importer + layout editor + outputs.
  # The registration function is called once per stable plate ID.
  register_sample_plate <- function(id) {
    sample_data <- reactive(get_plate(id))
    plate_layout <- samples_editor_server(
      id = paste0(id, "_layout"),
      sample_data = sample_data,
      on_apply = function(value, source) {
        
        target_ids <- sample_plate_ids()
        
        for (target_id in target_ids) {
          
          if (identical(paste0(target_id, "_layout"), source)) {
            next
          }
          
          if (exists(
            target_id,
            envir = sample_layout_registry,
            inherits = FALSE
          )) {
            
            target_layout <- get(
              target_id,
              envir = sample_layout_registry,
              inherits = FALSE
            )
            
            target_layout(value)
          }
        }
        
        showNotification(
          "Layout applied to all sample plates.",
          type = "message"
        )
      }
    )
    
    assign(
      id,
      plate_layout,
      envir = sample_layout_registry
    )
    sample_filtered <- reactive({
      assigned <- plate_layout() |>
        dplyr::filter(Type == "Sample") |>
        dplyr::select(Well, Type)
      sample_data() |>
        dplyr::inner_join(assigned, by = "Well")
    })
    
    output[[paste0(id, "_plate")]] <- plotly::renderPlotly({
      
      selected <- plate_layout() |>
        dplyr::filter(Type == "Sample") |>
        dplyr::pull(Well)
      
      make_plate_plot(
        dat = sample_data(),
        title = "Samples - Raw fluorescence",
        selected_wells = selected
      )
      
    })
    output[[paste0(id, "_filename")]] <- renderText({
      key <- input[[paste0(id, "_source")]]
      if (is.null(key) || !nzchar(key)) return("Choose a file / worksheet in the sidebar")
      idx <- suppressWarnings(as.integer(key))
      src <- sources()
      if (is.na(idx) || idx < 1 || idx > length(src)) return("Choose a file / worksheet")
      names(source_choices())[[idx]]
    })
    observeEvent(input[[paste0("remove_", id)]], {
      ids <- sample_plate_ids()
      if (length(ids) > 1L) {
        sample_plate_ids(setdiff(ids, id))
      }
    })
  }
  
  # First sample plate is available when the app opens.
  register_sample_plate("sample_1")
  
  # Automatically add sample plates for detected worksheets
  observeEvent(sample_sheet_indices(), {
    
    required <- max(
      1L,
      length(sample_sheet_indices())
    )
    
    current <- length(sample_plate_ids())
    
    if (current >= required) {
      return()
    }
    
    for (i in seq_len(required - current)) {
      
      n <- sample_next_id() + 1L
      sample_next_id(n)
      
      id <- paste0("sample_", n)
      
      register_sample_plate(id)
      
      sample_plate_ids(
        c(sample_plate_ids(), id)
      )
    }
  })
  
  # ============================================================
  # PIPETTING ORDER
  # ============================================================
  
  pipetting_order <- pipetting_order_server(
  id = "pipetting",
  standard_layout = standard_layout,
  sample_plate_ids = sample_plate_ids,
  sample_layout_registry = sample_layout_registry,

  plate_source_label = function(plate_id) {

    key <- if (identical(plate_id, "standards")) {
      input$std_source
    } else {
      input[[paste0(plate_id, "_source")]]
    }

    if (is.null(key)) {
      return("No file / worksheet selected")
    }

    idx <- suppressWarnings(as.integer(key))
    src <- sources()

    if (is.na(idx) || idx < 1 || idx > length(src)) {
      return("No file / worksheet selected")
    }

    names(source_choices())[[idx]]
  }
)
  
  
  # ============================================================
  # QUANTIFICATION DATA
  # ============================================================
  
  quantification <- reactive({
    
    order <- pipetting_order() |>
      dplyr::select(Group, Well, Order)
    
    # Standards, including initial and final groups
    std <- standards_filtered() |>
      dplyr::mutate(
        Group = ifelse(
          grepl("initial$", Type),
          "standards_initial",
          "standards_final"
        )
      ) |>
      dplyr::left_join(
        order,
        by = c("Group", "Well")
      )
    
    # Samples from all active plates
    sample_parts <- list()
    
    for (plate_id in sample_plate_ids()) {
      
      if (!exists(
        plate_id,
        envir = sample_layout_registry,
        inherits = FALSE
      )) next
      
      plate_layout <- get(
        plate_id,
        envir = sample_layout_registry,
        inherits = FALSE
      )
      
      assigned <- plate_layout() |>
        dplyr::filter(Type == "Sample") |>
        dplyr::select(Well, Type)
      
      plate_data <- get_plate(plate_id) |>
        dplyr::inner_join(
          assigned,
          by = "Well"
        ) |>
        dplyr::mutate(
          PlateID = plate_id,
          Group = plate_id
        ) |>
        dplyr::left_join(
          order,
          by = c("Group", "Well")
        )
      
      sample_parts[[length(sample_parts) + 1L]] <- plate_data
    }
    
    if (length(sample_parts)) {
      
      sample_data <- dplyr::bind_rows(sample_parts)
      
    } else {
      
      sample_data <- data.frame(
        PlateID = character(),
        Group = character(),
        Well = character(),
        Type = character(),
        RFU = numeric(),
        Order = numeric()
      )
    }
    
    calculate_dna(
      standards = std,
      samples = sample_data,
      assay = input$assay,
      assayVol = input$standard_volume,
      sampleVol = input$sample_volume,
      correct_decay = isTRUE(input$decay_correction)
    )
  })
  
  # ============================================================
  # QUANTIFICATION STATUS
  # ============================================================
  
  output$quant_status <- renderUI({
    
    result <- quantification()
    
    tags$div(
      style = paste(
        "padding:10px;",
        "border:1px solid #dce3eb;",
        "border-radius:8px;",
        "margin-bottom:12px;"
      ),
      tags$strong("Quantification: "),
      result$message
    )
  })
  
  
  # ============================================================
  # MULTIPLICATIVE DECAY DIAGNOSTICS
  # ============================================================
  
  output$decay_summary <- renderUI({
    
    result <- quantification()
    
    if (!isTRUE(result$correction_applied)) {
      return(NULL)
    }
    
    if (!identical(
      result$message,
      "Quantification calculated."
    )) {
      return(NULL)
    }
    
    gamma <- result$gamma
    
    percent_step <- 100 * (exp(gamma) - 1)
    
    fmt <- function(x, digits = 4) {
      formatC(
        x,
        format = "f",
        digits = digits
      )
    }
    
    tags$div(
      style = paste(
        "display:flex;",
        "gap:12px;",
        "flex-wrap:wrap;",
        "margin-bottom:16px;"
      ),
      
      tags$div(
        style = paste(
          "border:1px solid #dce3eb;",
          "border-radius:8px;",
          "padding:12px 16px;",
          "min-width:180px;"
        ),
        tags$div("Decay coefficient (γ)"),
        tags$strong(
          fmt(gamma, 6),
          style = "font-size:22px;"
        )
      ),
      
      tags$div(
        style = paste(
          "border:1px solid #dce3eb;",
          "border-radius:8px;",
          "padding:12px 16px;",
          "min-width:180px;"
        ),
        tags$div("RFU change per pipetting step"),
        tags$strong(
          paste0(fmt(percent_step, 2), "%"),
          style = "font-size:22px;"
        )
      ),
      
      tags$div(
        style = paste(
          "border:1px solid #dce3eb;",
          "border-radius:8px;",
          "padding:12px 16px;",
          "min-width:180px;"
        ),
        tags$div("Reference point"),
        tags$strong(
          paste(fmt(result$order_ref, 0), "st pipetting"),
          style = "font-size:22px;"
        )
      )
    )
  })
  
  # ============================================================
  # BEFORE / AFTER DECAY PLOT - RFU SCALE
  # Faceted by standard type
  # ============================================================
  
  make_decay_diagnostic <- function(result, corrected = FALSE) {
    
    standards <- result$standards
    
    standards <- standards |>
      dplyr::mutate(
        Standard = factor(
          ifelse(
            stdConcentration == 0,
            "ST1",
            "ST2"
          ),
          levels = c("ST1", "ST2")
        ),
        
        PlotRFU = if (corrected) cRFU else RFU,
        
        Tooltip = paste0(
          "Well: ", Well,
          "<br>Type: ", Type,
          "<br>Order: ", Order,
          "<br>RFU: ", signif(PlotRFU, 6)
        )
      )
    
    # Predictions from the original multiplicative model.
    # The predictions are transformed back to RFU scale.
    
    concentration_levels <- sort(
      unique(standards$stdConcentration)
    )
    
    prediction_grid <- expand.grid(
      Order = seq(
        min(standards$Order),
        max(standards$Order),
        length.out = 100
      ),
      stdConcentration = concentration_levels
    )
    
    prediction_grid$Standard <- factor(
      ifelse(
        prediction_grid$stdConcentration == 0,
        "ST1",
        "ST2"
      ),
      levels = c("ST1", "ST2")
    )
    
    predicted_log_rfu <- as.numeric(
      stats::predict(
        result$fit,
        newdata = prediction_grid
      )
    )
    
    if (corrected) {
      
      predicted_log_rfu <- predicted_log_rfu -
        result$gamma *
        (prediction_grid$Order - result$order_ref)
    }
    
    prediction_grid$PredictedRFU <- exp(
      predicted_log_rfu
    )
    
    # Plot measured data and model predictions
    
    p <- ggplot2::ggplot(
      standards,
      ggplot2::aes(
        x = Order,
        y = PlotRFU,
        color = Standard
      )
    ) +
      
      ggplot2::geom_point(
        ggplot2::aes(
          text = Tooltip
        ),
        size = 2.8,
        alpha = 0.85
      ) +
      
      ggplot2::geom_line(
        data = prediction_grid,
        ggplot2::aes(
          x = Order,
          y = PredictedRFU,
          color = Standard,
          group = Standard
        ),
        inherit.aes = FALSE,
        linewidth = 1
      ) +
      
      ggplot2::facet_wrap(
        ~Standard,
        ncol = 1,
        scales = "free_y"
      ) +
      
      ggplot2::scale_color_manual(
        values = c(
          "ST1" = "#03A9F4",
          "ST2" = "#E5A900"
        )
      ) +
      
      ggplot2::scale_y_continuous(
        labels = scales::label_comma()
      ) +
      
      ggplot2::labs(
        x = "Pipetting order",
        y = if (corrected) {
          "Corrected RFU"
        } else {
          "RFU"
        },
        color = "Standard"
      ) +
      
      ggplot2::theme_minimal(
        base_size = 13
      ) +
      
      ggplot2::theme(
        legend.position = "none",
        strip.text = ggplot2::element_text(
          face = "bold",
          size = 12
        ),
        panel.grid.minor = ggplot2::element_blank()
      )
    
    plotly::ggplotly(
      p,
      tooltip = "text"
    ) |>
      plotly::layout(
        margin = list(
          l = 65,
          r = 20,
          b = 55,
          t = 30
        )
      ) |>
      plotly::config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  }
  
  
  output$decay_before <- plotly::renderPlotly({
    
    result <- quantification()
    
    shiny::req(isTRUE(result$correction_applied))
    
    shiny::validate(
      shiny::need(
        identical(
          result$message,
          "Quantification calculated."
        ),
        result$message
      )
    )
    
    make_decay_diagnostic(
      result,
      corrected = FALSE
    )
  })
  
  
  output$decay_after <- plotly::renderPlotly({
    
    result <- quantification()
    
    shiny::req(isTRUE(result$correction_applied))
    
    shiny::validate(
      shiny::need(
        identical(
          result$message,
          "Quantification calculated."
        ),
        result$message
      )
    )
    
    make_decay_diagnostic(
      result,
      corrected = TRUE
    )
  })
  
  # ============================================================
  # CALIBRATION - DNA IN THE ASSAY WELL
  # ============================================================
  
  output$calibration_plot <- plotly::renderPlotly({
    
    result <- quantification()
    
    shiny::validate(
      shiny::need(
        identical(result$message, "Quantification calculated."),
        result$message
      )
    )
    
    # All measurements have the same final reaction volume.
    # Configurable final reaction volume
    well_volume <- input$well_volume
    
    shiny::validate(
      shiny::need(
        is.finite(well_volume) &&
          well_volume >= max(
            input$standard_volume,
            input$sample_volume
          ),
        "Final assay volume must be at least as large as the input volumes."
      )
    )
    
    standard_volume <- input$standard_volume
    sample_volume <- input$sample_volume
    
    # ----------------------------------------------------------
    # STANDARDS: KNOWN DNA AMOUNT AND WELL CONCENTRATION
    # ----------------------------------------------------------
    
    standards <- result$standards |>
      dplyr::mutate(
        Standard = ifelse(
          stdConcentration == 0,
          "ST1",
          "ST2"
        ),
        
        DNA_ng = stdConcentration * standard_volume,
        
        WellConcentration = DNA_ng / well_volume,
        
        Tooltip = paste0(
          "<b>", Type, "</b>",
          "<br>Well: ", Well,
          "<br>Corrected RFU: ", signif(cRFU, 6),
          "<br>Original standard concentration: ",
          signif(stdConcentration, 5), " ng/µL",
          "<br>Standard volume: ", standard_volume, " µL",
          "<br>DNA amount in well: ",
          signif(DNA_ng, 5), " ng",
          "<br>DNA concentration in well: ",
          signif(WellConcentration, 5), " ng/µL"
        )
      )
    
    # ----------------------------------------------------------
    # SAMPLES: ESTIMATED DNA AMOUNT AND WELL CONCENTRATION
    # ----------------------------------------------------------
    
    samples <- result$samples |>
      dplyr::filter(
        is.finite(cRFU),
        is.finite(DNA_ng_uL)
      ) |>
      dplyr::mutate(
        DNA_ng = DNA_ng_uL * sample_volume,
        
        WellConcentration = DNA_ng / well_volume,
        
        Tooltip = paste0(
          "<b>Sample: ", Well, "</b>",
          "<br>Plate: ", PlateID,
          "<br>Corrected RFU: ", signif(cRFU, 6),
          "<br>Original DNA concentration: ",
          signif(DNA_ng_uL, 5), " ng/µL",
          "<br>Sample volume: ", sample_volume, " µL",
          "<br>DNA amount in well: ",
          signif(DNA_ng, 5), " ng",
          "<br>DNA concentration in well: ",
          signif(WellConcentration, 5), " ng/µL",
          "<br>QC: ", QC
        )
      )
    
    # ----------------------------------------------------------
    # CALIBRATION FROM MEDIAN ST1 AND ST2 SIGNALS
    # ----------------------------------------------------------
    
    st2_concentration <- if (input$assay == "HS") 10 else 100
    
    st2_mass <- st2_concentration * standard_volume
    st2_well_concentration <- st2_mass / well_volume
    
    calibration_line <- data.frame(
      RFU = c(result$blank, result$st2),
      WellConcentration = c(0, st2_well_concentration),
      DNA_ng = c(0, st2_mass)
    )
    
    # ----------------------------------------------------------
    # PLOT
    # ----------------------------------------------------------
    
    p <- plotly::plot_ly()
    
    # Calibration line
    p <- p |>
      plotly::add_lines(
        data = calibration_line,
        x = ~RFU,
        y = ~WellConcentration,
        name = "Calibration",
        line = list(
          color = "#475569",
          width = 2.5
        ),
        hoverinfo = "skip",
        showlegend = FALSE
      )
    
    # ST1
    p <- p |>
      plotly::add_markers(
        data = dplyr::filter(standards, Standard == "ST1"),
        x = ~cRFU,
        y = ~WellConcentration,
        text = ~Tooltip,
        hoverinfo = "text",
        name = "ST1",
        marker = list(
          color = "#03A9F4",
          size = 9,
          opacity = 0.85
        )
      )
    
    # ST2
    p <- p |>
      plotly::add_markers(
        data = dplyr::filter(standards, Standard == "ST2"),
        x = ~cRFU,
        y = ~WellConcentration,
        text = ~Tooltip,
        hoverinfo = "text",
        name = "ST2",
        marker = list(
          color = "#E5A900",
          size = 9,
          opacity = 0.85
        )
      )
    
    # Samples
    p <- p |>
      plotly::add_markers(
        data = samples,
        x = ~cRFU,
        y = ~WellConcentration,
        text = ~Tooltip,
        hoverinfo = "text",
        name = "Samples",
        marker = list(
          color = "#10B981",
          size = 7,
          opacity = 0.8
        )
      )
    
    # ----------------------------------------------------------
    # EXTRAPOLATION ABOVE ST2
    # ----------------------------------------------------------
    
    max_rfu <- max(
      c(result$st2, samples$cRFU),
      na.rm = TRUE
    )
    
    if (max_rfu > result$st2) {
      
      extrapolation <- data.frame(
        RFU = c(result$st2, max_rfu),
        
        WellConcentration = c(
          st2_well_concentration,
          st2_well_concentration *
            (max_rfu - result$blank) /
            (result$st2 - result$blank)
        )
      )
      
      p <- p |>
        plotly::add_lines(
          data = extrapolation,
          x = ~RFU,
          y = ~WellConcentration,
          line = list(
            color = "#94A3B8",
            width = 2,
            dash = "dash"
          ),
          hoverinfo = "skip",
          showlegend = FALSE
        )
    }
    
    # ----------------------------------------------------------
    # SECOND Y AXIS: DNA AMOUNT (NG)
    #
    # This is an exact conversion:
    # DNA amount = well concentration * 200 µL
    # ----------------------------------------------------------
    
    p <- p |>
      plotly::add_trace(
        type = "scatter",
        mode = "lines",
        x = calibration_line$RFU,
        y = calibration_line$DNA_ng,
        yaxis = "y2",
        line = list(
          color = "rgba(0,0,0,0)",
          width = 0
        ),
        opacity = 0,
        hoverinfo = "skip",
        showlegend = FALSE
      )
    
    y_max <- max(
      c(
        standards$WellConcentration,
        samples$WellConcentration
      ),
      na.rm = TRUE
    )
    
    y_max <- max(
      y_max * 1.10,
      st2_well_concentration * 1.10
    )
    
    # ----------------------------------------------------------
    # AXES AND LAYOUT
    # ----------------------------------------------------------
    
    p |>
      plotly::layout(
        
        xaxis = list(
          title = "Corrected RFU",
          tickformat = ",",
          zeroline = FALSE,
          showgrid = TRUE
        ),
        
        yaxis = list(
          title = "DNA concentration in well (ng/µL)",
          range = c(0, y_max),
          zeroline = FALSE,
          showgrid = TRUE,
          fixedrange = TRUE
        ),
        
        yaxis2 = list(
          title = "DNA amount in well (ng)",
          overlaying = "y",
          side = "right",
          range = c(0, y_max * well_volume),
          zeroline = FALSE,
          showgrid = FALSE,
          fixedrange = TRUE
        ),
        
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 1.12
        ),
        
        margin = list(
          l = 90,
          r = 95,
          b = 60,
          t = 65
        ),
        
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        hovermode = "closest"
      ) |>
      plotly::config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  })
  
  
  
  # ============================================================
  # COMBINED QUANTIFICATION RESULTS
  # ============================================================
  
  processing_results <- reactive({
    
    result <- quantification()
    
    validate(
      need(
        identical(
          result$message,
          "Quantification calculated."
        ),
        result$message
      )
    )
    
    dat <- result$samples
    
    ids <- sample_plate_ids()
    
    dat |>
      dplyr::mutate(
        Plate = paste(
          "Sample plate",
          match(PlateID, ids)
        ),
        
        `log10 RFU` = ifelse(
          is.finite(RFU) & RFU > 0,
          log10(RFU),
          NA_real_
        ),
        
        `log10 cRFU` = ifelse(
          is.finite(cRFU) & cRFU > 0,
          log10(cRFU),
          NA_real_
        )
      ) |>
      dplyr::select(
        Plate,
        PlateID,
        Well,
        RFU,
        `log10 RFU`,
        Order,
        cRFU,
        `log10 cRFU`,
        cRFU_bg,
        assayConcentration,
        DNA_ng_uL,
        QC
      ) |>
      dplyr::rename(
        `Corrected RFU` = cRFU,
        `Background-corrected RFU` = cRFU_bg,
        `Assay concentration (ng/uL)` = assayConcentration,
        `DNA concentration (ng/uL)` = DNA_ng_uL
      ) |>
      dplyr::arrange(Order, Plate, Well)
  })
  
  # ============================================================
  # RESULTS TABLE
  # ============================================================
  
  output$results_table <- DT::renderDT({
    
    dat <- processing_results()
    
    DT::datatable(
      dat,
      rownames = FALSE,
      
      extensions = "Buttons",
      
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        searching = TRUE,
        ordering = TRUE,
        autoWidth = TRUE,
        dom = "ftip"
      )
    ) |>
      DT::formatRound(
        columns = c(
          "log10 RFU",
          "Corrected RFU",
          "log10 cRFU",
          "Background-corrected RFU",
          "Assay concentration (ng/uL)",
          "DNA concentration (ng/uL)"
        ),
        digits = 3
      )
  })
  
  
  # ============================================================
  # EXPORT RESULTS + MODEL INFORMATION
  # ============================================================
  
  output$download_results <- downloadHandler(
    
    filename = function() {
      paste0(
        "FluoR_results_",
        Sys.Date(),
        ".xlsx"
      )
    },
    
    content = function(file) {
      
      result <- quantification()
      
      if (!identical(
        result$message,
        "Quantification calculated."
      )) {
        stop(result$message)
      }
      
      results <- processing_results()
      
      model_info <- data.frame(
        Parameter = c(
          "Assay",
          "Fluorescence correction",
          "Standard volume (uL)",
          "Sample volume (uL)",
          "Decay coefficient gamma",
          "RFU change per order step (%)",
          "Reference order",
          "ST1 fluorescence median",
          "ST2 fluorescence median",
          "Calibration slope"
        ),
        
        Value = as.character(c(
          input$assay,
          
          if (isTRUE(result$correction_applied)) {
            "Multiplicative decay correction"
          } else {
            "No correction"
          },
          
          input$standard_volume,
          input$sample_volume,
          
          result$gamma,
          
          if (isTRUE(result$correction_applied)) {
            100 * (exp(result$gamma) - 1)
          } else {
            NA_real_
          },
          
          result$order_ref,
          result$blank,
          result$st2,
          result$slope
        ))
      )
      
      writexl::write_xlsx(
        list(
          "Results" = results,
          "Standards" = result$standards,
          "Calibration" = result$calibration,
          "Model parameters" = model_info
        ),
        path = file
      )
    }
  )
  
  
  output$sample_sections <- renderUI({
    
    ids <- sample_plate_ids()
    
    tagList(
      lapply(seq_along(ids), function(k) {
        
        id <- ids[k]
        
        tags$div(
          class = "sample-section",
          
          h4(
            paste("Sample plate", k),
            style = "margin:0 0 3px 0;"
          ),
          
          tags$div(
            class = "sample-file-name",
            textOutput(
              paste0(id, "_filename"),
              inline = TRUE
            )
          ),
          
          samples_editor_ui(
            id = paste0(id, "_layout"),
            plot_id = paste0(id, "_plate")
          )
        )
      })
    )
  })
  
  # ----------------------------------------------------------
  # INFORMATION / WARNINGS
  # ----------------------------------------------------------
  
  plate_info <- function(dat) {
    
    n_negative <- sum(dat$RFU < 0, na.rm = TRUE)
    n_missing <- sum(is.na(dat$RFU))
    n_duplicate <- sum(duplicated(dat$Well))
    
    paste(
      nrow(dat), "well records.",
      n_negative, "negative RFU values.",
      n_missing, "missing RFU values.",
      n_duplicate, "duplicate well identifiers."
    )
  }
  
  output$std_info <- renderText({
    plate_info(standards())
  })
  
  
  # ============================================================
  # INTERACTIVE PLATE VISUALIZATION
  # Compact, aligned and with fixed colorbar space
  # ============================================================
  
  make_plate_plot <- function(dat, title, selected_wells = character()) {
    
    # Complete 96-well plate
    grid <- expand.grid(
      Row = LETTERS[1:8],
      Column = 1:12,
      stringsAsFactors = FALSE
    )
    
    grid$Well <- paste0(grid$Row, grid$Column)
    
    measured <- dat |>
      dplyr::select(Well, RFU) |>
      dplyr::distinct(Well, .keep_all = TRUE)
    
    plot_dat <- grid |>
      dplyr::left_join(measured, by = "Well")
    
    # A at the top, H at the bottom
    # Slightly increase spacing between wells
    spacing <- 1.2
    
    plot_dat$X <- 1 + (plot_dat$Column - 1) * spacing
    plot_dat$Y <- 1 + (8 - match(plot_dat$Row, LETTERS[1:8])) * spacing
    
    # Tooltip for each well
    plot_dat$Tooltip <- ifelse(
      is.finite(plot_dat$RFU),
      paste0(
        "<b>Well: ", plot_dat$Well, "</b>",
        "<br>RFU: ",
        format(
          plot_dat$RFU,
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        )
      ),
      paste0(
        "<b>Well: ", plot_dat$Well, "</b>",
        "<br>No measurement"
      )
    )
    
    measured_wells <- plot_dat |>
      dplyr::filter(is.finite(RFU))
    
    empty_wells <- plot_dat |>
      dplyr::filter(!is.finite(RFU))
    
    # Measured wells not selected: muted fluorescence
    unselected_measured <- measured_wells |>
      dplyr::filter(!Well %in% selected_wells)
    
    # Measured wells selected: full fluorescence
    selected_measured <- measured_wells |>
      dplyr::filter(Well %in% selected_wells)
    
    # Fixed scale limits so the colorbar area is always reserved
    scale_min <- 0
    scale_max <- if (nrow(measured_wells) > 0) {
      max(measured_wells$RFU, na.rm = TRUE)
    } else {
      1
    }
    
    if (!is.finite(scale_max) || scale_max <= scale_min) {
      scale_max <- scale_min + 1
    }
    
    p <- plotly::plot_ly()
    
    # ----------------------------------------------------------
    # Unmeasured wells: grey
    # ----------------------------------------------------------
    
    p <- p |>
      plotly::add_markers(
        data = empty_wells,
        x = ~X,
        y = ~Y,
        text = ~Tooltip,
        hoverinfo = "text",
        marker = list(
          symbol = "circle",
          size = 34,
          color = "#E5E7EB",
          line = list(
            color = "#64748B",
            width = 1
          )
        ),
        showlegend = FALSE
      )
    
    # ----------------------------------------------------------
    # Unselected measured wells: muted fluorescence
    # ----------------------------------------------------------
    
    if (nrow(unselected_measured) > 0) {
      
      p <- p |>
        plotly::add_trace(
          data = unselected_measured,
          type = "scatter",
          mode = "markers",
          x = ~X,
          y = ~Y,
          text = ~Tooltip,
          hoverinfo = "text",
          showlegend = FALSE,
          opacity = 0.50,
          
          marker = list(
            symbol = "circle",
            size = 34,
            color = unselected_measured$RFU,
            colorscale = "Viridis",
            cmin = scale_min,
            cmax = scale_max,
            
            # Show scale only if no selected wells exist
            showscale = nrow(selected_measured) == 0,
            
            line = list(
              color = "#64748B",
              width = 1
            ),
            
            colorbar = list(
              title = list(text = "RFU", side = "top"),
              len = 0.42,
              lenmode = "fraction",
              thickness = 12,
              x = 1.02,
              y = 0.5,
              yanchor = "middle",
              outlinewidth = 0.5
            )
          )
        )
    }
    
    
    # ----------------------------------------------------------
    # Selected wells: white halo and blue selection outline
    # ----------------------------------------------------------
    
    if (nrow(selected_measured) > 0) {
      
      # Outer white halo
      p <- p |>
        plotly::add_trace(
          data = selected_measured,
          type = "scatter",
          mode = "markers",
          x = ~X,
          y = ~Y,
          hoverinfo = "skip",
          showlegend = FALSE,
          
          marker = list(
            symbol = "circle",
            size = 42,
            color = "rgba(0,0,0,0)",
            line = list(
              color = "#FFFFFF",
              width = 5
            )
          )
        )
      
      # Blue selection outline
      p <- p |>
        plotly::add_trace(
          data = selected_measured,
          type = "scatter",
          mode = "markers",
          x = ~X,
          y = ~Y,
          hoverinfo = "skip",
          showlegend = FALSE,
          
          marker = list(
            symbol = "circle",
            size = 39,
            color = "rgba(0,0,0,0)",
            line = list(
              color = "#2563EB",
              width = 2
            )
          )
        )
    }
    
    # ----------------------------------------------------------
    # Selected measured wells: full fluorescence
    # ----------------------------------------------------------
    
    if (nrow(selected_measured) > 0) {
      
      p <- p |>
        plotly::add_trace(
          data = selected_measured,
          type = "scatter",
          mode = "markers",
          x = ~X,
          y = ~Y,
          text = ~Tooltip,
          hoverinfo = "text",
          showlegend = FALSE,
          opacity = 1,
          
          marker = list(
            symbol = "circle",
            size = 34,
            color = selected_measured$RFU,
            colorscale = "Viridis",
            cmin = scale_min,
            cmax = scale_max,
            showscale = TRUE,
            
            line = list(
              color = "#64748B",
              width = 1
            ),
            
            colorbar = list(
              title = list(text = "RFU", side = "top"),
              len = 0.42,
              lenmode = "fraction",
              thickness = 12,
              x = 1.02,
              y = 0.5,
              yanchor = "middle",
              outlinewidth = 0.5
            )
          )
        )
    }
    
    p |>
      plotly::layout(
        
        xaxis = list(
          title = "",
          side = "top",
          tickmode = "array",
          tickvals = 1 + (0:11) * spacing,
          ticktext = as.character(1:12),
          range = c(
            1 - 0.65 * spacing,
            1 + 11 * spacing + 0.65 * spacing
          ),
          tickfont = list(
            size = 12,
            color = "#5B6472"
          ),
          zeroline = FALSE,
          showgrid = FALSE,
          showline = FALSE,
          ticks = "outside",
          ticklen = 2,
          tickcolor = "rgba(0,0,0,0)",
          fixedrange = TRUE,
          automargin = TRUE
        ),
        
        yaxis = list(
          title = "",
          tickmode = "array",
          tickvals = 1 + (7:0) * spacing,
          ticktext = LETTERS[1:8],
          tickfont = list(
            size = 12,
            color = "#5B6472"
          ),
          range = c(
            1 - 0.45 * spacing,
            1 + 7 * spacing + 0.55 * spacing
          ),
          zeroline = FALSE,
          showgrid = FALSE,
          showline = FALSE,
          ticks = "outside",
          ticklen = 5,
          tickcolor = "rgba(0,0,0,0)",
          fixedrange = TRUE,
          automargin = TRUE
        ),
        
        margin = list(
          l = 22,
          r = 30,
          b = 30,
          t = 0
        ),
        
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        hovermode = "closest",
        showlegend = FALSE
      ) |>
      plotly::config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
  }
  
  # ============================================================
  # STANDARDS PLATE
  # ============================================================
  
  output$std_plate <- renderPlotly({
    
    selected <- standard_layout() |>
      dplyr::filter(Type != "Unassigned") |>
      dplyr::pull(Well)
    
    make_plate_plot(
      dat = standards(),
      title = "Standards - Raw fluorescence",
      selected_wells = selected
    )
    
  })
  
  
}

shinyApp(ui, server)
