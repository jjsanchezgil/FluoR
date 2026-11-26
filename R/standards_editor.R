# ============================================================
# FluoR - Standards Layout Editor
# v0.1
# ============================================================

library(shiny)
library(dplyr)
library(readxl)
library(writexl)
library(DT)

std_categories <- c(
  "ST1 initial", "ST2 initial", "ST1 final", "ST2 final"
)

std_colors <- c(
  "ST1 initial" = "#03A9F4",
  "ST2 initial" = "#FFC107",
  "ST1 final" = "#B3E5FC",
  "ST2 final" = "#FFECB3",
  "Unassigned" = "#E5E7EB"
)

empty_std_layout <- function() {
  expand.grid(
    Row = LETTERS[1:8],
    Column = 1:12,
    stringsAsFactors = FALSE
  ) |>
    mutate(Well = paste0(Row, Column), Type = "Unassigned")
}

normalize_std_type <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("[[:space:]-]+", "_", x)
  out <- rep("Unassigned", length(x))
  
  aliases <- list(
    "ST1 initial" = c("ST1_INI", "ST1_INITIAL", "ST1_START"),
    "ST2 initial" = c("ST2_INI", "ST2_INITIAL", "ST2_START"),
    "ST1 final"   = c("ST1_FIN", "ST1_FINAL", "ST1_END"),
    "ST2 final"   = c("ST2_FIN", "ST2_FINAL", "ST2_END")
  )
  
  for (category in names(aliases)) {
    out[x %in% aliases[[category]]] <- category
  }
  
  blank <- is.na(x) | x %in% c("", "NA", "EMPTY", "UNASSIGNED", "NONE")
  unknown <- !blank & out == "Unassigned"
  
  if (any(unknown)) {
    stop("Unknown standard labels: ", paste(unique(x[unknown]), collapse = ", "))
  }
  out
}

# Read an Excel plate with A-H row labels and columns 1-12,
# or a plain 8x12 matrix without headers.
read_std_layout <- function(path) {
  raw <- readxl::read_excel(
    path, sheet = 1, col_names = FALSE, col_types = "text",
    .name_repair = "minimal"
  )
  mat <- as.matrix(raw)
  mat[is.na(mat)] <- ""
  mat <- trimws(mat)
  values <- NULL
  
  if (nrow(mat) >= 8 && ncol(mat) >= 13) {
    for (c in seq_len(min(4L, ncol(mat) - 12L))) {
      for (r in seq_len(nrow(mat) - 7L)) {
        labels <- toupper(mat[r:(r + 7L), c])
        if (identical(as.character(labels), LETTERS[1:8])) {
          values <- mat[r:(r + 7L), (c + 1L):(c + 12L), drop = FALSE]
          break
        }
      }
      if (!is.null(values)) break
    }
  }
  
  if (is.null(values)) {
    if (ncol(mat) < 12L || nrow(mat) < 8L) {
      stop("Expected an 8 x 12 plate layout.")
    }
    start_row <- 1L
    if (identical(as.character(mat[1, 1:12]), as.character(1:12))) {
      start_row <- 2L
    }
    if (nrow(mat) < start_row + 7L) stop("Incomplete plate layout.")
    values <- mat[start_row:(start_row + 7L), 1:12, drop = FALSE]
  }
  
  result <- empty_std_layout()
  for (r in seq_len(8)) {
    for (c in seq_len(12)) {
      idx <- match(paste0(LETTERS[r], c), result$Well)
      result$Type[idx] <- normalize_std_type(values[r, c])
    }
  }
  result
}

write_std_layout <- function(layout, path) {
  values <- matrix("", nrow = 8, ncol = 12)
  codes <- c(
    "ST1 initial" = "ST1_ini", "ST2 initial" = "ST2_ini",
    "ST1 final" = "ST1_fin", "ST2 final" = "ST2_fin"
  )
  assigned <- layout[layout$Type != "Unassigned", , drop = FALSE]
  for (i in seq_len(nrow(assigned))) {
    r <- match(assigned$Row[i], LETTERS[1:8])
    c <- assigned$Column[i]
    values[r, c] <- unname(codes[assigned$Type[i]])
  }
  export <- data.frame(Row = LETTERS[1:8], values, check.names = FALSE)
  names(export) <- c("Row", as.character(1:12))
  writexl::write_xlsx(list("Standards layout" = export), path)
}

# Render interactive HTML wells. No ggplot/brush involved.
std_plate_grid <- function(layout, ns) {
  cells <- list(tags$span(class = "fluor-axis"))
  for (c in 1:12) {
    cells[[length(cells) + 1L]] <- tags$span(class = "fluor-axis", as.character(c))
  }
  for (r in seq_len(8)) {
    cells[[length(cells) + 1L]] <- tags$span(
      class = "fluor-axis", LETTERS[r]
    )
    for (c in 1:12) {
      well <- paste0(LETTERS[r], c)
      category <- layout$Type[match(well, layout$Well)]
      cells[[length(cells) + 1L]] <- tags$div(
        class = "fluor-well",
        `data-well` = well,
        `data-row` = r,
        `data-col` = c,
        style = sprintf("--well-color:%s;", unname(std_colors[category])),
        title = paste(well, category, sep = " - "),
        well
      )
    }
  }
  tags$div(
    class = "fluor-plate-scroll",
    do.call(tags$div, c(list(
      id = ns("well_grid"), class = "fluor-well-grid"
    ), cells))
  )
}

standards_editor_ui <- function(id) {
  ns <- NS(id)
  board_id <- ns("well_grid")
  selection_id <- ns("well_selection")
  
  # The highlight is only a transient drag preview. It is removed in
  # pointerup, BEFORE sending the selection to Shiny.
  js <- sprintf("(function() {
    const boardId = '%s';
    const inputId = '%s';
    let dragging = false;
    let start = null;
    let end = null;

    function board() { return document.getElementById(boardId); }
    function getWell(evt) {
      const root = board();
      if (!root) return null;
      const el = document.elementFromPoint(evt.clientX, evt.clientY);
      const well = el && el.closest('[data-well]');
      return well && root.contains(well) ? well : null;
    }
    function position(el) {
      return { row: Number(el.dataset.row), col: Number(el.dataset.col) };
    }
    function chosen(root) {
      if (!start || !end) return [];
      const r0 = Math.min(start.row, end.row), r1 = Math.max(start.row, end.row);
      const c0 = Math.min(start.col, end.col), c1 = Math.max(start.col, end.col);
      return Array.from(root.querySelectorAll('[data-well]')).filter(el => {
        const r = Number(el.dataset.row), c = Number(el.dataset.col);
        return r >= r0 && r <= r1 && c >= c0 && c <= c1;
      });
    }
    function clearPreview(root) {
      if (root) root.querySelectorAll('.fluor-preview').forEach(el =>
        el.classList.remove('fluor-preview'));
    }
    function preview() {
      const root = board();
      if (!root) return;
      clearPreview(root);
      chosen(root).forEach(el => el.classList.add('fluor-preview'));
    }
    document.addEventListener('pointerdown', function(evt) {
      if (evt.button !== 0 || dragging) return;
      const root = board();
      const well = getWell(evt);
      if (!root || !well) return;
      dragging = true;
      start = position(well);
      end = start;
      evt.preventDefault();
      preview();
    });
    document.addEventListener('pointermove', function(evt) {
      if (!dragging) return;
      const well = getWell(evt);
      if (!well) return;
      end = position(well);
      preview();
    });
    document.addEventListener('pointerup', function(evt) {
      if (!dragging) return;
      const root = board();
      const well = getWell(evt);
      if (well) end = position(well);
      const wells = root ? chosen(root).map(el => el.dataset.well) : [];
      clearPreview(root); // Immediate, at mouse button release
      dragging = false;
      start = null;
      end = null;
      if (wells.length && window.Shiny) {
        Shiny.setInputValue(inputId, wells, {priority: 'event'});
      }
    });
    function cancel() {
      clearPreview(board());
      dragging = false;
      start = null;
      end = null;
    }
    document.addEventListener('pointercancel', cancel);
    window.addEventListener('blur', cancel);
  })();", board_id, selection_id)
  
  css <- "
    .fluor-plate-scroll {
  overflow-x: auto;
  padding: 2px 0;
  margin: 0;
}
   
.fluor-well-grid {
  display: grid;
  grid-template-columns: 22px repeat(12, minmax(0, 1fr));
  grid-template-rows: 26px repeat(8, minmax(0, 1fr));
  width: 100%;
  max-width: 580px;
  aspect-ratio: 580 / 405;
  align-items: center;
  justify-items: center;
  user-select: none;
  -webkit-user-select: none;
  touch-action: none;
}
    .fluor-axis { font-size: 12px; color: #5b6472; }
    .fluor-well {
      width: min(36px, calc(100% - 4px));
      height: auto;
      aspect-ratio: 1;
      border-radius: 50%;
      display: flex; align-items: center; justify-content: center;
      background: var(--well-color);
      border: 1px solid #64748b;
      color: #1f2937; font-size: 11px;
      cursor: crosshair;
      box-sizing: border-box;
    }
    .fluor-well.fluor-preview {
      outline: 3px solid #2563eb;
      outline-offset: 2px;
    }
  "
  
  tagList(
    tags$style(HTML(css)),
    tags$script(HTML(js)),
    tags$style(HTML("
  
/* ================================================
   FluoR - Compact standard editor
   ================================================ */

/* Category selector */
.fluor-category-picker {
  margin: 0 0 4px 0;
  padding: 0;
}

.fluor-category-picker .shiny-input-container {
  margin-bottom: 0 !important;
  width: 100%;
}

.fluor-category-picker .shiny-options-group {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 6px;
}

/* Category buttons */
.fluor-category-picker .radio-inline {
  position: relative;
  margin: 0 !important;
  padding: 5px 10px !important;
  border: 2px solid #cbd5e1;
  border-radius: 8px;
  background: white;
  cursor: pointer;
  font-size: 13px;
  font-weight: 500;
  line-height: 1.4;
  transition: background 0.15s, border-color 0.15s;
}

.fluor-category-picker input[type='radio'] {
  position: absolute;
  opacity: 0;
  width: 1px;
  height: 1px;
}

.fluor-category-picker .radio-inline:has(input:checked) {
  border-color: #2563eb;
  background: #eff6ff;
  box-shadow: 0 0 0 1px #2563eb;
}

.fluor-category-picker .radio-inline:hover {
  border-color: #64748b;
}

.fluor-category-picker .radio-inline:focus-within {
  outline: 2px solid #2563eb;
  outline-offset: 2px;
}

/* Category colors */
.fluor-category-picker .radio-inline:nth-child(1) {
  border-left: 7px solid #03A9F4;
}

.fluor-category-picker .radio-inline:nth-child(2) {
  border-left: 7px solid #FFC107;
}

.fluor-category-picker .radio-inline:nth-child(3) {
  border-left: 7px solid #B3E5FC;
}

.fluor-category-picker .radio-inline:nth-child(4) {
  border-left: 7px solid #FFECB3;
}

.fluor-category-picker .radio-inline:nth-child(5) {
  border-left: 7px solid #e5e7eb;
}

/* Reduce vertical spacing around editor */
.fluor-category-picker + div {
  margin-top: 0 !important;
}

/* Compact right-side controls */
.fluor-standards-editor .form-group {
  margin-bottom: 10px;
}

.fluor-standards-editor hr {
  margin-top: 12px;
  margin-bottom: 12px;
}

/* ================================================
   Compact statistics cards
   ================================================ */

.fluor-statistics-panel {
  margin-top: -2px;
}

.fluor-statistics-panel .shiny-html-output {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 12px;
}

.fluor-stat-card {
    margin-bottom: 0;
}

@media (max-width: 850px) {
    .fluor-statistics-panel .shiny-html-output {
        grid-template-columns: 1fr;
    }
}

.fluor-stat-card {
  border: 1px solid #dce3eb;
  border-left: 5px solid #94a3b8;
  border-radius: 8px;
  background: #ffffff;
  padding: 8px 10px;
  margin-bottom: 6px;
}

.fluor-stat-card-header {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
  margin-bottom: 6px;
}

.fluor-stat-card-title {
  font-size: 20px;
  line-height: 1.2;
}

.fluor-stat-card-count {
  font-size: 16px;
  color: #64748b;
  white-space: nowrap;
}

.fluor-stat-metrics {
  display: flex;
  flex-wrap: wrap;
  gap: 14px;
  align-items: baseline;
  margin-bottom: 5px;
}

.fluor-stat-metric {
  display: inline-flex;
  align-items: baseline;
  gap: 4px;
  white-space: nowrap;
}

.fluor-stat-label {
  font-size: 15px;
  color: #64748b;
}

.fluor-stat-value {
  font-size: 15px;
  font-weight: 700;
  color: #1f2937;
}

.fluor-stat-status {
  font-size: 15px;
  color: #64748b;
  padding-top: 4px;
  border-top: 1px solid #eef2f6;
  line-height: 1.2;
}

")),
    
    # ==========================================================
    # MAIN AREA - Standards layout and fluorescence plate
    # ==========================================================
    
    fluidRow(
      
      # LEFT - Interactive standards layout
      column(
        width = 6,
        
        tags$h5(
          "Standards layout",
          style = "margin:0 0 3px 0;"
        ),
        
        tags$p(
          "Select a category, then click or drag to assign wells.",
          style = "margin:0 0 10px 0;color:#64748b;font-size:14px;"
        ),
        
        tags$div(
          class = "fluor-category-picker",
          
          radioButtons(
            inputId = ns("type"),
            label = NULL,
            choices = c(
              "ST1 initial",
              "ST2 initial",
              "ST1 final",
              "ST2 final",
              "Unassigned"
            ),
            selected = "ST1 initial",
            inline = TRUE
          )
        ),
        
        uiOutput(ns("plate_ui"))
      ),
      
      # RIGHT - Fluorescence plate
      column(
        width = 6,
        
        tags$h5(
          "Standards - Raw fluorescence",
          style = "margin:0 0 10px 0;"
        ),
        
        tags$div(
          style = "height:42px;"
        ),
        
        tags$div(
          style = "width:100%;max-width:560px;",
          
          plotly::plotlyOutput(
            "std_plate",
            height = "430px"
          )
        )
      )
    ),
    
    # ==========================================================
    # STANDARD STATISTICS - Below both plates
    # ==========================================================
    
    tags$hr(
      style = "margin:16px 0 14px 0;"
    ),
    
    tags$h5(
      "Standard statistics",
      style = "margin:0 0 12px 0;"
    ),
    
    tags$div(
      class = "fluor-statistics-panel",
      uiOutput(ns("std_summary"))
    ),
    
    
    # ==========================================================
    # BOTTOM AREA - Measurements and layout files
    # ==========================================================
    
    tags$hr(
      style = "margin:12px 0 16px 0;"
    ),
    
    fluidRow(
      
      # LEFT - Selected measurements
      column(
        width = 6,
        
        tags$h5(
          "Selected well measurements",
          style = "margin:0 0 12px 0;"
        ),
        
        textOutput(ns("assigned_count")),
        
        DT::DTOutput(ns("assigned_table"))
      ),
      
      # RIGHT - Layout files
      column(
        width = 6,
        
        tags$h5(
          "Layout files",
          style = "margin:0 0 12px 0;"
        ),
        
        fileInput(
          ns("layout_file"),
          "Import layout from Excel",
          accept = c(".xlsx", ".xls")
        ),
        
        tags$div(
          style = paste(
            "display:flex;",
            "flex-wrap:wrap;",
            "align-items:center;",
            "gap:10px;"
          ),
          
          downloadButton(
            ns("download_layout"),
            "Download layout"
          ),
          
          actionButton(
            ns("clear"),
            "Clear all assignments"
          )
        )
      )
    )
  )
}


# ============================================================
# AUTOMATIC STANDARD LAYOUT DETECTION
# ============================================================

detect_standard_layout <- function(measurements, use_correction = TRUE) {
  
  if (!all(c("Well", "RFU") %in% names(measurements))) {
    return(NULL)
  }
  
  candidate_columns <- integer(0)
  
  # Search all 12 columns, not only the first ones
  for (col in 1:12) {
    
    st1_wells <- paste0(LETTERS[1:4], col)
    st2_wells <- paste0(LETTERS[5:8], col)
    
    st1 <- measurements$RFU[
      match(st1_wells, measurements$Well)
    ]
    
    st2 <- measurements$RFU[
      match(st2_wells, measurements$Well)
    ]
    
    values <- c(st1, st2)
    
    # All eight wells must have valid measurements
    if (
      any(!is.finite(values)) ||
      any(values <= 0)
    ) {
      next
    }
    
    # ST2 must be substantially brighter than ST1
    contrast_ok <-
      median(st2) >= 5 * median(st1) &&
      min(st2) > 3 * max(st1)
    
    # ST2 replicates should be relatively consistent
    st2_cv <- stats::sd(st2) / mean(st2)
    replicates_ok <- st2_cv <= 0.35
    
    if (contrast_ok && replicates_ok) {
      candidate_columns <- c(candidate_columns, col)
    }
  }
  
  # Require a clear spatial pattern
  if (isTRUE(use_correction)) {
    
    # Exactly two recognizable columns
    if (length(candidate_columns) != 2L) {
      return(NULL)
    }
    
    selected_columns <- candidate_columns
    
  } else {
    
    # One column, or the leftmost of two existing
    # standard columns
    if (
      length(candidate_columns) == 0L ||
      length(candidate_columns) > 2L
    ) {
      return(NULL)
    }
    
    selected_columns <- candidate_columns[1]
  }
  
  detected_layout <- empty_std_layout()
  
  for (i in seq_along(selected_columns)) {
    
    col <- selected_columns[i]
    
    phase <- if (i == 1L) "initial" else "final"
    
    detected_layout$Type[
      detected_layout$Well %in%
        paste0(LETTERS[1:4], col)
    ] <- paste("ST1", phase)
    
    detected_layout$Type[
      detected_layout$Well %in%
        paste0(LETTERS[5:8], col)
    ] <- paste("ST2", phase)
  }
  
  detected_layout
}

standards_editor_server <- function(
    id,
    standard_data,
    correction_enabled = function() TRUE
) {
  
  moduleServer(id, function(input, output, session) {
    layout <- reactiveVal(empty_std_layout())
    
    # Track automatic versus manual layout assignments
    manual_override <- reactiveVal(FALSE)
    last_standard_signature <- reactiveVal(NULL)
    
    # Detect standards when the measurement source
    # or decay correction setting changes
    observeEvent(
      list(standard_data(), correction_enabled()),
      {
        
        dat <- standard_data()
        
        signature <- paste(
          paste(dat$Well, dat$RFU, sep = "="),
          collapse = "|"
        )
        
        previous <- last_standard_signature()
        data_changed <- !identical(signature, previous)
        
        if (data_changed) {
          last_standard_signature(signature)
          
          # A new measurement source can be detected again
          if (!is.null(previous)) {
            manual_override(FALSE)
          }
        }
        
        if (!isTRUE(manual_override())) {
          
          proposed <- detect_standard_layout(
            measurements = dat,
            use_correction = isTRUE(correction_enabled())
          )
          
          if (!is.null(proposed)) {
            
            layout(proposed)
            
          } else {
            
            layout(empty_std_layout())
            
            if (data_changed) {
              showNotification(
                paste(
                  "Standards could not be identified",
                  "unambiguously. Assign wells manually."
                ),
                type = "warning",
                duration = 8
              )
            }
          }
        }
      },
      ignoreInit = FALSE
    )
    
    
    # ============================================================
    # STANDARD CATEGORIES ACCORDING TO CORRECTION MODE
    # ============================================================
    
    observe({
      
      use_correction <- isTRUE(correction_enabled())
      
      choices <- if (use_correction) {
        
        c(
          "ST1 initial" = "ST1 initial",
          "ST2 initial" = "ST2 initial",
          "ST1 final" = "ST1 final",
          "ST2 final" = "ST2 final",
          "Unassigned" = "Unassigned"
        )
        
      } else {
        
        c(
          "ST1" = "ST1 initial",
          "ST2" = "ST2 initial",
          "Unassigned" = "Unassigned"
        )
      }
      
      current <- isolate(input$type)
      
      selected <- if (
        !is.null(current) &&
        current %in% unname(choices)
      ) {
        current
      } else {
        "ST1 initial"
      }
      
      updateRadioButtons(
        session,
        "type",
        choices = choices,
        selected = selected,
        inline = TRUE
      )
    })
    
    assign_wells <- function(wells, type) {
      if (is.null(type) || !type %in% names(std_colors)) return(invisible(NULL))
      current <- layout()
      wells <- intersect(unique(as.character(wells)), current$Well)
      if (!length(wells)) return(invisible(NULL))
      current$Type[current$Well %in% wells] <- type
      manual_override(TRUE)
      layout(current)
    }
    
    output$plate_ui <- renderUI({
      std_plate_grid(layout(), session$ns)
    })
    
    observeEvent(input$well_selection, {
      assign_wells(input$well_selection, input$type)
    })
    
    observeEvent(input$assign_group, {
      rows <- input$rows
      cols <- as.integer(input$columns)
      if (!length(rows) && !length(cols)) {
        showNotification("Select rows and/or columns.", type = "warning")
        return()
      }
      if (!length(rows)) rows <- LETTERS[1:8]
      if (!length(cols)) cols <- 1:12
      assign_wells(as.vector(outer(rows, cols, paste0)), input$type)
    })
    
    observeEvent(input$layout_file, {
      imported <- tryCatch(
        read_std_layout(input$layout_file$datapath),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = NULL)
          NULL
        }
      )
      if (!is.null(imported)) {
        manual_override(TRUE)
        layout(imported)
        showNotification("Standards layout imported.", type = "message")
      }
    })
    
    output$download_layout <- downloadHandler(
      filename = function() "FluoR_standards_layout.xlsx",
      content = function(file) write_std_layout(layout(), file)
    )
    
    observeEvent(input$clear, {
      manual_override(TRUE)
      layout(empty_std_layout())
    })
    
    
    # ============================================================
    # STANDARD STATISTICS
    # ============================================================
    
    output$std_summary <- renderUI({
      
      current_layout <- layout()
      
      measurements <- tryCatch(
        standard_data(),
        error = function(e) NULL
      )
      
      if (is.null(measurements)) {
        measurements <- data.frame(
          Well = character(),
          RFU = numeric()
        )
      }
      
      measurements <- measurements |>
        select(Well, RFU) |>
        distinct(Well, .keep_all = TRUE)
      
      dat <- current_layout |>
        left_join(measurements, by = "Well")
      
      format_number <- function(x, digits = 1) {
        if (!is.finite(x)) return("—")
        
        formatC(
          x,
          format = "f",
          digits = digits,
          big.mark = ","
        )
      }
      
      visible_categories <- if (isTRUE(correction_enabled())) {
        std_categories
      } else {
        c("ST1 initial", "ST2 initial")
      }
      
      cards <- lapply(visible_categories, function(category) {        
        subset <- dat |>
          filter(Type == category)
        
        values <- subset$RFU
        valid <- values[is.finite(values)]
        
        n_assigned <- nrow(subset)
        n_measured <- length(valid)
        n_missing <- n_assigned - n_measured
        n_negative <- sum(valid < 0)
        
        mean_rfu <- if (n_measured > 0) mean(valid) else NA_real_
        sd_rfu   <- if (n_measured > 1) sd(valid) else NA_real_
        
        cv <- if (
          is.finite(mean_rfu) &&
          is.finite(sd_rfu) &&
          abs(mean_rfu) > 1e-8
        ) {
          100 * sd_rfu / abs(mean_rfu)
        } else {
          NA_real_
        }
        
        category_color <- unname(std_colors[category])
        
        tags$div(
          class = "fluor-stat-card",
          style = paste0("border-left-color: ", category_color, ";"),
          
          tags$div(
            class = "fluor-stat-card-header",
            
            tags$strong(
              category,
              class = "fluor-stat-card-title"
            ),
            
            tags$span(
              paste(n_assigned, "wells"),
              class = "fluor-stat-card-count"
            )
          ),
          
          tags$div(
            class = "fluor-stat-metrics",
            
            tags$span(
              class = "fluor-stat-metric",
              tags$span("Mean RFU", class = "fluor-stat-label"),
              tags$span(format_number(mean_rfu), class = "fluor-stat-value")
            ),
            
            tags$span(
              class = "fluor-stat-metric",
              tags$span("SD", class = "fluor-stat-label"),
              tags$span(format_number(sd_rfu), class = "fluor-stat-value")
            ),
            
            tags$span(
              class = "fluor-stat-metric",
              tags$span("CV (%)", class = "fluor-stat-label"),
              tags$span(
                if (is.finite(cv)) paste0(format_number(cv), "%") else "—",
                class = "fluor-stat-value"
              )
            )
          ),
          
          tags$div(
            class = "fluor-stat-status",
            paste0(
              "Measured: ", n_measured,
              " · Missing: ", n_missing,
              " · Negative: ", n_negative
            )
          )
        )
      })
      
      do.call(tagList, cards)
    })
    
    
    assigned_data <- reactive({
      
      dat <- layout() |>
        dplyr::filter(Type != "Unassigned")
      
      measurements <- standard_data() |>
        dplyr::select(Well, RFU) |>
        dplyr::distinct(Well, .keep_all = TRUE)
      
      dat |>
        dplyr::left_join(
          measurements,
          by = "Well"
        ) |>
        dplyr::mutate(
          `log10 RFU` = ifelse(
            is.finite(RFU) & RFU > 0,
            log10(RFU),
            NA_real_
          )
        ) |>
        dplyr::select(
          Well,
          Type,
          RFU,
          `log10 RFU`
        ) |>
        dplyr::arrange(Type, Well)
    })
    
    output$assigned_count <- renderText({
      
      dat <- assigned_data()
      
      paste0(
        nrow(dat),
        " assigned wells · ",
        sum(is.finite(dat$RFU)),
        " with valid RFU measurements"
      )
    })
    
    output$assigned_table <- DT::renderDT({
      
      DT::datatable(
        assigned_data(),
        rownames = FALSE,
        options = list(
          paging = FALSE,
          scrollY = "340px",
          scrollCollapse = TRUE,
          scrollX = FALSE,
          searching = TRUE,
          ordering = TRUE,
          dom = "ft",
          autoWidth = FALSE
        )
      ) |>
        DT::formatRound(
          "log10 RFU",
          digits = 3
        )
    })
    
    return(layout)
  })
}
