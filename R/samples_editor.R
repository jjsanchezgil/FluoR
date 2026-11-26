# ============================================================
# FluoR - Samples Layout Editor
# v0.1
# ============================================================

sample_empty_layout <- function() {
  d <- expand.grid(Row = LETTERS[1:8], Column = 1:12,
                   stringsAsFactors = FALSE)
  d$Well <- paste0(d$Row, d$Column)
  d$Type <- "Unassigned"
  d
}

sample_read_layout <- function(path) {
  raw <- readxl::read_excel(path, sheet = 1, col_names = FALSE,
                            col_types = "text", .name_repair = "minimal")
  mat <- as.matrix(raw)
  mat[is.na(mat)] <- ""
  mat <- trimws(mat)
  values <- NULL

  if (nrow(mat) >= 8 && ncol(mat) >= 13) {
    for (col in seq_len(min(4L, ncol(mat) - 12L))) {
      for (r in seq_len(nrow(mat) - 7L)) {
        if (identical(toupper(mat[r:(r + 7L), col]), LETTERS[1:8])) {
          values <- mat[r:(r + 7L), (col + 1L):(col + 12L), drop = FALSE]
          break
        }
      }
      if (!is.null(values)) break
    }
  }
  if (is.null(values)) {
    if (nrow(mat) < 8 || ncol(mat) < 12) {
      stop("Expected an 8 x 12 plate layout (or rows A-H and columns 1-12).")
    }
    start <- if (identical(as.character(mat[1, 1:12]), as.character(1:12))) 2L else 1L
    if (nrow(mat) < start + 7L) stop("Incomplete plate layout.")
    values <- mat[start:(start + 7L), 1:12, drop = FALSE]
  }

  codes <- toupper(trimws(values))
  empty <- codes %in% c("", "NA", "NONE", "EMPTY", "UNASSIGNED", "0", "-")
  valid <- empty | codes %in% c("SAMPLE", "S", "1")
  if (any(!valid)) {
    stop("Unknown sample labels: ", paste(unique(codes[!valid]), collapse = ", "),
         ". Use Sample or leave cells blank.")
  }
  d <- sample_empty_layout()
  for (r in 1:8) for (col in 1:12) {
    d$Type[d$Well == paste0(LETTERS[r], col)] <-
      if (empty[r, col]) "Unassigned" else "Sample"
  }
  d
}

sample_write_layout <- function(layout, path) {
  mat <- matrix("", nrow = 8, ncol = 12)
  for (i in seq_len(nrow(layout))) {
    if (identical(layout$Type[i], "Sample")) {
      mat[match(layout$Row[i], LETTERS[1:8]), layout$Column[i]] <- "Sample"
    }
  }
  export <- data.frame(Row = LETTERS[1:8], mat, check.names = FALSE)
  names(export) <- c("Row", as.character(1:12))
  writexl::write_xlsx(list("Samples layout" = export), path)
}

sample_plate_grid <- function(layout, ns) {
  cells <- list(shiny::tags$span(class = "fluor-sample-axis"))
  for (col in 1:12) {
    cells[[length(cells) + 1L]] <- shiny::tags$span(
      class = "fluor-sample-axis", as.character(col))
  }
  for (r in 1:8) {
    cells[[length(cells) + 1L]] <- shiny::tags$span(
      class = "fluor-sample-axis", LETTERS[r])
    for (col in 1:12) {
      w <- paste0(LETTERS[r], col)
      assigned <- layout$Type[match(w, layout$Well)] == "Sample"
      cells[[length(cells) + 1L]] <- shiny::tags$div(
        class = if (assigned) "fluor-sample-well assigned" else "fluor-sample-well",
        `data-well` = w,
        `data-row` = r,
        `data-col` = col,
        title = paste(w, if (assigned) "Sample" else "Unassigned"),
        w
      )
    }
  }
  shiny::tags$div(
    class = "fluor-sample-scroll",
    do.call(shiny::tags$div,
            c(list(id = ns("grid"), class = "fluor-sample-grid"), cells))
  )
}

samples_editor_ui <- function(id, plot_id = NULL) {
  ns <- shiny::NS(id)
  js <- sprintf("(function() {
    const gridId = '%s', inputId = '%s';
    window.__fluorSampleGridHandlers = window.__fluorSampleGridHandlers || {};
    if (window.__fluorSampleGridHandlers[gridId]) return;
    window.__fluorSampleGridHandlers[gridId] = true;
    let drag = false, start = null, finish = null;
    const root = () => document.getElementById(gridId);
    function hit(e) {
      const board = root(); if (!board) return null;
      const item = document.elementFromPoint(e.clientX, e.clientY);
      const well = item && item.closest('[data-well]');
      return well && board.contains(well) ? well : null;
    }
    const pos = el => ({r: +el.dataset.row, c: +el.dataset.col});
    function chosen(board) {
      if (!start || !finish) return [];
      const r0 = Math.min(start.r, finish.r), r1 = Math.max(start.r, finish.r);
      const c0 = Math.min(start.c, finish.c), c1 = Math.max(start.c, finish.c);
      return Array.from(board.querySelectorAll('[data-well]')).filter(el => {
        const r = +el.dataset.row, c = +el.dataset.col;
        return r >= r0 && r <= r1 && c >= c0 && c <= c1;
      });
    }
    function clear(board) {
      if (board) board.querySelectorAll('.fluor-sample-preview').forEach(el =>
        el.classList.remove('fluor-sample-preview'));
    }
    function preview() {
      const board = root(); if (!board) return;
      clear(board);
      chosen(board).forEach(el => el.classList.add('fluor-sample-preview'));
    }
    document.addEventListener('pointerdown', e => {
      if (e.button !== 0 || drag) return;
      const well = hit(e); if (!well) return;
      drag = true; start = pos(well); finish = start;
      e.preventDefault(); preview();
    });
    document.addEventListener('pointermove', e => {
      if (!drag) return;
      const well = hit(e); if (!well) return;
      finish = pos(well); preview();
    });
    document.addEventListener('pointerup', e => {
      if (!drag) return;
      const board = root(), well = hit(e);
      if (well) finish = pos(well);
      const wells = board ? chosen(board).map(el => el.dataset.well) : [];
      clear(board);
      drag = false; start = null; finish = null;
      if (wells.length && window.Shiny) {
        Shiny.setInputValue(inputId, wells, {priority: 'event'});
      }
    });
    function cancel() { clear(root()); drag = false; start = null; finish = null; }
    document.addEventListener('pointercancel', cancel);
    window.addEventListener('blur', cancel);
  })();", ns("grid"), ns("selection"))

  css <- "
  .fluor-sample-scroll {
    overflow-x: auto;
    padding: 2px 0;
    margin: 0;
    min-width: 0;
  }
  
  .fluor-sample-grid {
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
  
  .fluor-sample-axis {
    font-size: 12px;
    color: #5b6472;
  }
  
  .fluor-sample-well {
    width: min(36px, calc(100% - 4px));
    height: auto;
    aspect-ratio: 1;
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
    background: #e5e7eb;
    border: 1px solid #64748b;
    color: #1f2937;
    font-size: 11px;
    box-sizing: border-box;
    cursor: crosshair;
  }
  .fluor-sample-well.assigned { background:#4dd0a8; }
  .fluor-sample-well.fluor-sample-preview {
    outline:3px solid #2563eb; outline-offset:2px;
  }
  .fluor-sample-type .shiny-input-container {
    margin-bottom: 0 !important;
    width: auto !important;
    flex: 0 0 auto;
  }
  .fluor-sample-type .shiny-options-group {
    display: flex;
    gap: 6px;
    flex-wrap: nowrap;
  }
  .fluor-sample-type .radio-inline {
    position:relative; margin:0!important; padding:6px 12px!important;
    border:2px solid #cbd5e1; border-radius:8px; cursor:pointer;
    background:white; font-size:13px;
  }
  .fluor-sample-type input[type=radio] { position:absolute; opacity:0; width:1px; height:1px; }
  .fluor-sample-type .radio-inline:has(input:checked) {
    border-color:#2563eb; background:#eff6ff;
  }
  .fluor-sample-type .radio-inline:nth-child(1) { border-left:7px solid #4dd0a8; }
  .fluor-sample-type .radio-inline:nth-child(2) { border-left:7px solid #e5e7eb; }
  "

  
  shiny::tagList(
    
    shiny::tags$style(shiny::HTML(css)),
    shiny::tags$script(shiny::HTML(js)),
    
    # ==========================================================
    # MAIN AREA - Layout and fluorescence plate
    # ==========================================================
    
    shiny::fluidRow(
      
      # LEFT - Interactive samples layout
      shiny::column(
        width = 6,
        
        shiny::tags$h5(
          "Samples layout",
          style = "margin:0 0 3px 0;"
        ),
        
        shiny::tags$p(
          "Select a category, then click or drag to assign wells.",
          style = "margin:0 0 10px 0;color:#64748b;font-size:14px;"
        ),
        
        shiny::tags$div(
          class = "fluor-sample-type",
          style = paste(
            "display:flex;",
            "align-items:center;",
            "gap:8px;",
            "flex-wrap:nowrap;",
            "margin-bottom:4px;"
          ),
          
          shiny::radioButtons(
            ns("type"),
            NULL,
            choices = c("Sample", "Unassigned"),
            selected = "Sample",
            inline = TRUE
          ),
          
          shiny::tags$span(
            "|",
            style = "color:#94a3b8;margin:0 2px;"
          ),
          
          shiny::actionButton(
            ns("assign_measured"),
            "Assign all",
            class = "btn-sm btn-outline-primary",
            style = "white-space:nowrap;"
          ),
          
          shiny::actionButton(
            ns("apply_all"),
            "Apply to all",
            class = "btn-sm btn-outline-primary",
            style = "white-space:nowrap;"
          )
        ),
        
        shiny::uiOutput(ns("plate_ui")),
      ),
      
      # RIGHT - Raw fluorescence
      shiny::column(
        width = 6,
        
        shiny::tags$h5(
          "Samples - Raw fluorescence",
          style = "margin:0 0 10px 0;"
        ),
        
        shiny::tags$div(
          style = "height:42px;"
        ),
        
        if (!is.null(plot_id)) {
          shiny::tags$div(
            style = "width:100%;max-width:560px;",
            
            plotly::plotlyOutput(
              plot_id,
              height = "430px"
            )
          )
        }
      )
    ),
    
    # ==========================================================
    # SAMPLE STATISTICS
    # ==========================================================
    
    shiny::tags$hr(
      style = "margin:16px 0 14px 0;"
    ),
    
    shiny::tags$h5(
      "Sample statistics",
      style = "margin:0 0 12px 0;"
    ),
    
    shiny::uiOutput(ns("summary")),
    
    
    # ==========================================================
    # BOTTOM AREA - Measurements and layout files
    # ==========================================================
    
    shiny::tags$hr(
      style = "margin:16px 0;"
    ),
    
    shiny::fluidRow(
      
      # LEFT - Selected measurements
      shiny::column(
        width = 6,
        
        shiny::tags$h5(
          "Selected well measurements",
          style = "margin:0 0 12px 0;"
        ),
        
        shiny::textOutput(ns("assigned_count")),
        
        DT::DTOutput(ns("assigned_table"))
      ),
      
      # RIGHT - Layout files
      shiny::column(
        width = 6,
        
        shiny::tags$h5(
          "Layout files",
          style = "margin:0 0 12px 0;"
        ),
        
        shiny::fileInput(
          ns("layout_file"),
          "Import layout from Excel",
          accept = c(".xlsx", ".xls")
        ),
        
        shiny::tags$div(
          style = paste(
            "display:flex;",
            "flex-wrap:wrap;",
            "align-items:center;",
            "gap:10px;"
          ),
          
          shiny::downloadButton(
            ns("download_layout"),
            "Download layout"
          ),
          
          shiny::actionButton(
            ns("clear"),
            "Clear all assignments"
          )
        )
      )
    )
  )
}

samples_editor_server <- function(id, sample_data, on_apply = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    layout <- shiny::reactiveVal(sample_empty_layout())

    # Apply the current layout to other plates when requested.
    if (!is.null(on_apply)) {
      shiny::observeEvent(input$apply_all, {
        on_apply(layout(), id)
      })
    }

    assign_wells <- function(wells, category) {
      if (is.null(category) || !category %in% c("Sample", "Unassigned"))
        return(invisible(NULL))
      current <- layout()
      wells <- intersect(unique(as.character(wells)), current$Well)
      if (!length(wells)) return(invisible(NULL))
      current$Type[current$Well %in% wells] <- category
      layout(current)
    }

    output$plate_ui <- shiny::renderUI({
      sample_plate_grid(layout(), session$ns)
    })
    shiny::observeEvent(input$selection, {
      assign_wells(input$selection, input$type)
    })
    shiny::observeEvent(input$assign_group, {
      rows <- input$rows
      cols <- as.integer(input$columns)
      if (!length(rows) && !length(cols)) {
        shiny::showNotification("Select rows and/or columns.", type = "warning")
        return()
      }
      if (!length(rows)) rows <- LETTERS[1:8]
      if (!length(cols)) cols <- 1:12
      assign_wells(as.vector(outer(rows, cols, paste0)), input$type)
    })
    shiny::observeEvent(input$assign_measured, {
      dat <- sample_data()
      wells <- dat$Well[is.finite(dat$RFU)]
      assign_wells(wells, "Sample")
    })
    shiny::observeEvent(input$layout_file, {
      imported <- tryCatch(sample_read_layout(input$layout_file$datapath),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
          NULL
        })
      if (!is.null(imported)) layout(imported)
    })
    output$download_layout <- shiny::downloadHandler(
      filename = function() "FluoR_samples_layout.xlsx",
      content = function(file) sample_write_layout(layout(), file)
    )
    shiny::observeEvent(input$clear, { layout(sample_empty_layout()) })

    assigned <- shiny::reactive({
      dplyr::left_join(
        dplyr::filter(layout(), Type == "Sample"),
        dplyr::distinct(dplyr::select(sample_data(), Well, RFU), Well, .keep_all = TRUE),
        by = "Well"
      )
    })

    output$summary <- shiny::renderUI({
      current <- layout()
      n_assigned <- sum(current$Type == "Sample")
      dat <- tryCatch(assigned(), error = function(e) NULL)
      x <- if (is.null(dat)) numeric() else dat$RFU[is.finite(dat$RFU)]
      n <- length(x)
      avg <- if (n) mean(x) else NA_real_
      s <- if (n > 1) stats::sd(x) else NA_real_
      cv <- if (is.finite(avg) && is.finite(s) && abs(avg) > 1e-8)
        100 * s / abs(avg) else NA_real_
      fmt <- function(z) if (is.finite(z)) formatC(z, digits = 1, format = "f", big.mark = ",") else "—"
      shiny::tags$div(
        style = "border:1px solid #dce3eb;border-left:5px solid #4dd0a8;border-radius:8px;padding:12px;",
        shiny::tags$strong(paste(n_assigned, "assigned wells")),
        shiny::tags$p(paste("Measured:", n, "· Missing:", n_assigned-n,
                            "· Negative:", sum(x < 0)), style = "margin:8px 0;"),
        shiny::tags$p(paste("Mean RFU:", fmt(avg), "· SD:", fmt(s),
                            "· CV:", if (is.finite(cv)) paste0(fmt(cv), "%") else "—"),
                      style = "margin:0;")
      )
    })
    
    # ============================================================
    # ASSIGNED SAMPLE COUNT
    # ============================================================
    
    output$assigned_count <- shiny::renderText({
      
      dat <- assigned()
      
      paste0(
        nrow(dat),
        " assigned wells · ",
        sum(is.finite(dat$RFU)),
        " with valid RFU measurements"
      )
    })
    
    # ============================================================
    # SELECTED SAMPLE MEASUREMENTS
    # ============================================================
    
    output$assigned_table <- DT::renderDT({
      
      dat <- assigned() |>
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
        )
      
      DT::datatable(
        dat,
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
