
# ============================================================
# FluoR - Pipetting Order
# v0.1
# ============================================================

pipetting_order_ui <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    shiny::h4("Pipetting order"),
    
    shiny::radioButtons(
      ns("mode"),
      "Ordering method",
      choices = c(
        "By column (default)" = "column",
        "By well" = "well",
        "Manual" = "manual"
      ),
      selected = "column",
      inline = TRUE
    ),
    
    shiny::tags$div(
      style = "margin-bottom:12px;",
      
      shiny::radioButtons(
        ns("manual_unit"),
        "Manual selection unit",
        choices = c(
          "Column" = "column",
          "Well" = "well"
        ),
        selected = "column",
        inline = TRUE
      ),
      
      shiny::tags$div(
        style = "display:flex;gap:8px;flex-wrap:wrap;",
        
        shiny::actionButton(
          ns("undo"),
          "Undo last",
          icon = shiny::icon("rotate-left")
        ),
        
        shiny::actionButton(
          ns("reset"),
          "Reset order",
          icon = shiny::icon("arrows-rotate")
        )
      ),
      
      shiny::tags$p(
        "In Manual mode, click assigned wells in preparation order.",
        style = "font-size:13px;color:#64748b;margin-top:8px;"
      )
    ),
    
    shiny::tags$div(
      style = "margin:12px 0;",
      shiny::textOutput(ns("status"))
    ),
    
    shiny::tags$style(shiny::HTML("
      .fluor-order-grid {
        display: grid;
        grid-template-columns: 28px repeat(12, 42px);
        grid-template-rows: 24px repeat(8, 40px);
        width: max-content;
        align-items: center;
        justify-items: center;
        gap: 2px;
      }
      
      /* Add a separate row for column pipetting steps */
      .fluor-order-grid.with-column-order {
        grid-template-rows: 24px 34px repeat(8, 40px);
      }
      .fluor-order-scroll {
        overflow-x:auto;
        margin-bottom:15px;
      }
      .fluor-order-axis {
        font-size:12px;
        color:#64748b;
      }
      .fluor-order-well {
        width:35px;
        height:35px;
        border-radius:50%;
        border:1px solid #94a3b8;
        background:#e5e7eb;
        color:#1f2937;
        font-size:12px;
        display:flex;
        align-items:center;
        justify-content:center;
        padding:0;
      }
      .fluor-order-well.assigned {
        background:#bce5dc;
      }
      .fluor-order-well.pending {
        background:#fff0c2;
      }
      .fluor-order-well.clickable,
      .fluor-order-column.clickable {
        cursor:pointer;
      }
      .fluor-order-column {
        border:1px solid #cbd5e1;
        border-radius:5px;
        background:#f1f5f9;
        width:38px;
        height:30px;
        font-size:12px;
        padding:0;
      }
      .fluor-order-column.selected {
        background:#bce5dc;
        border-color:#4f9b85;
      }
      .fluor-order-group {
        border-bottom:1px solid #e2e8f0;
        padding:12px 0;
      }
      .fluor-order-group h5 {
        margin:0 0 10px;
      }
      .fluor-order-source {
        font-size: 13px;
        color: #64748b;
        margin-bottom: 12px;
        overflow-wrap: anywhere;
      }
    ")),
    
    shiny::uiOutput(ns("boards")),
    
    shiny::h5("Preparation sequence"),
    
    DT::DTOutput(ns("order_table"))
  )
}


pipetting_order_server <- function(
    id,
    standard_layout,
    sample_plate_ids,
    sample_layout_registry,
    plate_source_label = NULL
) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    manual_steps <- shiny::reactiveVal(character())
    
    # ----------------------------------------------------------
    # Collect assigned wells in experimental default order
    # ----------------------------------------------------------
    
    assigned_wells <- shiny::reactive({
      
      standard <- standard_layout()
      
      make_group <- function(dat, group, plate) {
        
        if (nrow(dat) == 0L) return(NULL)
        
        data.frame(
          Group = group,
          Plate = plate,
          Well = dat$Well,
          Row = dat$Row,
          Column = as.integer(dat$Column),
          stringsAsFactors = FALSE
        )
      }
      
      parts <- list()
      
      initial <- standard |>
        dplyr::filter(
          Type %in% c("ST1 initial", "ST2 initial")
        )
      
      parts[[length(parts) + 1L]] <- make_group(
        initial,
        "standards_initial",
        "Standards initial"
      )
      
      ids <- sample_plate_ids()
      
      for (k in seq_along(ids)) {
        
        plate_id <- ids[[k]]
        
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
          dplyr::filter(Type == "Sample")
        
        parts[[length(parts) + 1L]] <- make_group(
          assigned,
          plate_id,
          paste("Sample plate", k)
        )
      }
      
      final <- standard |>
        dplyr::filter(
          Type %in% c("ST1 final", "ST2 final")
        )
      
      parts[[length(parts) + 1L]] <- make_group(
        final,
        "standards_final",
        "Standards final"
      )
      
      parts <- Filter(Negate(is.null), parts)
      
      if (!length(parts)) {
        return(data.frame(
          Group = character(),
          Plate = character(),
          Well = character(),
          Row = character(),
          Column = integer()
        ))
      }
      
      dat <- do.call(rbind, parts)
      
      group_order <- unique(dat$Group)
      
      dat |>
        dplyr::mutate(
          GroupIndex = match(Group, group_order),
          RowIndex = match(Row, LETTERS[1:8])
        ) |>
        dplyr::arrange(GroupIndex, Column, RowIndex) |>
        dplyr::select(-GroupIndex, -RowIndex)
    })
    
    # ----------------------------------------------------------
    # Ordering unit: column or individual well
    # ----------------------------------------------------------
    
    active_unit <- shiny::reactive({
      
      if (identical(input$mode, "manual")) {
        if (identical(input$manual_unit, "well")) {
          return("well")
        }
        return("column")
      }
      
      if (identical(input$mode, "well")) {
        return("well")
      }
      
      "column"
    })
    
    order_data <- shiny::reactive({
      
      dat <- assigned_wells()
      
      dat$Unit <- if (identical(active_unit(), "column")) {
        paste(dat$Group, dat$Column, sep = "|")
      } else {
        paste(dat$Group, dat$Well, sep = "|")
      }
      
      if (identical(input$mode, "manual")) {
        
        sequence <- intersect(
          manual_steps(),
          unique(dat$Unit)
        )
        
      } else {
        
        sequence <- unique(dat$Unit)
      }
      
      dat$Order <- match(dat$Unit, sequence)
      
      dat
    })
    
    # ----------------------------------------------------------
    # Manual selection by successive clicks
    # ----------------------------------------------------------
    
    shiny::observeEvent(input$clicked, {
      
      if (!identical(input$mode, "manual")) {
        return()
      }
      
      key <- as.character(input$clicked)
      
      available <- unique(assigned_wells()$Group)
      
      if (identical(input$manual_unit, "column")) {
        
        dat <- assigned_wells()
        
        valid_keys <- unique(
          paste(dat$Group, dat$Column, sep = "|")
        )
        
      } else {
        
        dat <- assigned_wells()
        
        valid_keys <- unique(
          paste(dat$Group, dat$Well, sep = "|")
        )
      }
      
      if (!key %in% valid_keys) {
        return()
      }
      
      previous <- manual_steps()
      
      if (key %in% previous) {
        return()
      }
      
      manual_steps(c(previous, key))
    })
    
    shiny::observeEvent(input$undo, {
      
      previous <- manual_steps()
      
      if (length(previous)) {
        manual_steps(head(previous, -1L))
      }
    })
    
    shiny::observeEvent(input$reset, {
      manual_steps(character())
    })
    
    shiny::observeEvent(input$manual_unit, {
      manual_steps(character())
    }, ignoreInit = TRUE)
    
    # ----------------------------------------------------------
    # Progress and validation
    # ----------------------------------------------------------
    
    output$status <- shiny::renderText({
      
      dat <- order_data()
      
      n_units <- length(unique(dat$Unit))
      n_done <- length(unique(dat$Unit[!is.na(dat$Order)]))
      
      if (n_units == 0L) {
        return("No assigned wells. Configure plate layouts first.")
      }
      
      if (identical(input$mode, "manual")) {
        
        return(paste(
          "Ordered:", n_done, "/", n_units,
          "preparation steps.",
          if (n_done < n_units) "Click the remaining units." else "Complete."
        ))
      }
      
      paste(
        n_units,
        "preparation steps generated automatically."
      )
    })
    
    # ----------------------------------------------------------
    # Plate grids
    # One physical standards plate + sample plates
    # ----------------------------------------------------------
    
    output$boards <- shiny::renderUI({
      
      dat <- order_data()
      
      if (!nrow(dat)) return(NULL)
      
      manual <- identical(input$mode, "manual")
      by_column <- identical(active_unit(), "column")
      
      standard_groups <- c(
        "standards_initial",
        "standards_final"
      )
      
      # One board for all standards; one board per sample plate
      groups <- unique(dat$Group)
      
      board_keys <- c(
        if (any(groups %in% standard_groups)) "standards",
        groups[!groups %in% standard_groups]
      )
      
      shiny::tagList(lapply(board_keys, function(board_key) {
        
        is_standards <- identical(board_key, "standards")
        
        part <- if (is_standards) {
          dat[dat$Group %in% standard_groups, , drop = FALSE]
        } else {
          dat[dat$Group == board_key, , drop = FALSE]
        }
        
        plate_label <- if (is_standards) {
          "Standards"
        } else {
          part$Plate[[1]]
        }
        
        source_label <- if (is.null(plate_source_label)) {
          ""
        } else {
          plate_source_label(board_key)
        }
        
        # --------------------------------------------------------
        # Physical column numbers
        # --------------------------------------------------------
        
        cells <- list(
          shiny::tags$span(
            class = "fluor-order-axis",
            "Col"
          )
        )
        
        for (col in 1:12) {
          
          cells[[length(cells) + 1L]] <-
            shiny::tags$span(
              class = "fluor-order-axis",
              title = paste("Plate column", col),
              as.character(col)
            )
        }
        
        # --------------------------------------------------------
        # Column pipetting steps
        # One row, without displaying standard categories
        # --------------------------------------------------------
        
        if (by_column) {
          
          cells[[length(cells) + 1L]] <-
            shiny::tags$span(
              class = "fluor-order-axis",
              "Step"
            )
          
          for (col in 1:12) {
            
            column_wells <- part[
              part$Column == col,
              ,
              drop = FALSE
            ]
            
            # A physical column can contain several independent
            # preparation units. Preserve all of their keys.
            
            column_units <- unique(column_wells$Unit)
            
            column_orders <- unique(
              column_wells$Order[!is.na(column_wells$Order)]
            )
            
            column_orders <- sort(column_orders)
            
            # In manual mode, clicking a column orders its next
            # pending preparation unit.
            
            pending_units <- column_units[
              !column_units %in% manual_steps()
            ]
            
            clickable <- manual &&
              length(pending_units) > 0L
            
            next_key <- if (clickable) {
              pending_units[[1]]
            } else {
              NULL
            }
            
            order_label <- if (length(column_orders)) {
              paste(column_orders, collapse = "/")
            } else {
              "—"
            }
            
            cells[[length(cells) + 1L]] <-
              shiny::tags$button(
                type = "button",
                
                class = paste(
                  "fluor-order-column",
                  if (clickable) "clickable",
                  if (length(column_orders)) "selected"
                ),
                
                disabled = if (clickable) NULL else NA,
                
                title = paste0(
                  "Column ", col,
                  " · Pipetting step: ", order_label,
                  if (clickable) {
                    " · Click to order the next pending unit"
                  } else {
                    ""
                  }
                ),
                
                onclick = if (clickable) {
                  sprintf(
                    "Shiny.setInputValue('%s','%s',{priority:'event'});",
                    session$ns("clicked"),
                    next_key
                  )
                } else {
                  NULL
                },
                
                order_label
              )
          }
        }
        
        # --------------------------------------------------------
        # Wells
        # --------------------------------------------------------
        
        for (row in LETTERS[1:8]) {
          
          cells[[length(cells) + 1L]] <-
            shiny::tags$span(
              class = "fluor-order-axis",
              row
            )
          
          for (col in 1:12) {
            
            well <- paste0(row, col)
            
            entry <- part[
              part$Well == well,
              ,
              drop = FALSE
            ]
            
            assigned <- nrow(entry) > 0L
            
            position <- if (assigned) {
              entry$Order[[1]]
            } else {
              NA_integer_
            }
            
            well_group <- if (assigned) {
              entry$Group[[1]]
            } else {
              NA_character_
            }
            
            clickable <- manual && assigned
            
            well_key <- if (assigned) {
              
              if (by_column) {
                paste(well_group, col, sep = "|")
              } else {
                paste(well_group, well, sep = "|")
              }
              
            } else {
              ""
            }
            
            classes <- paste(
              "fluor-order-well",
              if (assigned && !is.na(position)) "assigned",
              if (assigned && is.na(position)) "pending",
              if (clickable) "clickable"
            )
            
            cells[[length(cells) + 1L]] <-
              shiny::tags$button(
                type = "button",
                class = classes,
                
                disabled = if (clickable) NULL else NA,
                
                title = if (assigned) {
                  paste(
                    "Well", well,
                    "·",
                    if (is.na(position)) {
                      "Not ordered"
                    } else {
                      paste("Pipetting step", position)
                    }
                  )
                } else {
                  paste("Well", well, "· Unassigned")
                },
                
                onclick = if (clickable) {
                  sprintf(
                    "Shiny.setInputValue('%s','%s',{priority:'event'});",
                    session$ns("clicked"),
                    well_key
                  )
                } else {
                  NULL
                },
                
                if (assigned && !is.na(position)) {
                  as.character(position)
                } else {
                  ""
                }
              )
          }
        }
        
        # --------------------------------------------------------
        # Grid dimensions
        # --------------------------------------------------------
        
        grid_rows <- paste(
          c(
            "24px",
            if (by_column) "34px",
            rep("40px", 8)
          ),
          collapse = " "
        )
        
        shiny::tags$div(
          class = "fluor-order-group",
          
          shiny::tags$h5(plate_label),
          
          shiny::tags$div(
            class = "fluor-order-source",
            source_label
          ),
          
          shiny::tags$div(
            class = "fluor-order-scroll",
            
            do.call(
              shiny::tags$div,
              c(
                list(
                  class = "fluor-order-grid",
                  style = paste0(
                    "grid-template-rows:",
                    grid_rows,
                    ";"
                  )
                ),
                cells
              )
            )
          )
        )
      }))
    })
    
    # ----------------------------------------------------------
    # Global order table
    # ----------------------------------------------------------
    
    output$order_table <- DT::renderDT({
      
      dat <- order_data() |>
        dplyr::select(
          Order,
          Plate,
          Well,
          Column
        ) |>
        dplyr::arrange(Order, Plate, Column, Well)
      
      DT::datatable(
        dat,
        rownames = FALSE,
        options = list(
          pageLength = 12,
          order = list(list(0, "asc")),
          scrollX = TRUE
        )
      )
    })
    
    return(order_data)
  })
}
