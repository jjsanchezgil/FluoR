
# ============================================================
# FluoR - DNA quantification
# v0.1
# ============================================================

calculate_dna <- function(
    standards,
    samples,
    assay,
    assayVol = 10,
    sampleVol = 5,
    correct_decay = TRUE
) {
  
  # ----------------------------------------------------------
  # INITIALIZE OUTPUT COLUMNS
  # ----------------------------------------------------------
  
  standards$stdConcentration <- NA_real_
  standards$cRFU <- NA_real_
  standards$RFU_corrected <- NA_real_
  
  samples$cRFU <- NA_real_
  samples$RFU_corrected <- NA_real_
  samples$cRFU_bg <- NA_real_
  samples$assayConcentration <- NA_real_
  samples$origConcentration <- NA_real_
  samples$DNA_ng_uL <- NA_real_
  samples$QC <- "Not calculated"
  
  # Consistent output, also when validation fails
  make_result <- function(
    message,
    fit = NULL,
    calibration = NULL,
    gamma = NA_real_,
    order_ref = NA_real_,
    blank = NA_real_,
    st2 = NA_real_,
    slope = NA_real_
  ) {
    
    list(
      standards = standards,
      samples = samples,
      fit = fit,
      calibration = calibration,
      gamma = gamma,
      order_ref = order_ref,
      blank = blank,
      st2 = st2,
      slope = slope,
      correction_applied = correct_decay,
      message = message
    )
  }
  
  # ----------------------------------------------------------
  # VALIDATE PARAMETERS
  # ----------------------------------------------------------
  
  if (length(assay) != 1L ||
      is.na(assay) ||
      !assay %in% c("HS", "BR")) {
    
    return(make_result("Select a valid Qubit assay (HS or BR)."))
  }
  
  if (length(assayVol) != 1L ||
      length(sampleVol) != 1L ||
      !is.finite(assayVol) ||
      !is.finite(sampleVol) ||
      assayVol <= 0 ||
      sampleVol <= 0) {
    
    return(make_result(
      "Standard and sample volumes must be positive."
    ))
  }
  
  
  # ----------------------------------------------------------
  # REQUIRED COLUMNS AND STANDARD CATEGORIES
  # ----------------------------------------------------------
  
  required_std_columns <- c("Well", "Type", "RFU")
  required_sample_columns <- c("Well", "RFU")
  
  if (isTRUE(correct_decay)) {
    required_std_columns <- c(required_std_columns, "Order")
    required_sample_columns <- c(required_sample_columns, "Order")
  }
  
  if (!all(required_std_columns %in% names(standards)) ||
      !all(required_sample_columns %in% names(samples))) {
    
    return(make_result(
      "Missing required measurement or pipetting-order columns."
    ))
  }
  
  required_types <- if (isTRUE(correct_decay)) {
    c(
      "ST1 initial",
      "ST2 initial",
      "ST1 final",
      "ST2 final"
    )
  } else {
    c(
      "ST1 initial",
      "ST2 initial"
    )
  }
  
  if (!all(required_types %in% standards$Type)) {
    
    return(make_result(
      if (isTRUE(correct_decay)) {
        "Assign ST1/ST2 initial and final standards."
      } else {
        "Assign ST1 and ST2 standards."
      }
    ))
  }
  
  # Use only the standards relevant to the selected mode.
  # Existing final assignments are preserved in the layout.
  
  standards <- standards[
    standards$Type %in% required_types,
    ,
    drop = FALSE
  ]
  
  if (any(!is.finite(standards$RFU)) ||
      any(standards$RFU <= 0)) {
    
    return(make_result(
      "All selected standards must have positive, finite RFU."
    ))
  }
  
  # Pipetting order is required only for decay correction.
  
  if (isTRUE(correct_decay)) {
    
    if (any(!is.finite(standards$Order))) {
      
      return(make_result(
        "Pipetting order is incomplete for the standards."
      ))
    }
    
    if (nrow(samples) > 0L &&
        any(!is.finite(samples$Order))) {
      
      return(make_result(
        "Pipetting order is incomplete for the samples."
      ))
    }
  }
  
  # ----------------------------------------------------------
  # STANDARD CONCENTRATIONS
  # ----------------------------------------------------------
  
  st2_concentration <- if (assay == "HS") 10 else 100
  
  standards$stdConcentration <- dplyr::case_when(
    grepl("^ST1", standards$Type) ~ 0,
    grepl("^ST2", standards$Type) ~ st2_concentration,
    TRUE ~ NA_real_
  )
  
  if (any(!is.finite(standards$stdConcentration))) {
    
    return(make_result(
      "Invalid standard categories."
    ))
  }
  
  # ============================================================
  # OPTIONAL MULTIPLICATIVE DECAY CORRECTION
  # MULTIPLICATIVE DECAY MODEL
  # log(RFU) = alpha + beta * stdConcentration
  #                     + gamma * Order
  # ============================================================
  
  fit_multiplicative <- NULL
  gamma <- NA_real_
  order_ref <- NA_real_
  
  if (isTRUE(correct_decay)) {
    
    # --------------------------------------------------------
    # MODE 1: MULTIPLICATIVE DECAY CORRECTION
    # --------------------------------------------------------
    
    order_ref <- 1
    
    fit_multiplicative <- tryCatch(
      stats::lm(
        log(RFU) ~ stdConcentration + Order,
        data = standards
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit_multiplicative)) {
      
      return(make_result(
        "Unable to fit the multiplicative decay model."
      ))
    }
    
    coefficients <- stats::coef(fit_multiplicative)
    
    if (any(!is.finite(coefficients)) ||
        stats::df.residual(fit_multiplicative) <= 0) {
      
      return(make_result(
        "Insufficient information to estimate the decay model."
      ))
    }
    
    gamma <- unname(coefficients["Order"])
    
    standards$cRFU <- standards$RFU *
      exp(-gamma * (standards$Order - order_ref))
    
    samples$cRFU <- samples$RFU *
      exp(-gamma * (samples$Order - order_ref))
    
  } else {
    
    # --------------------------------------------------------
    # MODE 2: NO DECAY CORRECTION
    # --------------------------------------------------------
    
    # Raw fluorescence is used directly for calibration.
    
    standards$cRFU <- standards$RFU
    samples$cRFU <- samples$RFU
  }
  
  # Keep existing output column names in both modes.
  
  standards$RFU_corrected <- standards$cRFU
  samples$RFU_corrected <- samples$cRFU
  
  if (any(!is.finite(standards$cRFU))) {
    
    return(make_result(
      "Non-finite fluorescence in standards.",
      fit = fit_multiplicative,
      gamma = gamma,
      order_ref = order_ref
    ))
  }
  
  
  # ----------------------------------------------------------
  # QUBIT CALIBRATION USING CORRECTED STANDARDS
  # ----------------------------------------------------------
  
  blank <- stats::median(
    standards$cRFU[
      standards$stdConcentration == 0
    ],
    na.rm = TRUE
  )
  
  st2 <- stats::median(
    standards$cRFU[
      standards$stdConcentration == st2_concentration
    ],
    na.rm = TRUE
  )
  
  slope <- (st2 - blank) / st2_concentration
  
  calibration <- data.frame(
    Standard = c("ST1", "ST2"),
    Concentration_ng_uL = c(0, st2_concentration),
    RFU_corrected = c(blank, st2)
  )
  
  if (!is.finite(slope) || slope <= 0) {
    
    return(make_result(
      "Invalid calibration: corrected ST2 must exceed ST1.",
      fit = fit_multiplicative,
      calibration = calibration,
      gamma = gamma,
      order_ref = order_ref,
      blank = blank,
      st2 = st2,
      slope = slope
    ))
  }
  
  # ----------------------------------------------------------
  # BACKGROUND SUBTRACTION
  # Same lower limit used in the original R script
  # ----------------------------------------------------------
  
  samples$cRFU_bg <- pmax(
    samples$cRFU - blank,
    1
  )
  
  # ----------------------------------------------------------
  # DNA CONCENTRATION
  #
  # assayConcentration = cRFU_bg / slope
  # origConcentration  = assayConcentration *
  #                      assayVol / sampleVol
  # ----------------------------------------------------------
  
  samples$assayConcentration <-
    samples$cRFU_bg / slope
  
  samples$origConcentration <-
    samples$assayConcentration *
    assayVol / sampleVol
  
  samples$DNA_ng_uL <- samples$origConcentration
  
  # ----------------------------------------------------------
  # QUALITY CONTROL
  # ----------------------------------------------------------
  
  samples$QC <- dplyr::case_when(
    !is.finite(samples$RFU) ~ "Missing RFU",
    samples$RFU <= 0 ~ "Non-positive RFU",
    !is.finite(samples$cRFU) ~ "Invalid corrected RFU",
    samples$cRFU <= blank ~ "At or below blank",
    samples$cRFU > st2 ~ "Above ST2",
    TRUE ~ "Within calibration interval"
  )
  
  # Preserve the original calculation while avoiding
  # apparently valid concentrations for invalid RFU.
  invalid <- !is.finite(samples$RFU) |
    samples$RFU <= 0 |
    !is.finite(samples$cRFU)
  
  samples$cRFU_bg[invalid] <- NA_real_
  samples$assayConcentration[invalid] <- NA_real_
  samples$origConcentration[invalid] <- NA_real_
  samples$DNA_ng_uL[invalid] <- NA_real_
  
  # ----------------------------------------------------------
  # RETURN RESULTS
  # ----------------------------------------------------------
  
  make_result(
    message = "Quantification calculated.",
    fit = fit_multiplicative,
    calibration = calibration,
    gamma = gamma,
    order_ref = order_ref,
    blank = blank,
    st2 = st2,
    slope = slope
  )
}
