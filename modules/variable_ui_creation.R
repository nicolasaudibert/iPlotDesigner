# Variable UI creation module
# Functions for creating variable properties UI with safe label access

# Safe label access function with multiple fallbacks
safeGetLabel <- function(key, fallback) {
  tryCatch({
    # Try cache first
    if(exists("getCachedLabel", envir = .GlobalEnv)) {
      result <- getCachedLabel(key, labels, fallback)
      if(!is.null(result) && !grepl("^Cascade prevented:", result)) {
        return(result)
      }
    }
    
    # Try emergency fallbacks
    if(exists("EMERGENCY_LABEL_FALLBACKS", envir = .GlobalEnv) && 
       key %in% names(EMERGENCY_LABEL_FALLBACKS)) {
      return(EMERGENCY_LABEL_FALLBACKS[[key]])
    }
    
    # Use provided fallback
    return(fallback)
  }, error = function(e) {
    return(fallback)
  })
}

# Create variable properties UI with safe label access
createVariablePropertiesUI <- function(varNum, varName, modalities, values, labels, session, validModalities = NULL) {
  if(is.null(varName) || varName == "") {
    return(NULL)
  }
  
  modalityList <- unique(modalities)
  modalityCounts <- table(modalities)
  
  # Use current order if available, otherwise use original order
  currentOrder <- values[[paste0("var", varNum, "_order")]]
  if(!is.null(currentOrder) && length(currentOrder) > 0) {
    modalityList <- currentOrder
  }
  
  # Create modality controls
  modalityControls <- lapply(seq_along(modalityList), function(i) {
    modality <- modalityList[i]
    count <- modalityCounts[modality]
    
    # Check if this modality is valid (has at least 2 data points)
    isValidModality <- is.null(validModalities) || modality %in% validModalities
    
    # Create stable ID based on modality name (not position)
    modalityId <- gsub("[^[:alnum:]]", "_", modality)
    
    # Get current color
    currentColors <- values[[paste0("var", varNum, "_colors")]]
    modalityColor <- if(!is.null(currentColors) && modality %in% names(currentColors)) {
      currentColors[modality]
    } else {
      defaultColors[[paste0("var", varNum)]][((i-1) %% length(defaultColors[[paste0("var", varNum)]])) + 1]
    }
    
    # Use safe label access
    insufficientDataNote <- safeGetLabel("insufficientDataNote", "Insufficient data")
    lessThanTwoObservations <- safeGetLabel("lessThanTwoObservations", "Less than 2 observations")
    moveModalityUpTitle <- safeGetLabel("moveModalityUpTitle", "Move this modality up")
    moveModalityDownTitle <- safeGetLabel("moveModalityDownTitle", "Move this modality down")
    
    # Conditional style for invalid modalities
    rowStyle <- paste0(
      "margin-bottom: 5px; padding: 5px; border-radius: 3px;",
      "background-color: ", if(isValidModality) "#f9f9f9" else "#f5f5f5", ";",
      if(!isValidModality) " opacity: 0.6; border-left: 3px solid #ffc107;" else ""
    )
    
    fluidRow(
      style = rowStyle,
      
      # Checkbox - uses stable ID
      column(2,
             checkboxInput(
               paste0("var", varNum, "_check_", modalityId), 
               NULL, 
               value = isValidModality  # Only checked if valid
             )
      ),
      
      # HTML5 Color picker - COLOURPICKER REPLACEMENT
      column(3,
             div(
               style = "padding-top: 2px;",
               tags$input(
                 type = "color",
                 id = paste0("var", varNum, "_color_", modalityId),
                 value = modalityColor,
                 style = paste0(
                   "width: 45px; ",
                   "height: 32px; ",
                   "padding: 0; ",
                   "border: 2px solid #ddd; ",
                   "border-radius: 4px; ",
                   "cursor: pointer; ",
                   "background: none; ",
                   "-webkit-appearance: none; ",
                   "-moz-appearance: none; ",
                   "appearance: none; ",
                   "transition: border-color 0.2s ease;"
                 ),
                 onchange = paste0("Shiny.setInputValue('", paste0("var", varNum, "_color_", modalityId), "', this.value);")
               )
             )
      ),
      
      # Label with status indication
      column(5,
             div(style = "padding-top: 5px;",
                 strong(
                   paste0(modality, " (n=", count, ")",
                          if(!isValidModality) paste(" -", insufficientDataNote) else "")
                 ),
                 if(!isValidModality) {
                   tags$small(
                     style = "color: #856404; display: block;",
                     lessThanTwoObservations
                   )
                 }
             )
      ),
      
      # Reordering buttons - uses stable ID with safe labels
      column(2,
             div(style = "text-align: right;",
                 actionButton(
                   paste0("var", varNum, "_up_", modalityId), 
                   "", 
                   icon = icon("arrow-up"), 
                   style = "padding: 2px 6px; margin: 0 1px;",
                   title = moveModalityUpTitle
                 ),
                 actionButton(
                   paste0("var", varNum, "_down_", modalityId), 
                   "", 
                   icon = icon("arrow-down"),
                   style = "padding: 2px 6px; margin: 0 1px;",
                   title = moveModalityDownTitle
                 )
             )
      )
    )
  })
  
  # Use safe label access for all UI text
  variableLabel <- safeGetLabel("variable", "Variable")
  modalitiesFilteredNotification <- safeGetLabel("modalitiesFilteredNotification", "{count} modalities excluded (insufficient data)")
  selectAllLabel <- safeGetLabel("selectAllLabel", "Select all")
  selectNoneLabel <- safeGetLabel("selectNoneLabel", "Select none")
  selectAllValidModalities <- safeGetLabel("selectAllValidModalities", "Select all valid modalities")
  deselectAllModalities <- safeGetLabel("deselectAllModalities", "Deselect all modalities")
  validModalitiesCount <- safeGetLabel("validModalitiesCount", "Valid modalities: {valid}/{total}")
  
  # Complete interface
  tagList(
    h5(paste0(variableLabel, " ", varNum, ": ", varName)),
    
    # Display summary if modalities are filtered
    if(!is.null(validModalities) && length(validModalities) < length(modalityList)) {
      div(
        style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 8px; margin-bottom: 10px; border-radius: 3px;",
        icon("info-circle", style = "color: #856404; margin-right: 5px;"),
        strong("Note: "),
        paste(length(modalityList) - length(validModalities), 
              gsub("\\{count\\}", length(modalityList) - length(validModalities), modalitiesFilteredNotification))
      )
    },
    
    # Modality container with scroll
    div(
      id = paste0("var", varNum, "_modalities_container"),
      style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: white;",
      modalityControls
    ),
    
    br(),
    
    # Global selection buttons with safe labels
    fluidRow(
      column(6, 
             actionButton(
               paste0("var", varNum, "_selectAll"), 
               selectAllLabel,
               style = "width: 100%; background-color: #28a745; color: white;",
               title = selectAllValidModalities
             )
      ),
      column(6, 
             actionButton(
               paste0("var", varNum, "_selectNone"), 
               selectNoneLabel,
               style = "width: 100%; background-color: #dc3545; color: white;",
               title = deselectAllModalities
             )
      )
    ),
    
    # Additional information with safe labels
    if(!is.null(validModalities)) {
      div(
        style = "margin-top: 10px; padding: 5px; background-color: #f8f9fa; border-radius: 3px; font-size: 0.9em;",
        gsub("\\{valid\\}", length(validModalities), 
             gsub("\\{total\\}", length(modalityList), validModalitiesCount))
      )
    }
  )
}

debugLog("VARIABLE_UI_CREATION: Variable UI creation functions loaded with safe label access", "APP_STARTUP")
