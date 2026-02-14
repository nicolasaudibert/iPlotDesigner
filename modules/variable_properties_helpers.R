# Variable properties helper functions module - simple optimization
# Utility functions for modality selection, color management, and UI creation
# Simple data.table optimization for table() replacement only

# Create variable properties UI
createVariablePropertiesUI <- function(varNum, varName, modalities, values, labels, session, validModalities = NULL) {
  if(is.null(varName) || varName == "") {
    return(NULL)
  }
  
  modalityList <- unique(modalities)
  
  # Adaptive optimization - use table_fast for optimal performance
  debugLog("OPTIMIZATION: Computing modality counts with table_fast", "PERFORMANCE_OPTIMIZATION")
  modalityCounts <- table_fast(modalities, alphabetical_order = TRUE, useNA = TRUE)
  
  # Use current order if available, otherwise use original order
  currentOrder <- values[[paste0("var", varNum, "_order")]]
  if(!is.null(currentOrder) && length(currentOrder) > 0) {
    modalityList <- currentOrder
  }
  
  # Get current selection state from values (not from defaults)
  currentSelected <- values[[paste0("var", varNum, "_selected")]]
  
  # Check if this variable is a copy
  is_copy <- !is.null(values[[paste0("var", varNum, "_is_copy")]]) && 
    values[[paste0("var", varNum, "_is_copy")]]
  source_var <- values[[paste0("var", varNum, "_source_var")]]
  
  # If this is a copy, display informative message instead of controls
  if (is_copy && !is.null(source_var)) {
    copy_label <- getCachedLabel("variableIsCopyLabel", labels, "Copy of Variable {source} (read-only)")
    copy_text <- gsub("\\{source\\}", source_var, copy_label)
    
    properties_sync_label <- getCachedLabel("variablePropertiesSynchronized", labels, 
                                          "Properties synchronized with Variable {source}")
    sync_text <- gsub("\\{source\\}", source_var, properties_sync_label)
    
    modify_instructions_label <- getCachedLabel("modifyPropertiesInSourceTab", labels, 
                                              "To modify properties, please use the {source_tab} tab.")
    variable_label <- getCachedLabel("variable", labels, "Variable")
    source_tab_text <- paste0(variable_label, " ", source_var)
    instructions_text <- gsub("\\{source_tab\\}", source_tab_text, modify_instructions_label)
    
    return(tagList(
      h5(paste0(variable_label, " ", varNum, ": ", varName)),
      
      div(
        style = "background-color: #e3f2fd; border-left: 4px solid #2196f3; padding: 16px; margin: 20px 0; border-radius: 4px;",
        div(
          style = "display: flex; align-items: center; margin-bottom: 12px;",
          icon("link", style = "color: #2196f3; font-size: 24px; margin-right: 12px;"),
          strong(style = "font-size: 16px; color: #1976d2;", copy_text)
        ),
        p(
          style = "margin: 8px 0 0 36px; color: #555;",
          sync_text
        ),
        p(
          style = "margin: 12px 0 0 36px; color: #666; font-size: 14px;",
          icon("info-circle", style = "margin-right: 6px;"),
          instructions_text
        )
      )
    ))
  }
  
  # Create modality controls
  modalityControls <- lapply(seq_along(modalityList), function(i) {
    modality <- modalityList[i]
    
    # Handle NA indexing - R's vector[NA] returns NA instead of the element named "NA"
    if(is.na(modality)) {
      count <- modalityCounts[names(modalityCounts) == "NA" | is.na(names(modalityCounts))][1]
    } else {
      count <- modalityCounts[as.character(modality)]
    }
    
    # Check if this modality is valid (has at least 2 data points)
    isValidModality <- is.null(validModalities) || modality %in% validModalities
    
    # Check if modality is currently selected (from values, not defaults)
    isCurrentlySelected <- if(!is.null(currentSelected)) {
      modality %in% currentSelected
    } else {
      isValidModality  # Fallback to validity if no selection stored
    }
    
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
    insufficientDataNote <- getCachedLabel("insufficientDataNote", labels, "Insufficient data")
    lessThanTwoObservations <- getCachedLabel("lessThanTwoObservations", labels, "Less than 2 observations")
    moveModalityUpTitle <- getCachedLabel("moveModalityUpTitle", labels, "Move this modality up")
    moveModalityDownTitle <- getCachedLabel("moveModalityDownTitle", labels, "Move this modality down")
    
    # Conditional style for invalid modalities
    rowStyle <- paste0(
      "margin-bottom: 5px; padding: 5px; border-radius: 3px;",
      "background-color: #f5f5f5"
    )
    
    # Create row elements conditionally to avoid layout issues
    if(varNum != 3) {
      # Standard layout for Variables 1 and 2 with color picker
      fluidRow(
        style = rowStyle,
        
        # Checkbox - uses stable ID and current selection state
        column(2,
               checkboxInput(
                 paste0("var", varNum, "_check_", modalityId), 
                 NULL, 
                 value = isCurrentlySelected  # Use actual selection state from values
               )
        ),
        
        # HTML5 Color picker
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
                     paste0(modality, " (n=", count, ")")
                   ),
                   if(!isValidModality) {
                     tags$small(
                       style = "color: #856404; display: block;",
                       lessThanTwoObservations
                     )
                   }
               )
        ),
        
        # Reordering buttons
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
    } else {
      # Simplified layout for Variable 3 - no color picker
      fluidRow(
        style = rowStyle,
        
        # Checkbox - uses stable ID and current selection state
        column(2,
               checkboxInput(
                 paste0("var", varNum, "_check_", modalityId), 
                 NULL, 
                 value = isCurrentlySelected  # Use actual selection state from values
               )
        ),
        
        # Label with expanded width - no color picker column
        column(8,
               div(style = "padding-top: 5px;",
                   strong(
                     paste0(modality, " (n=", count, ")")
                   ),
                   if(!isValidModality) {
                     tags$small(
                       style = "color: #856404; display: block;",
                       lessThanTwoObservations
                     )
                   }
               )
        ),
        
        # Reordering buttons
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
    }
    
  })
  
  # Use safe label access for all UI text
  variableLabel <- getCachedLabel("variable", labels, "Variable")
  modalitiesFilteredNotification <- getCachedLabel("modalitiesFilteredNotification", labels, "{count} modalities excluded (insufficient data)")
  selectAllLabel <- getCachedLabel("selectAllLabel", labels, "Select all")
  selectNoneLabel <- getCachedLabel("selectNoneLabel", labels, "Select none")
  selectAllValidModalities <- getCachedLabel("selectAllValidModalities", labels, "Select all valid modalities")
  deselectAllModalities <- getCachedLabel("deselectAllModalities", labels, "Deselect all modalities")
  validModalitiesCount <- getCachedLabel("validModalitiesCount", labels, "Valid modalities: {valid}/{total}")
  
  # Complete interface
  tagList(
    h5(paste0(variableLabel, " ", varNum, ": ", varName)),
    
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

debugLog("VARIABLE_PROPERTIES_HELPERS: Helper functions loaded with simple data.table optimization", "VAR_INIT")
