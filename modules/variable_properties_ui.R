# Variable properties UI generation module
# Generate UI elements for variable properties with validation and localization
# Prevents double initialization

# Helper function to check if properties are already initialized
arePropertiesInitialized <- function(varNum, varName, values) {
  # Check if all required properties exist and match current variable
  modalities_exist <- !is.null(values[[paste0("var", varNum, "_modalities")]])
  selected_exist <- !is.null(values[[paste0("var", varNum, "_selected")]])
  colors_exist <- !is.null(values[[paste0("var", varNum, "_colors")]])
  
  # Check if they match the current variable data
  if(modalities_exist && !is.null(values$filteredData) && varName %in% names(values$filteredData)) {
    current_data_signature <- paste(sort(unique(values$filteredData[[varName]])), collapse = "|")
    existing_data_signature <- paste(sort(unique(values[[paste0("var", varNum, "_modalities")]])), collapse = "|")
    data_matches <- current_data_signature == existing_data_signature
  } else {
    data_matches <- FALSE
  }
  
  result <- modalities_exist && selected_exist && colors_exist && data_matches
  
  if(result) {
    debugLog(paste("ISSUE1_FIX: Properties already initialized for var", varNum, varName), "APP_DEBUG")
  }
  
  return(result)
}

# Main UI generation for variable properties
output$var1Properties <- renderUI({
  # Direct reactive dependency on input
  current_var1 <- input$independentVar1
  
  debugLog("var1Properties - Starting natural reactive render", "VAR_DEBUG")
  
  # Early exit if no input
  if(is.null(current_var1) || current_var1 == "") {
    debugLog("var1Properties - No input, returning NULL", "VAR_DEBUG")
    return(NULL)
  }
  
  debugLog(paste("var1Properties - Processing input:", current_var1), "VAR_DEBUG")
  
  # Variable validation with error protection
  validation <- tryCatch({
    validateGroupingVariable(values$filteredData, current_var1)
  }, error = function(e) {
    debugLog(paste("var1Properties - Validation error:", e$message), "VAR_DEBUG")
    list(isValid = FALSE, reason = paste("Validation error:", e$message))
  })
  
  if(!validation$isValid) {
    debugLog("var1Properties - Validation failed, returning error div", "VAR_DEBUG")
    
    # Use localization system
    localizedMessage <- if(validation$reason == "Numeric variables cannot be used for grouping") {
      getCachedLabel("numericVariableError", labels, "Numeric variables cannot be used for grouping. Please select a categorical variable.")
    } else {
      getCachedLabel("insufficientDataError", labels, "This variable has no categories with sufficient data for analysis.")
    }
    
    return(div(
      style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
      h5("Variable 1: ", current_var1),
      p(strong(localizedMessage))
    ))
  }
  
  # Only initialize if not already done by performVariableValidation()
  tryCatch({
    if(!arePropertiesInitialized(1, current_var1, values)) {
      debugLog("var1Properties - Properties not initialized, initializing now", "VAR_DEBUG")
      
      values$var1_modalities <- values$filteredData[[current_var1]]
      values$var1_selected <- validation$validModalities
      values$var1_order <- validation$validModalities
      
      if(length(validation$validModalities) > 0) {
        colors <- defaultColors$var1[1:length(validation$validModalities)]
        if(length(validation$validModalities) > length(defaultColors$var1)) {
          colors <- rep(defaultColors$var1, ceiling(length(validation$validModalities) / length(defaultColors$var1)))[1:length(validation$validModalities)]
        }
        values$var1_colors <- setNames(colors, validation$validModalities)
      } else {
        values$var1_colors <- NULL
      }
      
      debugLog("var1Properties - Properties initialized by UI", "VAR_DEBUG")
    } else {
      debugLog("var1Properties - Properties already initialized by validation, skipping", "VAR_DEBUG")
    }
  }, error = function(e) {
    debugLog(paste("var1Properties - Property initialization error:", e$message), "VAR_DEBUG")
  })
  
  # UI creation with error protection
  tryCatch({
    debugLog("var1Properties - Creating UI", "VAR_DEBUG")
    result <- createVariablePropertiesUI(1, current_var1, values$var1_modalities, 
                                         values, labels, session, 
                                         validModalities = validation$validModalities)
    debugLog("var1Properties - UI created successfully", "VAR_DEBUG")
    return(result)
  }, error = function(e) {
    debugLog(paste("var1Properties - UI creation error:", e$message), "VAR_DEBUG")
    return(div(
      style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
      h5("Variable 1: ", current_var1),
      p(strong("UI ERROR:"), e$message)
    ))
  })
})

# Variable 2 properties UI
output$var2Properties <- renderUI({
  # Direct reactive dependency on input
  current_var2 <- input$independentVar2
  
  debugLog("var2Properties - Starting natural reactive render", "VAR_DEBUG")
  
  # Early exit if no input
  if(is.null(current_var2) || current_var2 == "") {
    debugLog("var2Properties - No input, returning NULL", "VAR_DEBUG")
    return(NULL)
  }
  
  debugLog(paste("var2Properties - Processing input:", current_var2), "VAR_DEBUG")
  
  # Use cached label to avoid cascades
  noVar2Label <- getCachedLabel("noIndependentVar2Label", labels, "-- No independent variable 2 --")
  
  if(current_var2 == noVar2Label) {
    debugLog("var2Properties - Deselection option detected, returning NULL", "VAR_DEBUG")
    return(NULL)
  }
  
  # Variable validation with error protection
  validation <- tryCatch({
    validateGroupingVariable(values$filteredData, current_var2)
  }, error = function(e) {
    debugLog(paste("var2Properties - Validation error:", e$message), "VAR_DEBUG")
    list(isValid = FALSE, reason = paste("Validation error:", e$message))
  })
  
  if(!validation$isValid) {
    debugLog("var2Properties - Validation failed, returning error div", "VAR_DEBUG")
    
    # Use localization system
    localizedMessage <- if(validation$reason == "Numeric variables cannot be used for grouping") {
      getCachedLabel("numericVariableError", labels, "Numeric variables cannot be used for grouping. Please select a categorical variable.")
    } else {
      getCachedLabel("insufficientDataError", labels, "This variable has no categories with sufficient data for analysis.")
    }
    
    return(div(
      style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
      h5("Variable 2: ", current_var2),
      p(strong(localizedMessage))
    ))
  }
  
  # ISSUE 1 FIX: Only initialize if not already done by performVariableValidation()
  tryCatch({
    if(!arePropertiesInitialized(2, current_var2, values)) {
      debugLog("var2Properties - Properties not initialized, initializing now", "VAR_DEBUG")
      
      values$var2_modalities <- values$filteredData[[current_var2]]
      values$var2_selected <- validation$validModalities
      values$var2_order <- validation$validModalities
      
      if(length(validation$validModalities) > 0) {
        colors <- defaultColors$var2[1:length(validation$validModalities)]
        if(length(validation$validModalities) > length(defaultColors$var2)) {
          colors <- rep(defaultColors$var2, ceiling(length(validation$validModalities) / length(defaultColors$var2)))[1:length(validation$validModalities)]
        }
        values$var2_colors <- setNames(colors, validation$validModalities)
      } else {
        values$var2_colors <- NULL
      }
      
      debugLog("var2Properties - Properties initialized by UI", "VAR_DEBUG")
    } else {
      debugLog("var2Properties - Properties already initialized by validation, skipping", "VAR_DEBUG")
    }
  }, error = function(e) {
    debugLog(paste("var2Properties - Property initialization error:", e$message), "VAR_DEBUG")
  })
  
  # UI creation with error protection
  tryCatch({
    debugLog("var2Properties - Creating UI", "VAR_DEBUG")
    result <- createVariablePropertiesUI(2, current_var2, values$var2_modalities, 
                                         values, labels, session, 
                                         validModalities = validation$validModalities)
    debugLog("var2Properties - UI created successfully", "VAR_DEBUG")
    return(result)
  }, error = function(e) {
    debugLog(paste("var2Properties - UI creation error:", e$message), "VAR_DEBUG")
    return(div(
      style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
      h5("Variable 2: ", current_var2),
      p(strong("UI ERROR:"), e$message)
    ))
  })
})

# Variable 3 properties UI
output$var3Properties <- renderUI({
  # Direct reactive dependency on input (same as var1/var2)
  current_var3 <- input$independentVar3
  
  debugLog("var3Properties - Starting natural reactive render", "VAR_DEBUG")
  
  # Early exit if no input
  if(is.null(current_var3) || current_var3 == "") {
    debugLog("var3Properties - No input, returning NULL", "VAR_DEBUG")
    return(NULL)
  }
  
  debugLog(paste("var3Properties - Processing input:", current_var3), "VAR_DEBUG")
  
  # Use cached label to avoid cascades
  noVar3Label <- getCachedLabel("noIndependentVar3Label", labels, "-- No independent variable 3 --")
  
  if(current_var3 == noVar3Label) {
    debugLog("var3Properties - Deselection option detected, returning NULL", "VAR_DEBUG")
    return(NULL)
  }
  
  # Same validation logic as var1/var2
  validation <- tryCatch({
    validateGroupingVariable(values$filteredData, current_var3)
  }, error = function(e) {
    debugLog(paste("var3Properties - Validation error:", e$message), "VAR_DEBUG")
    list(isValid = FALSE, reason = paste("Validation error:", e$message))
  })
  
  if(!validation$isValid) {
    debugLog("var3Properties - Validation failed, returning error div", "VAR_DEBUG")
    
    # Use localization system
    localizedMessage <- if(validation$reason == "Numeric variables cannot be used for grouping") {
      getCachedLabel("numericVariableError", labels, "Numeric variables cannot be used for grouping. Please select a categorical variable.")
    } else {
      getCachedLabel("insufficientDataError", labels, "This variable has no categories with sufficient data for analysis.")
    }
    
    return(div(
      style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
      h5("Variable 3: ", current_var3),
      p(strong(localizedMessage))
    ))
  }
  
  # ISSUE 1 FIX: Only initialize if not already done by performVariableValidation()
  tryCatch({
    if(!arePropertiesInitialized(3, current_var3, values)) {
      debugLog("var3Properties - Properties not initialized, initializing now", "VAR_DEBUG")
      
      values$var3_modalities <- values$filteredData[[current_var3]]
      values$var3_selected <- validation$validModalities
      values$var3_order <- validation$validModalities
      
      if(length(validation$validModalities) > 0) {
        colors <- defaultColors$var3[1:length(validation$validModalities)]
        if(length(validation$validModalities) > length(defaultColors$var3)) {
          colors <- rep(defaultColors$var3, ceiling(length(validation$validModalities) / length(defaultColors$var3)))[1:length(validation$validModalities)]
        }
        values$var3_colors <- setNames(colors, validation$validModalities)
      } else {
        values$var3_colors <- NULL
      }
      
      debugLog("var3Properties - Properties initialized by UI", "VAR_DEBUG")
    } else {
      debugLog("var3Properties - Properties already initialized by validation, skipping", "VAR_DEBUG")
    }
  }, error = function(e) {
    debugLog(paste("var3Properties - Property initialization error:", e$message), "VAR_DEBUG")
  })
  
  # Same UI creation logic as var1/var2
  tryCatch({
    debugLog("var3Properties - Creating UI", "VAR_DEBUG")
    result <- createVariablePropertiesUI(3, current_var3, values$var3_modalities, 
                                         values, labels, session, 
                                         validModalities = validation$validModalities)
    debugLog("var3Properties - UI created successfully", "VAR_DEBUG")
    return(result)
  }, error = function(e) {
    debugLog(paste("var3Properties - UI creation error:", e$message), "VAR_DEBUG")
    return(div(
      style = "color: red; padding: 10px; border: 1px solid red; border-radius: 5px;",
      h5("Variable 3: ", current_var3),
      p(strong("UI ERROR:"), e$message)
    ))
  })
})

debugLog("VARIABLE_PROPERTIES_UI: Variable properties UI generation loaded with ISSUE1 fix", "VAR_INIT")
