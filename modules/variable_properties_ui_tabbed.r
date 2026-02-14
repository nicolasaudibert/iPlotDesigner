# Variable properties UI generation module - tabbed version
# Generate UI elements only when tab is active (lazy loading)

# Variable 1 properties - lazy loading version
output$var1Properties_tabbed <- renderUI({
  # Check if this tab is active
  if(is.null(input$variablePropertiesTabset) || input$variablePropertiesTabset != "var1_tab") {
    debugLog("var1Properties_tabbed - Tab not active, skipping render", "UI_RENDER")
    return(NULL)
  }
  
  # Direct reactive dependency on input
  current_var1 <- input$independentVar1
  
  debugLog("var1Properties_tabbed - Starting lazy reactive render", "VAR_PROPERTIES")
  
  # Early exit if no input
  if(is.null(current_var1) || current_var1 == "") {
    noVariableMessage <- safeGetUILabel("noVariableSelectedMessage", labels, "No variable selected")
    return(div(
      style = "text-align: center; padding: 40px; color: #666;",
      h4(noVariableMessage),
      p("Select a variable from the dropdown above to configure its properties.")
    ))
  }
  
  debugLog(paste("var1Properties_tabbed - Processing input:", current_var1), "UI_RENDER")
  
  # Variable validation with error protection
  validation <- tryCatch({
    validateGroupingVariable(values$filteredData, current_var1)
  }, error = function(e) {
    debugLog(paste("var1Properties_tabbed - Validation error:", e$message), "UI_ERROR")
    list(isValid = FALSE, reason = paste("Validation error:", e$message))
  })
  
  if(!validation$isValid) {
    debugLog("var1Properties_tabbed - Validation failed, returning error div", "VAR_PROPERTIES")
    
    # Use localization system
    localizedMessage <- if(validation$reason == "Numeric variables cannot be used for grouping") {
      getCachedLabel("numericVariableError", labels, "Numeric variables cannot be used for grouping. Please select a categorical variable.")
    } else {
      getCachedLabel("insufficientDataError", labels, "This variable has no categories with sufficient data for analysis.")
    }
    
    return(div(
      style = "color: red; padding: 20px; border: 1px solid red; border-radius: 5px; margin: 10px;",
      h4("Variable 1: ", current_var1),
      p(strong(localizedMessage))
    ))
  }
  
  # Property initialization with error protection (SAME LOGIC)
  tryCatch({
    if(is.null(values$var1_modalities) || 
       !identical(names(table(values$var1_modalities)), names(table(values$filteredData[[current_var1]])))) {
      
      debugLog("var1Properties_tabbed - Initializing properties", "VAR_PROPERTIES")
      
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
      
      debugLog("var1Properties_tabbed - Properties initialized", "VAR_PROPERTIES")
    }
  }, error = function(e) {
    debugLog(paste("var1Properties_tabbed - Property initialization error:", e$message), "UI_ERROR")
  })
  
  # UI creation with error protection (SAME LOGIC)
  tryCatch({
    debugLog("var1Properties_tabbed - Creating UI", "VAR_PROPERTIES")
    result <- createVariablePropertiesUI(1, current_var1, values$var1_modalities, 
                                         values, labels, session, 
                                         validModalities = validation$validModalities)
    debugLog("var1Properties_tabbed - UI created successfully", "VAR_PROPERTIES")
    return(result)
  }, error = function(e) {
    debugLog(paste("var1Properties_tabbed - UI creation error:", e$message), "UI_ERROR")
    return(div(
      style = "color: red; padding: 20px; border: 1px solid red; border-radius: 5px; margin: 10px;",
      h4("Variable 1: ", current_var1),
      p(strong("UI ERROR:"), e$message)
    ))
  })
})

# Variable 2 properties - lazy loading version
output$var2Properties_tabbed <- renderUI({
  # Check if this tab is active
  if(is.null(input$variablePropertiesTabset) || input$variablePropertiesTabset != "var2_tab") {
    debugLog("var2Properties_tabbed - Tab not active, skipping render", "UI_RENDER")
    return(NULL)
  }
  
  # Direct reactive dependency on input
  current_var2 <- input$independentVar2
  
  debugLog("var2Properties_tabbed - Starting lazy reactive render", "VAR_PROPERTIES")
  
  # Early exit if no input
  if(is.null(current_var2) || current_var2 == "") {
    noVariableMessage <- safeGetUILabel("noVariableSelectedMessage", labels, "No variable selected")
    return(div(
      style = "text-align: center; padding: 40px; color: #666;",
      h4(noVariableMessage),
      p("Select a variable from the dropdown above to configure its properties.")
    ))
  }
  
  debugLog(paste("var2Properties_tabbed - Processing input:", current_var2), "UI_RENDER")
  
  # Use cached label to avoid cascades
  noVar2Label <- getCachedLabel("noIndependentVar2Label", labels, "-- No independent variable 2 --")
  
  if(current_var2 == noVar2Label) {
    noVariableMessage <- safeGetUILabel("noVariableSelectedMessage", labels, "No variable selected")
    return(div(
      style = "text-align: center; padding: 40px; color: #666;",
      h4(noVariableMessage),
      p("Select a variable from the dropdown above to configure its properties.")
    ))
  }
  
  # Same validation and UI logic as original var2Properties
  validation <- tryCatch({
    validateGroupingVariable(values$filteredData, current_var2)
  }, error = function(e) {
    debugLog(paste("var2Properties_tabbed - Validation error:", e$message), "UI_ERROR")
    list(isValid = FALSE, reason = paste("Validation error:", e$message))
  })
  
  if(!validation$isValid) {
    debugLog("var2Properties_tabbed - Validation failed, returning error div", "VAR_PROPERTIES")
    
    localizedMessage <- if(validation$reason == "Numeric variables cannot be used for grouping") {
      getCachedLabel("numericVariableError", labels, "Numeric variables cannot be used for grouping. Please select a categorical variable.")
    } else {
      getCachedLabel("insufficientDataError", labels, "This variable has no categories with sufficient data for analysis.")
    }
    
    return(div(
      style = "color: red; padding: 20px; border: 1px solid red; border-radius: 5px; margin: 10px;",
      h4("Variable 2: ", current_var2),
      p(strong(localizedMessage))
    ))
  }
  
  # Property initialization (same logic)
  tryCatch({
    if(is.null(values$var2_modalities) || 
       !identical(names(table(values$var2_modalities)), names(table(values$filteredData[[current_var2]])))) {
      
      debugLog("var2Properties_tabbed - Initializing properties", "VAR_PROPERTIES")
      
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
      
      debugLog("var2Properties_tabbed - Properties initialized", "VAR_PROPERTIES")
    }
  }, error = function(e) {
    debugLog(paste("var2Properties_tabbed - Property initialization error:", e$message), "UI_ERROR")
  })
  
  # UI creation (same logic)
  tryCatch({
    debugLog("var2Properties_tabbed - Creating UI", "VAR_PROPERTIES")
    result <- createVariablePropertiesUI(2, current_var2, values$var2_modalities, 
                                         values, labels, session, 
                                         validModalities = validation$validModalities)
    debugLog("var2Properties_tabbed - UI created successfully", "VAR_PROPERTIES")
    return(result)
  }, error = function(e) {
    debugLog(paste("var2Properties_tabbed - UI creation error:", e$message), "UI_ERROR")
    return(div(
      style = "color: red; padding: 20px; border: 1px solid red; border-radius: 5px; margin: 10px;",
      h4("Variable 2: ", current_var2),
      p(strong("UI ERROR:"), e$message)
    ))
  })
})

# Variable 3 properties - lazy loading version
output$var3Properties_tabbed <- renderUI({
  # Check if this tab is active
  if(is.null(input$variablePropertiesTabset) || input$variablePropertiesTabset != "var3_tab") {
    debugLog("var3Properties_tabbed - Tab not active, skipping render", "UI_RENDER")
    return(NULL)
  }
  
  # Direct reactive dependency on input
  current_var3 <- input$independentVar3
  
  debugLog("var3Properties_tabbed - Starting lazy reactive render", "VAR_PROPERTIES")
  
  # Early exit if no input
  if(is.null(current_var3) || current_var3 == "") {
    noVariableMessage <- safeGetUILabel("noVariableSelectedMessage", labels, "No variable selected")
    return(div(
      style = "text-align: center; padding: 40px; color: #666;",
      h4(noVariableMessage),
      p("Select a variable from the dropdown above to configure its properties.")
    ))
  }
  
  debugLog(paste("var3Properties_tabbed - Processing input:", current_var3), "UI_RENDER")
  
  # Use cached label to avoid cascades
  noVar3Label <- getCachedLabel("noIndependentVar3Label", labels, "-- No independent variable 3 --")
  
  if(current_var3 == noVar3Label) {
    noVariableMessage <- safeGetUILabel("noVariableSelectedMessage", labels, "No variable selected")
    return(div(
      style = "text-align: center; padding: 40px; color: #666;",
      h4(noVariableMessage),
      p("Select a variable from the dropdown above to configure its properties.")
    ))
  }
  
  # Same validation and UI logic as original var3Properties
  validation <- tryCatch({
    validateGroupingVariable(values$filteredData, current_var3)
  }, error = function(e) {
    debugLog(paste("var3Properties_tabbed - Validation error:", e$message), "UI_ERROR")
    list(isValid = FALSE, reason = paste("Validation error:", e$message))
  })
  
  if(!validation$isValid) {
    debugLog("var3Properties_tabbed - Validation failed, returning error div", "VAR_PROPERTIES")
    
    localizedMessage <- if(validation$reason == "Numeric variables cannot be used for grouping") {
      getCachedLabel("numericVariableError", labels, "Numeric variables cannot be used for grouping. Please select a categorical variable.")
    } else {
      getCachedLabel("insufficientDataError", labels, "This variable has no categories with sufficient data for analysis.")
    }
    
    return(div(
      style = "color: red; padding: 20px; border: 1px solid red; border-radius: 5px; margin: 10px;",
      h4("Variable 3: ", current_var3),
      p(strong(localizedMessage))
    ))
  }
  
  # Property initialization (same logic)
  tryCatch({
    if(is.null(values$var3_modalities) || 
       !identical(names(table(values$var3_modalities)), names(table(values$filteredData[[current_var3]])))) {
      
      debugLog("var3Properties_tabbed - Initializing properties", "VAR_PROPERTIES")
      
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
      
      debugLog("var3Properties_tabbed - Properties initialized", "VAR_PROPERTIES")
    }
  }, error = function(e) {
    debugLog(paste("var3Properties_tabbed - Property initialization error:", e$message), "UI_ERROR")
  })
  
  # UI creation (same logic)
  tryCatch({
    debugLog("var3Properties_tabbed - Creating UI", "VAR_PROPERTIES")
    result <- createVariablePropertiesUI(3, current_var3, values$var3_modalities, 
                                         values, labels, session, 
                                         validModalities = validation$validModalities)
    debugLog("var3Properties_tabbed - UI created successfully", "VAR_PROPERTIES")
    return(result)
  }, error = function(e) {
    debugLog(paste("var3Properties_tabbed - UI creation error:", e$message), "UI_ERROR")
    return(div(
      style = "color: red; padding: 20px; border: 1px solid red; border-radius: 5px; margin: 10px;",
      h4("Variable 3: ", current_var3),
      p(strong("UI ERROR:"), e$message)
    ))
  })
})

debugLog("VARIABLE_PROPERTIES_UI_TABBED: Lazy loading tabbed UI system loaded", "VAR_INIT")
