# Variable properties UI generation functions - tabbed version
# Functions only - outputs are registered in server context

# Function to create variable 1 properties - lazy loading
createVar1PropertiesTabbed <- function(input, output, values, labels, session) {
  output$var1Properties_tabbed <- renderUI({
    # Check if this tab is active - critical for performance
    if(is.null(input$variableConfigurationTabset) || input$variableConfigurationTabset != "var1_tab") {
      debugLog("var1Properties_tabbed - Tab not active, skipping render", "UI_RENDER")
      return(NULL)
    }
    
    # Direct reactive dependency on input
    current_var1 <- input$independentVar1
    
    debugLog("var1Properties_tabbed - Starting lazy reactive render", "VAR_PROPERTIES")
    
    # Early exit if no input
    if(is.null(current_var1) || current_var1 == "") {
      noVariableMessage <- safeGetUILabel("noVariableSelectedMessage", labels, "No variable selected")
      selectVariableInstructions <- safeGetUILabel("selectVariableInstructions", labels, "Select a variable from the dropdown in the Variable Selection tab to configure its properties.")
      return(div(
        style = "text-align: center; padding: 40px; color: #666;",
        h4(noVariableMessage),
        p(selectVariableInstructions)
      ))
    }
    
    debugLog(paste("var1Properties_tabbed - Processing input:", current_var1), "VAR_PROPERTIES")
    
    # Variable validation with error protection
    validation <- tryCatch({
      validateGroupingVariable(values$filteredData, current_var1)
    }, error = function(e) {
      debugLog(paste("var1Properties_tabbed - Validation error:", e$message), "VAR_DEBUG")
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
    
    # Property initialization with error protection
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
      debugLog(paste("var1Properties_tabbed - Property initialization error:", e$message), "VAR_DEBUG")
    })
    
    # UI creation with error protection
    tryCatch({
      debugLog("var1Properties_tabbed - Creating UI", "VAR_PROPERTIES")
      result <- createVariablePropertiesUI(1, current_var1, values$var1_modalities, 
                                           values, labels, session, 
                                           validModalities = validation$validModalities)
      debugLog("var1Properties_tabbed - UI created successfully", "VAR_PROPERTIES")
      return(result)
    }, error = function(e) {
      debugLog(paste("var1Properties_tabbed - UI creation error:", e$message), "VAR_DEBUG")
      return(div(
        style = "color: red; padding: 20px; border: 1px solid red; border-radius: 5px; margin: 10px;",
        h4("Variable 1: ", current_var1),
        p(strong("UI ERROR:"), e$message)
      ))
    })
  })
}

# Function to create variable 2 properties - lazy loading
createVar2PropertiesTabbed <- function(input, output, values, labels, session) {
  output$var2Properties_tabbed <- renderUI({
    # Check if this tab is active - CRITICAL FOR PERFORMANCE
    if(is.null(input$variableConfigurationTabset) || input$variableConfigurationTabset != "var2_tab") {
      debugLog("var2Properties_tabbed - Tab not active, skipping render", "UI_RENDER")
      return(NULL)
    }
    
    # Direct reactive dependency on input
    current_var2 <- input$independentVar2
    
    debugLog("var2Properties_tabbed - Starting lazy reactive render", "VAR_PROPERTIES")
    
    # Early exit if no input
    if(is.null(current_var2) || current_var2 == "") {
      noVariableMessage <- safeGetUILabel("noVariableSelectedMessage", labels, "No variable selected")
      selectVariableInstructions <- safeGetUILabel("selectVariableInstructions", labels, "Select a variable from the dropdown in the Variable Selection tab to configure its properties.")
      return(div(
        style = "text-align: center; padding: 40px; color: #666;",
        h4(noVariableMessage),
        p(selectVariableInstructions)
      ))
    }
    
    debugLog(paste("var2Properties_tabbed - Processing input:", current_var2), "VAR_PROPERTIES")
    
    # Use cached label to avoid cascades
    noVar2Label <- getCachedLabel("noIndependentVar2Label", labels, "-- No independent variable 2 --")
    
    if(current_var2 == noVar2Label) {
      noVariableMessage <- safeGetUILabel("noVariableSelectedMessage", labels, "No variable selected")
      selectVariableInstructions <- safeGetUILabel("selectVariableInstructions", labels, "Select a variable from the dropdown in the Variable Selection tab to configure its properties.")
      return(div(
        style = "text-align: center; padding: 40px; color: #666;",
        h4(noVariableMessage),
        p(selectVariableInstructions)
      ))
    }
    
    # Same validation logic as var1
    validation <- tryCatch({
      validateGroupingVariable(values$filteredData, current_var2)
    }, error = function(e) {
      debugLog(paste("var2Properties_tabbed - Validation error:", e$message), "VAR_DEBUG")
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
    
    # Property initialization (same logic as var1)
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
      debugLog(paste("var2Properties_tabbed - Property initialization error:", e$message), "VAR_DEBUG")
    })
    
    # UI creation (same logic as var1)
    tryCatch({
      debugLog("var2Properties_tabbed - Creating UI", "VAR_PROPERTIES")
      result <- createVariablePropertiesUI(2, current_var2, values$var2_modalities, 
                                           values, labels, session, 
                                           validModalities = validation$validModalities)
      debugLog("var2Properties_tabbed - UI created successfully", "VAR_PROPERTIES")
      return(result)
    }, error = function(e) {
      debugLog(paste("var2Properties_tabbed - UI creation error:", e$message), "VAR_DEBUG")
      return(div(
        style = "color: red; padding: 20px; border: 1px solid red; border-radius: 5px; margin: 10px;",
        h4("Variable 2: ", current_var2),
        p(strong("UI ERROR:"), e$message)
      ))
    })
  })
}

# Function to create variable 3 properties - lazy loading
createVar3PropertiesTabbed <- function(input, output, values, labels, session) {
  output$var3Properties_tabbed <- renderUI({
    # Check if this tab is active - CRITICAL FOR PERFORMANCE
    if(is.null(input$variableConfigurationTabset) || input$variableConfigurationTabset != "var3_tab") {
      debugLog("var3Properties_tabbed - Tab not active, skipping render", "UI_RENDER")
      return(NULL)
    }
    
    # Direct reactive dependency on input
    current_var3 <- input$independentVar3
    
    debugLog("var3Properties_tabbed - Starting lazy reactive render", "VAR_PROPERTIES")
    
    # Early exit if no input
    if(is.null(current_var3) || current_var3 == "") {
      noVariableMessage <- safeGetUILabel("noVariableSelectedMessage", labels, "No variable selected")
      selectVariableInstructions <- safeGetUILabel("selectVariableInstructions", labels, "Select a variable from the dropdown in the Variable Selection tab to configure its properties.")
      return(div(
        style = "text-align: center; padding: 40px; color: #666;",
        h4(noVariableMessage),
        p(selectVariableInstructions)
      ))
    }
    
    debugLog(paste("var3Properties_tabbed - Processing input:", current_var3), "VAR_PROPERTIES")
    
    # Use cached label to avoid cascades
    noVar3Label <- getCachedLabel("noIndependentVar3Label", labels, "-- No independent variable 3 --")
    
    if(current_var3 == noVar3Label) {
      noVariableMessage <- safeGetUILabel("noVariableSelectedMessage", labels, "No variable selected")
      selectVariableInstructions <- safeGetUILabel("selectVariableInstructions", labels, "Select a variable from the dropdown in the Variable Selection tab to configure its properties.")
      return(div(
        style = "text-align: center; padding: 40px; color: #666;",
        h4(noVariableMessage),
        p(selectVariableInstructions)
      ))
    }
    
    # Same validation logic as var1/var2
    validation <- tryCatch({
      validateGroupingVariable(values$filteredData, current_var3)
    }, error = function(e) {
      debugLog(paste("var3Properties_tabbed - Validation error:", e$message), "VAR_DEBUG")
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
    
    # Property initialization (same logic as var1/var2)
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
      debugLog(paste("var3Properties_tabbed - Property initialization error:", e$message), "VAR_DEBUG")
    })
    
    # UI creation (same logic as var1/var2)
    tryCatch({
      debugLog("var3Properties_tabbed - Creating UI", "VAR_PROPERTIES")
      result <- createVariablePropertiesUI(3, current_var3, values$var3_modalities, 
                                           values, labels, session, 
                                           validModalities = validation$validModalities)
      debugLog("var3Properties_tabbed - UI created successfully", "VAR_PROPERTIES")
      return(result)
    }, error = function(e) {
      debugLog(paste("var3Properties_tabbed - UI creation error:", e$message), "VAR_DEBUG")
      return(div(
        style = "color: red; padding: 20px; border: 1px solid red; border-radius: 5px; margin: 10px;",
        h4("Variable 3: ", current_var3),
        p(strong("UI ERROR:"), e$message)
      ))
    })
  })
}

debugLog("VARIABLE_PROPERTIES_UI_FUNCTIONS: Tabbed UI functions defined", "VAR_PROPERTIES")
