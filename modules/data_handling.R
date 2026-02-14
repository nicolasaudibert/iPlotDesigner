# data_handling.R
# Fix for 'object values not found' error
# Main server coordination with proper values initialization + tabbed properties

server_main <- function(labels) {
  function(input, output, session) {
    
    # Initialize values first before any module loading
    values <- reactiveValues()
    
    # Initialize essential values to prevent 'not found' errors
    values$filedata <- NULL
    values$originalData <- NULL  # Preserve original data
    values$filteredData <- NULL
    values$var1_modalities <- NULL
    values$var2_modalities <- NULL
    values$var3_modalities <- NULL
    values$var1_selected <- NULL
    values$var2_selected <- NULL
    values$var3_selected <- NULL
    values$var1_colors <- NULL
    values$var2_colors <- NULL
    values$var3_colors <- NULL
    values$var1_order <- NULL
    values$var2_order <- NULL
    values$var3_order <- NULL
    # Variable reuse tracking structures - source/copy relationships
    values$var1_is_copy <- FALSE
    values$var2_is_copy <- FALSE
    values$var3_is_copy <- FALSE
    values$var1_source_var <- NULL  # NULL or 1/2/3 indicating source variable
    values$var2_source_var <- NULL
    values$var3_source_var <- NULL
    values$var_reuse_map <- list()  # Map: variable_name -> source_var_num
    
    debugLog("DATA_HANDLING: Variable reuse structures initialized", "USER_OPERATION")
    
    # Initialize enhanced validation context variables
    values$user_choice_enabled <- FALSE  # Initialize early to prevent reactive errors
    values$current_user_choice_variable <- NULL
    values$current_user_choice_var_num <- NULL
    values$current_user_choice_validation <- NULL
    values$current_user_choice_complexity <- NULL
    
    # Initialize user choice filtering state
    values$user_applied_filtering <- list()  # Store user applied filters by variable name
    
    debugLog("DATA_HANDLING: values initialized successfully with enhanced validation context, data preservation, and user operations", "APP_STARTUP")
    
    # Load core modules after values initialization
    tryCatch({
      source("modules/file_handling.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: file_handling.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading file_handling.R:", e$message), "MODULE_ERROR")
    })
    
    # Load variable observers after values initialization
    tryCatch({
      source("modules/variable_observers.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: variable_observers.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading variable_observers.R:", e$message), "MODULE_ERROR")
    })
    
    # Initialize user operations history after loading variable_observers.R (which loads operation_tracking.R)
    tryCatch({
      initializeUserOperationsHistory(values)
      debugLog("DATA_HANDLING: User operations history initialized", "USER_OPERATION")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error initializing user operations:", e$message), "USER_OPERATION")
      # Non-blocking error - application continues without operations history
    })
    
    # Load button observers with modular architecture
    tryCatch({
      source("modules/button_observers.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: button_observers.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading button_observers.R:", e$message), "MODULE_ERROR")
    })
    
    # Load validation outputs
    tryCatch({
      source("modules/validation_outputs.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: validation_outputs.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading validation_outputs.R:", e$message), "MODULE_ERROR")
    })
    
    # Load UI outputs
    tryCatch({
      source("modules/ui_outputs.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: ui_outputs.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading ui_outputs.R:", e$message), "MODULE_ERROR")
    })
    
    # Load theme server modules
    tryCatch({
      source("modules/theme_renderui.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: theme_renderui.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading theme_renderui.R:", e$message), "MODULE_ERROR")
    })
    
    tryCatch({
      source("modules/theme_observers.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: theme_observers.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading theme_observers.R:", e$message), "MODULE_ERROR")
    })
    
    # Load plot rendering
    tryCatch({
      source("modules/plot_rendering.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: plot_rendering.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading plot_rendering.R:", e$message), "MODULE_ERROR")
    })
    
    # Load code generation handlers
    tryCatch({
      source("modules/code_generation_handlers.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: code_generation_handlers.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading code_generation_handlers.R:", e$message), "MODULE_ERROR")
    })
    
    # Load sample size table module
    tryCatch({
      source("modules/sample_size_table.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: sample_size_table.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading sample_size_table.R:", e$message), "MODULE_ERROR")
    })
    
    # Load font management observers
    tryCatch({
      source("modules/font_management_observers.R", local = TRUE, encoding = "UTF-8")

      debugLog("DATA_HANDLING: font_management_observers.R loaded", "MODULE_LOAD")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading font_management_observers.R:", e$message), "MODULE_ERROR")
    })
    
    # Observer to enable/disable Var3 display radio buttons based on Var3 selection
    observe({
      req(input$independentVar3)
      
      # Check if Var3 is actually selected (not null, empty, or noVar3Label)
      noVar3Label <- safeGetUILabel("noVar3Label", labels, "None")
      var3_has_content <- !is.null(input$independentVar3) && 
                         input$independentVar3 != "" && 
                         input$independentVar3 != noVar3Label
      
      # Enable/disable radio buttons and slider
      if (var3_has_content) {
        shinyjs::enable("sampleSizeVar3Display")
        # Enable slider only if facets mode is selected
        if (!is.null(input$sampleSizeVar3Display) && input$sampleSizeVar3Display == "facets") {
          shinyjs::enable("sampleSizeTablesPerRow")
        }
      } else {
        shinyjs::disable("sampleSizeVar3Display")
        shinyjs::disable("sampleSizeTablesPerRow")
      }
    })
    
    # Observer to enable/disable tables per row slider based on display mode
    observe({
      req(input$sampleSizeVar3Display)
      
      if (input$sampleSizeVar3Display == "facets") {
        shinyjs::enable("sampleSizeTablesPerRow")
      } else {
        shinyjs::disable("sampleSizeTablesPerRow")
      }
    })
    
    # Create sample size table output
    output$sampleSizeTable <- renderUI({
      req(values$filteredData, input$independentVar1)
      
      # Get clean variable names
      cleanVars <- getCleanVariables(input, labels)
      
      # Get display options
      var3Display <- if (!is.null(input$sampleSizeVar3Display)) input$sampleSizeVar3Display else "facets"
      showTotals <- if (!is.null(input$sampleSizeShowTotals)) input$sampleSizeShowTotals else FALSE
      hideEmpty <- if (!is.null(input$sampleSizeHideEmpty)) input$sampleSizeHideEmpty else FALSE
      
      # Generate table
      table_data <- generateSampleSizeTable(
        values$filteredData,
        cleanVars$indVar1,
        cleanVars$indVar2,
        cleanVars$indVar3,
        values$var1_selected,
        values$var1_order,
        values$var2_selected,
        values$var2_order,
        values$var3_selected,
        values$var3_order,
        var3Display,
        showTotals,
        hideEmpty,
        labels
      )
      
      # Get tables per row option (only for facets)
      tables_per_row <- if (!is.null(input$sampleSizeTablesPerRow) && var3Display == "facets") {
        input$sampleSizeTablesPerRow
      } else {
        1
      }
      
      renderSampleSizeTable(table_data, labels, tables_per_row)
    })
    
    # Create subgroup warning message output
    output$subgroupWarningMessage <- renderUI({
      req(values$filteredData, input$independentVar1)
      
      # Get clean variable names
      cleanVars <- getCleanVariables(input, labels)
      
      # Calculate sample sizes
      sample_sizes <- calculateSampleSizes(
        values$filteredData,
        cleanVars$indVar1,
        cleanVars$indVar2,
        cleanVars$indVar3
      )
      
      # Count problematic subgroups
      problems <- countProblematicSubgroups(sample_sizes)
      
      if (!problems$has_problems) {
        return(NULL)
      }
      
      # Build warning message based on what problems exist
      if (problems$zero_groups > 0 && problems$one_groups > 0) {
        message_key <- "subgroupWarningBoth"
        message_text <- safeGetUILabel(message_key, labels, 
                                      paste0("The variable selection includes ", problems$zero_groups, 
                                            " group(s) with no values and ", problems$one_groups,
                                            " group(s) with a single value. See the Sample sizes tab for details."))
        message_text <- gsub("\\{N1\\}", problems$zero_groups, message_text)
        message_text <- gsub("\\{N2\\}", problems$one_groups, message_text)
      } else if (problems$zero_groups > 0) {
        message_key <- "subgroupWarningZero"
        message_text <- safeGetUILabel(message_key, labels,
                                      paste0("The variable selection includes ", problems$zero_groups,
                                            " group(s) with no values. See the Sample sizes tab for details."))
        message_text <- gsub("\\{N1\\}", problems$zero_groups, message_text)
      } else {
        message_key <- "subgroupWarningOne"
        message_text <- safeGetUILabel(message_key, labels,
                                      paste0("The variable selection includes ", problems$one_groups,
                                            " group(s) with a single value. See the Sample sizes tab for details."))
        message_text <- gsub("\\{N2\\}", problems$one_groups, message_text)
      }
      
      div(
        class = "alert alert-warning",
        style = "margin-bottom: 15px;",
        icon("triangle-exclamation"),
        " ",
        message_text
      )
    })
    
    # Initialize tabbed variable properties outputs for performance
    tryCatch({
      debugLog("DATA_HANDLING: Initializing tabbed variable properties", "UI_INIT")
      
      createVar1PropertiesTabbed(input, output, values, labels, session)
      createVar2PropertiesTabbed(input, output, values, labels, session)
      createVar3PropertiesTabbed(input, output, values, labels, session)
      
      debugLog("DATA_HANDLING: Tabbed variable properties initialized successfully", "UI_INIT")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error initializing tabbed properties:", e$message), "UI_ERROR")
    })
    
    debugLog("DATA_HANDLING: All core modules loaded successfully", "APP_STARTUP")
    
    # Initialize profiling system outputs
    tryCatch({
      debugLog("DATA_HANDLING: Loading profiling server module at the very end", "PROFILING_INIT")
      source("modules/debug_profiling_server.R", local = TRUE, encoding = "UTF-8")
      debugLog("DATA_HANDLING: Initializing profiling outputs after all core modules", "PROFILING_INIT")
      initializeProfilingOutputs(input, output, session)
      debugLog("DATA_HANDLING: Profiling system initialized successfully at the end", "PROFILING_RESULT")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error initializing final profiling:", e$message), "PROFILING_RESULT")
    })
    
    # Initialize enhanced validation system - moved after all other modules
    tryCatch({
      debugLog("DATA_HANDLING: Integrating enhanced validation system", "VALIDATION_INIT")

    # Load enhanced validation system - Phase 2A
    tryCatch({
      source("modules/enhanced_validation_main.R", encoding = "UTF-8", local = FALSE)
      debugLog("DATA_HANDLING: Enhanced validation main module loaded", "VALIDATION_INIT")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error loading enhanced validation main:", e$message), "VALIDATION_ERROR")
    })

      integrateEnhancedValidation(session, values, labels, input, output)
      debugLog("DATA_HANDLING: Enhanced validation system integrated successfully", "VALIDATION_INIT")
    }, error = function(e) {
      debugLog(paste("DATA_HANDLING: Error integrating enhanced validation:", e$message), "VALIDATION_ERROR")
      # Non-blocking error - application continues without enhanced validation
    })
    
    debugLog("DATA_HANDLING: All modules loaded successfully", "APP_STARTUP")
  }
}

debugLog("DATA_HANDLING_TABBED: Main server function defined with tabbed properties support, data preservation, and user operations history", "APP_STARTUP")
