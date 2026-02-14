


# ============================================================================
# Dialog orchestration functions - kept here (reactive context required)
# ============================================================================

# Unified dialog orchestration - main entry point for dialogs
showUnifiedValidationDialog <- function(variable_name, var_num, validation_result, session, values, labels) {
  
  debugLog(paste("UNIFIED_DIALOG: Showing dialog for", variable_name, "type:", validation_result$dialogType), "VALIDATION_DIALOG")
  
  # Load enhanced validation module if needed
  if (!exists("showIntelligentValidationDialog", envir = .GlobalEnv)) {
    tryCatch({
      source("modules/enhanced_validation_main.R", encoding = "UTF-8", local = FALSE)
      debugLog("UNIFIED_DIALOG: Enhanced validation module loaded", "VALIDATION_DIALOG")
    }, error = function(e) {
      debugLog(paste("UNIFIED_DIALOG: Error loading module:", e$message), "VALIDATION_ERROR")
      # Fallback to automatic behavior
      handleAutomaticFallback(variable_name, var_num, validation_result, session, values)
      return()
    })
  }
  
  # Prepare complexity information
  complexity_info <- list(
    modality_count = validation_result$totalModalities %||% 0,
    complexity_level = getComplexityLevel(validation_result$totalModalities %||% 0),
    performance_impact = estimatePerformanceImpact(validation_result$totalModalities %||% 0),
    is_numeric = !is.null(validation_result$isNumeric) && validation_result$isNumeric,
    variable_data = if(!is.null(validation_result$isNumeric) && validation_result$isNumeric) {
      getValidationData(values)[[variable_name]]
    } else {
      NULL
    }
  )
  
  # Generate callback ID
  callback_id <- paste0("var", var_num, "_", gsub("[^[:alnum:]]", "_", variable_name), "_", 
                        format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  # Store context for dialog handlers
  storeUserChoiceContext(values, variable_name, var_num, validation_result, complexity_info)
  
  # Show appropriate dialog based on type
  tryCatch({
    showIntelligentValidationDialog(variable_name, validation_result$dialogType, validation_result, 
                                    complexity_info, labels, session, callback_id)
    
    debugLog("UNIFIED_DIALOG: Dialog shown successfully", "VALIDATION_DIALOG")
    
  }, error = function(e) {
    debugLog(paste("UNIFIED_DIALOG: Error showing dialog:", e$message), "VALIDATION_ERROR")
    
    # Fallback to automatic behavior
    handleAutomaticFallback(variable_name, var_num, validation_result, session, values)
  })
}

# Fallback function when dialog system fails
handleAutomaticFallback <- function(variable_name, var_num, validation_result, session, values) {
  debugLog(paste("UNIFIED_FALLBACK: Applying automatic fallback for var", var_num), "VALIDATION_PROCESS")
  
  if(validation_result$dialogType == "numeric_variable" || 
     (!is.null(validation_result$shouldReject) && validation_result$shouldReject)) {
    # Reject the variable
    resetInvalidVariableUI(var_num, session)
    cleanupVariableProperties(var_num, values)
    
    # Show simple error modal
    showModal(modalDialog(
      title = "Variable Non Supportée",
      p("Cette variable ne peut pas être utilisée pour le groupement."),
      p("Variable : ", variable_name),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  } else {
    # Accept with automatic filtering
    if(!is.null(validation_result$validModalities)) {
      validation_data <- getValidationData(values)
      values[[paste0("var", var_num, "_modalities")]] <- validation_data[[variable_name]]
      values[[paste0("var", var_num, "_selected")]] <- validation_result$validModalities
      values[[paste0("var", var_num, "_order")]] <- validation_result$validModalities
      
      # Simple color assignment
      colorPalette <- switch(as.character(var_num),
                             "1" = defaultColors$var1,
                             "2" = defaultColors$var2, 
                             "3" = defaultColors$var3,
                             defaultColors$var1
      )
      
      if(length(validation_result$validModalities) > 0) {
        colors <- colorPalette[1:length(validation_result$validModalities)]
        values[[paste0("var", var_num, "_colors")]] <- setNames(colors, validation_result$validModalities)
      }
    }
  }
  
  # Clear waiting flag
  values[[paste0("var", var_num, "_waiting_user_choice")]] <- FALSE
  
  debugLog("UNIFIED_FALLBACK: Fallback completed", "VALIDATION_PROCESS")
}

# Helper function to store user choice context
storeUserChoiceContext <- function(values, variable_name, var_num, validation_result = NULL, complexity_info = NULL) {
  values$current_user_choice_variable <- variable_name
  values$current_user_choice_var_num <- var_num
  values$current_user_choice_validation <- validation_result
  values$current_user_choice_complexity <- complexity_info
  
  debugLog(paste("Stored user choice context for", variable_name, "var", var_num), "USER_OPERATION")
}

# Extract variable number from callback ID
extractVariableNumber <- function(callback_id) {
  if (grepl("var1", callback_id)) return(1)
  if (grepl("var2", callback_id)) return(2) 
  if (grepl("var3", callback_id)) return(3)
  return(1) # fallback
}

debugLog("VALIDATION_ENGINE: Core validation functions loaded", "MODULE_LOAD")
# Variable Validation Core Module


# ============================================================================
# Modular imports - safe functions only
# ============================================================================

# Load validation engine (pure validation logic - no reactive dependencies)
tryCatch({
  if (file.exists("modules/validation_engine.R")) {
    source("modules/validation_engine.R", local = FALSE, encoding = "UTF-8")
    debugLog("VARIABLE_VALIDATION_CORE: validation_engine.R loaded successfully", "MODULE_LOAD")
  } else {
    stop("validation_engine.R not found")
  }
}, error = function(e) {
  debugLog(paste("VARIABLE_VALIDATION_CORE: Error loading validation_engine.R:", e$message), "MODULE_ERROR")
})

# Load operation tracking (Phase 1A functions - no reactive dependencies)
tryCatch({
  if (file.exists("modules/operation_tracking.R")) {
    source("modules/operation_tracking.R", local = FALSE, encoding = "UTF-8")
    debugLog("VARIABLE_VALIDATION_CORE: operation_tracking.R loaded successfully", "MODULE_LOAD")
  } else {
    stop("operation_tracking.R not found")
  }
}, error = function(e) {
  debugLog(paste("VARIABLE_VALIDATION_CORE: Error loading operation_tracking.R:", e$message), "MODULE_ERROR")
})

# Load validation observers (user choice handlers - no reactive dependencies in observers themselves)
tryCatch({
  if (file.exists("modules/validation_observers.R")) {
    source("modules/validation_observers.R", local = FALSE, encoding = "UTF-8")
    debugLog("VARIABLE_VALIDATION_CORE: validation_observers.R loaded successfully", "MODULE_LOAD")
  } else {
    stop("validation_observers.R not found")
  }
}, error = function(e) {
  debugLog(paste("VARIABLE_VALIDATION_CORE: Error loading validation_observers.R:", e$message), "MODULE_ERROR")
})

# ============================================================================
# Main validation orchestration - kept in main file (reactive context required)
# ============================================================================

# Enhanced main validation function - KEPT HERE due to reactive dependencies
performVariableValidation <- function(varName, varNum, session, values, labels, input, user_choice_enabled = NULL) {
  startTime <- traceObserverTiming("performVariableValidation", paste("var", varNum, varName))
  
  traceObserver(paste0("var", varNum, "_validation"), varName, "START")
  
# Early protection - pause plot generation as soon as a complex validation starts
  values[["plot_generation_paused"]] <- TRUE
  debugLog(paste("EARLY_PROTECTION: Plot generation paused for var", varNum, "validation"), "PLOT_PROTECT")
  
  # Clear waiting flag if variable has changed
  current_var_name <- paste0("var", varNum, "_current_variable")
  previous_variable <- values[[current_var_name]]
  
  if(!is.null(previous_variable) && previous_variable != varName) {
    debugLog(paste("VARIABLE_CHANGE: Variable changed from", previous_variable, "to", varName, "- clearing state"), "VAR_CHANGE")
    values[[paste0("var", varNum, "_waiting_user_choice")]] <- FALSE
    
    # Clear existing properties to prevent conflicts
    values[[paste0("var", varNum, "_modalities")]] <- NULL
    values[[paste0("var", varNum, "_selected")]] <- NULL
    values[[paste0("var", varNum, "_colors")]] <- NULL
    values[[paste0("var", varNum, "_order")]] <- NULL
    
    # Clear user choice filtering for the previous variable
    if(!is.null(previous_variable) && previous_variable != "") {
      clearUserChoiceFilteringForVariable(previous_variable, values)
    }
  }
  
  # Store current variable name for future comparison
  values[[current_var_name]] <- varName
  
  # Check if this variable is waiting for user choice
  if(!is.null(values[[paste0("var", varNum, "_waiting_user_choice")]]) && 
     values[[paste0("var", varNum, "_waiting_user_choice")]]) {
    debugLog(paste("UNIFIED_VALIDATION: Var", varNum, "is waiting for user choice - skipping validation"), "VALIDATION_PROCESS")
    traceObserverTiming("performVariableValidation", "WAITING_USER_CHOICE", startTime)
    return()
  }
  
  # Use reactive protection functions directly (available in this context)
  if(isProtected()) {
    # Resume plot generation if protected
    values[["plot_generation_paused"]] <- FALSE
    traceObserver(paste0("var", varNum, "_validation"), varName, "SKIPPED - protection active")
    traceObserverTiming("performVariableValidation", "SKIPPED", startTime)
    return()
  }
  
  validationInProgress(TRUE)
  traceObserver(paste0("var", varNum, "_validation"), varName, "VALIDATION_FLAG_SET")
  
  tryCatch({
    # Get validation data (original data, not user-filtered data)
    validation_data <- getValidationData(values)
    
    if(is.null(validation_data) || nrow(validation_data) == 0) {
      debugLog(paste("No validation data available for var", varNum, "validation"), "VALIDATION_ERROR")
      validationInProgress(FALSE)
      # Resume plot generation on no data
      values[["plot_generation_paused"]] <- FALSE
      traceObserver(paste0("var", varNum, "_validation"), varName, "END - no data")
      traceObserverTiming("performVariableValidation", "NO_DATA", startTime)
      return()
    }
    
# Unified validation - single call handles both automatic and advanced modes
    validationResult <- tryCatch({
      measureTime({
        # Use validation data instead of filtered data
        result <- validateGroupingVariable(validation_data, varName)
        traceFunctionCall("validateGroupingVariable", varName, paste("isValid:", result$isValid))
        result
      }, paste("Unified validation for var", varNum, varName))
    }, error = function(e) {
      debugLog(paste("ERROR in unified validation for var", varNum, ":", e$message), "APP_ERROR")
      list(isValid = FALSE, shouldReject = TRUE, reason = paste("Validation error:", e$message))
    })
    
    # UNIFIED DECISION LOGIC
    if(!is.null(validationResult$shouldShowDialog) && validationResult$shouldShowDialog) {
# Show user choice dialog - keep plot generation paused
      debugLog(paste("UNIFIED_VALIDATION: Showing dialog for var", varNum, "- Type:", validationResult$dialogType), "VALIDATION_PROCESS")
      debugLog("DIALOG_PROTECTION: Plot generation remains paused during dialog display", "PLOT_PROTECT")
      
      # Stop processing and show dialog
      validationInProgress(FALSE)
      values[[paste0("var", varNum, "_waiting_user_choice")]] <- TRUE
      
      # Clear any existing properties to prevent conflicts
      values[[paste0("var", varNum, "_modalities")]] <- NULL
      values[[paste0("var", varNum, "_selected")]] <- NULL
      values[[paste0("var", varNum, "_colors")]] <- NULL
      values[[paste0("var", varNum, "_order")]] <- NULL
      
      # Show appropriate dialog (plot generation stays paused)
      showUnifiedValidationDialog(varName, varNum, validationResult, session, values, labels)
      
      traceObserverTiming("performVariableValidation", "DIALOG_SHOWN", startTime)
      return()
      
    } else if(!is.null(validationResult$shouldReject) && validationResult$shouldReject) {
# Automatic rejection
      traceObserver(paste0("var", varNum, "_validation"), varName, "REJECTED - immediate UI reset")
      
      # Show error modal
      tryCatch({
        showInvalidVariableModal(varName, validationResult, labels, varNum)
      }, error = function(e) {
        debugLog(paste("Error showing invalid variable modal:", e$message), "APP_ERROR")
        showModal(modalDialog(
          title = getCachedLabel("invalidVariableTitle", labels, "Invalid Variable"),
          p(getCachedLabel("invalidVariableMessage", labels, "Variable cannot be used for grouping")),
          p("Variable: ", varName),
          p("Reason: ", validationResult$reason),
          easyClose = TRUE,
          footer = modalButton(getCachedLabel("okLabel", labels, "OK"))
        ))
      })
      
      resetInvalidVariableUI(varNum, session)
      cleanupVariableProperties(varNum, values)
      validationInProgress(FALSE)
      
      # Resume plot generation after rejection
      values[["plot_generation_paused"]] <- FALSE
      
      traceObserverTiming("performVariableValidation", "REJECTED", startTime)
      return()
      
    } else if(validationResult$isValid) {
# Variable accepted - apply standard processing
      debugLog(paste("UNIFIED_VALIDATION: Variable", varName, "accepted - applying standard processing"), "VALIDATION_PROCESS")
      
      # Apply immediate modality filtering and property initialization
      if(!is.null(validationResult$validModalities)) {
        tryCatch({
          # Use validation data for property initialization
          applyImmediateModalityFiltering(varNum, varName, validationResult, values, validation_data)
          values[[paste0("var", varNum, "_last_update")]] <- Sys.time()
          debugLog(paste("UNIFIED_VALIDATION: Applied immediate filtering for", varName, "- valid modalities:", length(validationResult$validModalities)), "VALIDATION_PROCESS")
        }, error = function(e) {
          debugLog(paste("Error in immediate filtering:", e$message), "APP_ERROR")
        })
      }
      
      # Show filtered modalities notification if needed (automatic mode only)
      if(!is.null(validationResult$hasFilteredModalities) && validationResult$hasFilteredModalities) {
        user_choice_enabled <- tryCatch({
          isUserChoiceEnabled()
        }, error = function(e) {
          FALSE
        })
        
        if(!user_choice_enabled) {
          tryCatch({
            showFilteredModalitiesDialog(varName, validationResult, labels, session)
            debugLog("UNIFIED_VALIDATION: Showed filtered modalities dialog for automatic mode", "VALIDATION_PROCESS")
          }, error = function(e) {
            debugLog(paste("Error showing filtered dialog:", e$message), "APP_ERROR")
          })
        } else {
          debugLog("UNIFIED_VALIDATION: Skipped filtered modalities dialog - user choice mode handles this", "VALIDATION_PROCESS")
        }
      }
      
      # Resume plot generation after successful validation
      values[["plot_generation_paused"]] <- FALSE
      values[["plot_refresh_trigger"]] <- Sys.time()
      values[["validation_refresh_trigger"]] <- Sys.time()
      debugLog("VALIDATION_PROTECTION: Plot generation resumed after successful validation", "PLOT_PROTECT")
    }
    
    validationInProgress(FALSE)
    traceObserver(paste0("var", varNum, "_validation"), varName, "END - successful")
    traceObserverTiming("performVariableValidation", "SUCCESS", startTime)
    
  }, error = function(e) {
    debugLog(paste("ERROR during unified validation for var", varNum, ":", e$message), "APP_ERROR")
    debugLog(paste("VALIDATION MAIN ERROR STACK:", toString(sys.calls())), "APP_CRITICAL")
    
# Critical - always clean up on error
    validationInProgress(FALSE)
    values[[paste0("var", varNum, "_waiting_user_choice")]] <- FALSE
    
    # Resume plot generation even on error
    values[["plot_generation_paused"]] <- FALSE
    
    resetAllFlags()
    
    traceObserver(paste0("var", varNum, "_validation"), varName, "END - error")
    traceObserverTiming("performVariableValidation", "ERROR", startTime)
  })
}

# ============================================================================
# Integration wrapper functions - kept in main file
# ============================================================================

# Wrapper function to integrate enhanced validation with existing system
integrateEnhancedValidation <- function(session, values, labels, input, output) {
  
  debugLog("ENHANCED_VALIDATION: Integrating enhanced validation system", "VALIDATION_PROCESS")
  
  # Create user choice observers
  tryCatch({
    createUserChoiceObservers(input, output, session, values, labels)
    debugLog("ENHANCED_VALIDATION: User choice observers created successfully", "VALIDATION_PROCESS")
  }, error = function(e) {
    debugLog(paste("ENHANCED_VALIDATION: Error creating observers:", e$message), "VALIDATION_ERROR")
  })
  
  # Load enhanced validation module
  tryCatch({
    if (!exists("showIntelligentValidationDialog", envir = .GlobalEnv)) {
      source("modules/enhanced_validation_main.R", encoding = "UTF-8", local = FALSE)
      debugLog("ENHANCED_VALIDATION: Enhanced validation module loaded", "VALIDATION_PROCESS")
    }
  }, error = function(e) {
    debugLog(paste("ENHANCED_VALIDATION: Error loading module:", e$message), "VALIDATION_ERROR")
  })
  
  # Initialize user choice settings without accessing reactive value outside context
  tryCatch({
    initial_setting <- isUserChoiceEnabled()
    values$user_choice_enabled <- initial_setting
    debugLog(paste("ENHANCED_VALIDATION: Initial user choice setting:", initial_setting), "VALIDATION_PROCESS")
  }, error = function(e) {
    debugLog(paste("ENHANCED_VALIDATION: Error setting initial user choice:", e$message), "VALIDATION_ERROR")
    values$user_choice_enabled <- FALSE  # Safe fallback
  })
  
  # Observer for user choice setting changes
  observeEvent(input$enableUserChoiceDialogs, {
    if(!is.null(input$enableUserChoiceDialogs)) {
      values$user_choice_enabled <- input$enableUserChoiceDialogs
      debugLog(paste("ENHANCED_VALIDATION: User choice setting changed to:", values$user_choice_enabled), "VALIDATION_PROCESS")
    }
  }, ignoreInit = TRUE)
  
  debugLog("ENHANCED_VALIDATION: Integration completed successfully", "VALIDATION_PROCESS")
}

# ============================================================================
# Public interface functions - kept in main file
# ============================================================================

# Public interface function for checking if enhanced validation is available
isEnhancedValidationAvailable <- function() {
  return(exists("showIntelligentValidationDialog", envir = .GlobalEnv) && isUserChoiceEnabled())
}

debugLog("VARIABLE_VALIDATION_CORE: Enhanced validation core module with hybrid modular architecture loaded", "VALIDATION_PROCESS")
