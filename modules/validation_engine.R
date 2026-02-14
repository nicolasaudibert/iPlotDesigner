# Validation engine module
# Core validation logic extracted from variable_validation_core.R
# Pure validation functions without observers or coordination

# ============================================================================
# Core validation function
# ============================================================================

# Core validation function
validateGroupingVariable <- function(data, varName) {
  startTime <- Sys.time()
  
  debugLog(paste("DEBUG validateGroupingVariable - Starting UNIFIED validation for:", varName), "VALIDATION_PROCESS")
  
  # Initialize values_context to NULL
  values_context <- NULL
  
  # Loop prevention - check if auto_exclude is in progress for any variable
  # This prevents dialog loops when auto_exclude triggers re-validation
  if(exists("values", envir = parent.frame(n = 4)) || exists("values", envir = parent.frame(n = 3)) || exists("values", envir = parent.frame(n = 2))) {
    # Try to access values from calling context
    values_context <- tryCatch({
      if(exists("values", envir = parent.frame(n = 2))) {
        parent.frame(n = 2)$values
      } else if(exists("values", envir = parent.frame(n = 3))) {
        parent.frame(n = 3)$values
      } else if(exists("values", envir = parent.frame(n = 4))) {
        parent.frame(n = 4)$values
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
    
    if(!is.null(values_context)) {
      # Check if any variable is in auto_exclude processing
      for(var_num in 1:3) {
        auto_exclude_flag <- paste0("var", var_num, "_auto_exclude_in_progress")
        if(!is.null(values_context[[auto_exclude_flag]]) && values_context[[auto_exclude_flag]]) {
          debugLog(paste("LOOP_PREVENTION: Auto-exclude in progress for var", var_num, "- forcing standard validation without dialogs"), "LOOP_WARN")
          
          # Force automatic mode temporarily
          original_user_choice <- tryCatch({
            isUserChoiceEnabled()
          }, error = function(e) {
            FALSE
          })
          
          # Continue with validation but skip dialog triggers
          break
        }
      }
    }
  }
  
  if(is.null(data) || nrow(data) == 0 || is.null(varName) || varName == "") {
    return(list(
      isValid = FALSE,
      shouldReject = TRUE,
      reason = "No data or variable name provided"
    ))
  }
  
  # Check if variable exists
  if(!varName %in% names(data)) {
    return(list(
      isValid = FALSE, 
      shouldReject = TRUE,
      reason = "Variable not found in data"
    ))
  }
  
  variable_data <- data[[varName]]
  
  # ============================================================================
  # PRIORITY CHECK: NA VALUES DETECTION (HIGHEST PRIORITY)
  # ============================================================================
  
  # Check if user choice is enabled
  user_choice_enabled <- tryCatch({
    # Check if we're in auto_exclude mode
    if(!is.null(values_context)) {
      for(var_num in 1:3) {
        auto_exclude_flag <- paste0("var", var_num, "_auto_exclude_in_progress")
        if(!is.null(values_context[[auto_exclude_flag]]) && values_context[[auto_exclude_flag]]) {
          debugLog("LOOP_PREVENTION: Forcing automatic behavior during auto_exclude", "LOOP_WARN")
          return(FALSE)
        }
      }
    }
    
    isUserChoiceEnabled()
  }, error = function(e) {
    FALSE
  })
  
  if (user_choice_enabled) {
    # Detect NA values early in validation
    has_na <- any(is.na(variable_data))
    
    if (has_na) {
      na_count <- sum(is.na(variable_data))
      total_count <- length(variable_data)
      na_percentage <- (na_count / total_count) * 100
      
      # Check if variable is numeric (important for dialog warning)
      is_numeric_var <- is.numeric(variable_data)
      
      debugLog(paste("NA_DETECTION: Variable", varName, "contains", na_count, 
                     "NA values (", round(na_percentage, 1), "%) - Is numeric:", is_numeric_var), 
               "VALIDATION_PROCESS")
      
      return(list(
        isValid = FALSE,
        shouldReject = FALSE,
        shouldShowDialog = TRUE,
        dialogType = "na_values",
        reason = "Variable contains NA values",
        naCount = na_count,
        totalCount = total_count,
        naPercentage = na_percentage,
        isNumeric = is_numeric_var  # ADD THIS LINE
      ))
    }
  }
  
  # ============================================================================
  # Continue with existing validation logic (numeric check, modalities, etc.)
  # ============================================================================
  
  # Check if variable is numeric (always problematic for grouping)
  if(is.numeric(variable_data)) {
    unique_count <- length(unique(variable_data[!is.na(variable_data)]))
    
    debugLog(paste("UNIFIED_VALIDATION: Numeric variable detected with", unique_count, "unique values"), "VALIDATION_PROCESS")
    
    # Check if user choice is enabled AND not in auto_exclude mode
    user_choice_enabled <- tryCatch({
      # Check if we're in auto_exclude mode
      if(!is.null(values_context)) {
        for(var_num in 1:3) {
          auto_exclude_flag <- paste0("var", var_num, "_auto_exclude_in_progress")
          if(!is.null(values_context[[auto_exclude_flag]]) && values_context[[auto_exclude_flag]]) {
            debugLog("LOOP_PREVENTION: Forcing automatic behavior during auto_exclude", "LOOP_WARN")
            return(FALSE)
          }
        }
      }
      
      isUserChoiceEnabled()
    }, error = function(e) {
      FALSE
    })
    
    if (user_choice_enabled) {
      # Unified mode - return dialog trigger for numeric variables
      return(list(
        isValid = FALSE,
        shouldReject = FALSE,
        shouldShowDialog = TRUE,
        dialogType = "numeric_variable",
        reason = "Numeric variables cannot be used for grouping",
        totalModalities = unique_count,
        uniqueValues = unique_count,
        isNumeric = TRUE
      ))
    } else {
      # Automatic mode - direct rejection (existing behavior)
      return(list(
        isValid = FALSE,
        shouldReject = TRUE,
        reason = "Numeric variables cannot be used for grouping",
        totalModalities = unique_count
      ))
    }
  }
  
  # Process categorical variable with optimized performance utilities
  debugLog("PERFORMANCE_OPTIMIZATION: Starting optimized modality processing", "PERFORMANCE_OPTIMIZATION")
  
  tryCatch({
    debugLog(paste("PERFORMANCE_OPTIMIZATION: Starting count-filter for variable:", varName), "PERFORMANCE_OPTIMIZATION")
    
    # Retrieve user-approved modalities if available
    user_approved <- NULL
    if (!is.null(values_context)) {
      for (var_num in 1:3) {
        approved_key <- paste0("var", var_num, "_user_approved_modalities")
        if (!is.null(values_context[[approved_key]])) {
          user_approved <- values_context[[approved_key]]
          debugLog(paste("VALIDATION: Found", length(user_approved), "user-approved modalities for validation"), "VALIDATION_PROCESS")
          break
        }
      }
    }
    
    # Use optimized count and filter operations
    count_result <- count_filter_fast(data, varName, min_count = 2, sort_result = TRUE, user_approved_modalities = user_approved)
    
    # Extract results
    total_modalities <- length(count_result$all_counts)
    valid_modalities <- count_result$valid_modalities
    filtered_modalities <- count_result$filtered_modalities
    
    debugLog(paste("VALIDATE_GROUPING:  Total modalities:", total_modalities), "VALIDATION_PROCESS")
    debugLog(paste("VALIDATE_GROUPING:  Valid modalities (n>=2):", length(valid_modalities)), "VALIDATION_PROCESS")
    debugLog(paste("VALIDATE_GROUPING:  Filtered modalities (n<2):", length(filtered_modalities)), "VALIDATION_PROCESS")
    
    # Check for insufficient data
    if(length(valid_modalities) == 0) {
      return(list(
        isValid = FALSE,
        shouldReject = TRUE,
        reason = "No modalities with at least 2 data points",
        totalModalities = total_modalities,
        validModalities = valid_modalities,
        filteredModalities = filtered_modalities
      ))
    }
    
    # Variable is valid - check if we should show complexity dialog
    user_choice_enabled <- tryCatch({
      # Check if we're in auto_exclude mode
      if(!is.null(values_context)) {
        for(var_num in 1:3) {
          auto_exclude_flag <- paste0("var", var_num, "_auto_exclude_in_progress")
          if(!is.null(values_context[[auto_exclude_flag]]) && values_context[[auto_exclude_flag]]) {
            debugLog("LOOP_PREVENTION: Skipping dialog triggers during auto_exclude", "LOOP_WARN")
            return(FALSE)
          }
        }
      }
      
      isUserChoiceEnabled()
    }, error = function(e) {
      FALSE
    })
    
    if (user_choice_enabled) {
      # Unified mode - check thresholds for dialog triggers
      
      # Check complexity threshold
      complexity_threshold <- tryCatch({
        USER_CHOICE_SETTINGS$complexity_warning_threshold
      }, error = function(e) {
        200
      })
      
      if (total_modalities >= complexity_threshold) {
        debugLog(paste("UNIFIED_VALIDATION: High complexity detected -", total_modalities, "modalities >= threshold", complexity_threshold), "VALIDATION_PROCESS")
        
        return(list(
          isValid = TRUE,
          shouldReject = FALSE,
          shouldShowDialog = TRUE,
          dialogType = "high_complexity",
          reason = paste("High complexity variable with", total_modalities, "modalities"),
          totalModalities = total_modalities,
          validModalities = valid_modalities,
          filteredModalities = filtered_modalities,
          hasFilteredModalities = length(filtered_modalities) > 0,
          modalityCounts = count_result$all_counts
        ))
      }
      
      # Check filtered modalities
      if(total_modalities > 0) {
        
        if (length(filtered_modalities) > 0) {
          debugLog(paste("UNIFIED_VALIDATION: ", length(filtered_modalities), "filtered out of", total_modalities), "VALIDATION_PROCESS")
          
          return(list(
            isValid = TRUE,
            shouldReject = FALSE,
            shouldShowDialog = TRUE,
            dialogType = "single_value_modalities",
            reason = paste(length(filtered_modalities), "modalities filtered"),
            totalModalities = total_modalities,
            validModalities = valid_modalities,
            filteredModalities = filtered_modalities,
            hasFilteredModalities = TRUE,
            modalityCounts = count_result$all_counts
          ))
        }
      }
    }
    
    # Standard valid result (both modes)
    elapsedTime <- as.numeric(difftime(Sys.time(), startTime, units = "secs"))
    
    debugLog(paste("PERFORMANCE SUCCESS: Variable (", total_modalities, "modalities) processed in", 
                   round(elapsedTime, 3), "seconds using", count_result$method_used), "PERFORMANCE_OPTIMIZATION")
    
    return(list(
      isValid = TRUE,
      shouldReject = FALSE,
      totalModalities = total_modalities,
      validModalities = valid_modalities,
      filteredModalities = filtered_modalities,
      hasFilteredModalities = length(filtered_modalities) > 0,
      modalityCounts = count_result$all_counts,
      validModalityCounts = setNames(count_result$all_counts[valid_modalities], valid_modalities)
    ))
    
  }, error = function(e) {
    debugLog(paste("ERROR in validation for", varName, ":", e$message), "VALIDATION_ERROR")
    
    # Final fallback to simple validation
    return(list(
      isValid = FALSE,
      shouldReject = TRUE,
      reason = paste("Validation error:", e$message)
    ))
  })
}

# ============================================================================
# Modality processing utilities
# ============================================================================

# Apply immediate modality filtering - core logic only
applyImmediateModalityFiltering <- function(varNum, varName, validationResult, values, validation_data = NULL) {
  debugLog(paste("IMMEDIATE_FILTERING: Starting for var", varNum, varName), "APP_DEBUG")
  
  # Use validation_data if provided, otherwise fall back to current system
  if(is.null(validation_data)) {
    validation_data <- values$filteredData
  }
  
  # Initialize/update variable data immediately
  values[[paste0("var", varNum, "_modalities")]] <- validation_data[[varName]]
  values[[paste0("var", varNum, "_selected")]] <- validationResult$validModalities
  values[[paste0("var", varNum, "_order")]] <- validationResult$validModalities
  
  # Initialize colors if needed
  if(length(validationResult$validModalities) > 0) {
    colorPalette <- switch(varNum,
      "1" = defaultColors$var1,
      "2" = defaultColors$var2, 
      "3" = defaultColors$var3,
      defaultColors$var1  # fallback
    )
    
    colors <- colorPalette[1:length(validationResult$validModalities)]
    if(length(validationResult$validModalities) > length(colorPalette)) {
      colors <- rep(colorPalette, ceiling(length(validationResult$validModalities) / length(colorPalette)))[1:length(validationResult$validModalities)]
    }
    values[[paste0("var", varNum, "_colors")]] <- setNames(colors, validationResult$validModalities)
  } else {
    values[[paste0("var", varNum, "_colors")]] <- NULL
  }
    
  debugLog(paste("IMMEDIATE_FILTERING: Completed for var", varNum, 
                "- Modalities:", length(validationResult$validModalities), "- Selected:", length(values[[paste0("var", varNum, "_selected")]])), "APP_DEBUG")            
}

# ============================================================================
# Dialog support functions - moved to main file due to reactive dependencies
# ============================================================================

# Note: showUnifiedValidationDialog, handleAutomaticFallback, and storeUserChoiceContext
# are kept in the main validation file due to reactive context requirements

debugLog("VALIDATION_ENGINE: Core validation functions loaded", "MODULE_LOAD")
