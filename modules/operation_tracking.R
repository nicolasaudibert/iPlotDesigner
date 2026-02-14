# Operation Tracking Module - Phase 2B
# User operations history and data management functions - PHASE 1A CRITICAL PRESERVATION
# Extracted from variable_validation_core.R

# ============================================================================
# DATA MANAGEMENT FUNCTIONS - Preserve Original Data
# ============================================================================

# Function to ensure we validate against original data, not previously filtered data
getValidationData <- function(values) {
  # Always validate against original data + DataTable filtering only
  # Never validate against user choice filtered data
  
  if(!is.null(values$originalData)) {
    # Use original data as base
    validation_data <- values$originalData
    
    # Apply only DataTable filtering if available in current context
    # Note: This is complex to get current DT state, so we'll use a simpler approach
    # We'll reset filteredData before validation and let the system rebuild it
    
    debugLog("VALIDATION_DATA: Using original data for validation", "DATA_PRESERVE")
    return(validation_data)
  } else if(!is.null(values$filedata)) {
    # Fallback to filedata if originalData not available
    debugLog("VALIDATION_DATA: Fallback to filedata for validation", "DATA_PRESERVE")
    return(values$filedata)
  } else if(!is.null(values$filteredData)) {
    # Last resort - use filtered data (old behavior)
    debugLog("VALIDATION_DATA: Using filtered data (fallback)", "DATA_PRESERVE")
    return(values$filteredData)
  } else {
    debugLog("VALIDATION_DATA: No data available for validation", "DATA_PRESERVE")
    return(NULL)
  }
}

# Function to apply user choice filtering without modifying validation data
applyUserChoiceFilteringToVariable <- function(variable_name, selected_modalities, values) {
  tryCatch({
    # Store the user choice filtering
    storeUserChoiceFiltering(variable_name, selected_modalities, values)
    
    # Refresh filtered data to apply all stored filters
    refreshFilteredData(values)
    
    debugLog(paste("USER_CHOICE_FILTERING: Applied filtering for", variable_name, 
                   "- Kept", length(selected_modalities), "modalities"), "USER_OPERATION")
    
  }, error = function(e) {
    debugLog(paste("ERROR in applyUserChoiceFilteringToVariable:", e$message), "USER_OPERATION")
  })
}

# Function to clear user choice filtering for a variable
clearUserChoiceFilteringForVariable <- function(variable_name, values) {
  tryCatch({
    clearUserChoiceFiltering(variable_name, values)
    refreshFilteredData(values)

    # Clear operations history for this variable - PHASE 1A
    clearUserOperationsForVariable(values, variable_name)

    debugLog(paste("USER_CHOICE_FILTERING: Cleared filtering and operations for", variable_name), "USER_OPERATION")

  }, error = function(e) {
    debugLog(paste("ERROR in clearUserChoiceFilteringForVariable:", e$message), "USER_OPERATION")
  })
}

# ============================================================================
# USER OPERATIONS HISTORY FUNCTIONS
# ============================================================================

# Initialize user operations history structure
initializeUserOperationsHistory <- function(values) {
  values$user_operations_history <- list(
    operations = list(),  # Main operations storage
    session_id = paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    created_at = Sys.time(),
    last_updated = NULL,
    operation_count = 0
  )
  
  debugLog("USER_OPERATIONS: History structure initialized", "USER_OPERATION")
}

# Core operation storage function
storeUserOperation <- function(values, operation_type, variable_name, var_num, 
                              operation_data, metadata = NULL) {
  
  tryCatch({
    operation_id <- paste0("op_", values$user_operations_history$operation_count + 1)
    
    operation <- list(
      id = operation_id,
      timestamp = Sys.time(),
      type = operation_type,  # "numeric_conversion", "custom_filtering", "complexity_choice"
      variable_name = variable_name,
      variable_number = var_num,
      data = operation_data,
      metadata = metadata
    )
    
    # Add to operations list
    values$user_operations_history$operations[[operation_id]] <- operation
    values$user_operations_history$operation_count <- values$user_operations_history$operation_count + 1
    values$user_operations_history$last_updated <- Sys.time()
    
    # Memory management - keep only recent operations
    if (values$user_operations_history$operation_count > USER_OPERATIONS_CONFIG$max_operations_stored) {
      oldest_id <- names(values$user_operations_history$operations)[1]
      values$user_operations_history$operations[[oldest_id]] <- NULL
    }
    
    debugLog(paste("USER_OPERATIONS: Stored operation", operation_type, "for", variable_name), 
             "USER_OPERATION")
    
  }, error = function(e) {
    debugLog(paste("USER_OPERATIONS: Error storing operation:", e$message), 
             "USER_OPERATION")
  })
}

storeFilteringOperation <- function(values, variable_name, var_num, filtering_choice, 
                                   filtering_params = NULL, modality_info = NULL) {
  
  operation_data <- list(
    action = filtering_choice,  # "auto_exclude", "keep_all", "smart_filter", "custom_filter", "continue_all"
    filtering_method = if (!is.null(filtering_params$method)) filtering_params$method else "auto",
    parameters = filtering_params,
    original_modality_count = if (!is.null(modality_info$total)) modality_info$total else NA,
    kept_modality_count = if (!is.null(modality_info$kept)) modality_info$kept else NA,
    filtered_modality_count = if (!is.null(modality_info$filtered)) modality_info$filtered else NA
  )
  
  metadata <- list(
    dialog_type = if (filtering_choice %in% c("smart_filter", "custom_filter", "continue_all")) {
      "high_complexity"
    } else {
      "single_value_modalities"
    },
    user_choice = TRUE,
    complexity_level = if (!is.null(modality_info$total)) {
      getComplexityLevel(modality_info$total)
    } else NULL
  )
  
  storeUserOperation(values, "custom_filtering", variable_name, var_num, 
                    operation_data, metadata)
}

# Helper function to get all operations by type
getUserOperationsByType <- function(values, operation_type) {
  if (is.null(values$user_operations_history) || 
      is.null(values$user_operations_history$operations)) {
    return(list())
  }
  
  operations <- values$user_operations_history$operations
  type_ops <- operations[sapply(operations, function(op) op$type == operation_type)]
  
  return(type_ops)
}

clearUserOperationsForVariable <- function(values, variable_name) {
  if (is.null(values$user_operations_history) || 
      is.null(values$user_operations_history$operations)) {
    return()
  }
  
  operations <- values$user_operations_history$operations
  
  # More defensive filtering
  remaining_ops <- Filter(function(op) {
    # Check if op is valid and has variable_name field
    if (is.null(op) || !is.list(op)) return(TRUE)
    if (!"variable_name" %in% names(op)) return(TRUE)
    
    # Keep operation if variable_name is different
    return(op$variable_name != variable_name)
  }, operations)
  
  values$user_operations_history$operations <- remaining_ops
  values$user_operations_history$operation_count <- length(remaining_ops)
  values$user_operations_history$last_updated <- Sys.time()
  
  debugLog(paste("USER_OPERATIONS: Cleared operations for", variable_name), 
           "USER_OPERATION")
}

# ============================================================================
# NUMERIC LEVELS CAPTURE FUNCTIONS
# ============================================================================

# Capture numeric order before alphabetical sorting
captureNumericLevelsForConversion <- function(variable_data, variable_name) {
  if (is.numeric(variable_data)) {
    # Sort numerically, not alphabetically
    unique_numeric_values <- sort(unique(variable_data[!is.na(variable_data)]))
    
    debugLog(paste("NUMERIC_CAPTURE: Captured", length(unique_numeric_values), 
                   "numeric levels for", variable_name), "USER_OPERATION")
    
    return(as.character(unique_numeric_values))
  }
  return(NULL)
}

# Enhanced function to capture exact levels during validation
captureExactLevelsFromValidation <- function(validation_data, variable_name, is_numeric_conversion = FALSE) {
  if (is.null(validation_data) || is.null(variable_name) || !variable_name %in% names(validation_data)) {
    return(NULL)
  }
  
  variable_data <- validation_data[[variable_name]]
  
  if (is_numeric_conversion && is.numeric(variable_data)) {
    # For numeric conversions: preserve numeric order
    exact_levels <- captureNumericLevelsForConversion(variable_data, variable_name)
    
    debugLog(paste("EXACT_LEVELS: Captured numeric levels for", variable_name, 
                   "- First few:", paste(head(exact_levels, 5), collapse = ", ")), "USER_OPERATION")
    
    return(exact_levels)
  } else if (is.factor(variable_data)) {
    # For existing factors: preserve current levels
    exact_levels <- levels(variable_data)
    
    debugLog(paste("EXACT_LEVELS: Captured factor levels for", variable_name, 
                   "- Count:", length(exact_levels)), "USER_OPERATION")
    
    return(exact_levels)
  } else {
    # For character variables: use unique values in original data order
    exact_levels <- unique(variable_data[!is.na(variable_data)])
    
    debugLog(paste("EXACT_LEVELS: Captured character levels for", variable_name, 
                   "- Count:", length(exact_levels)), "USER_OPERATION")
    
    return(as.character(exact_levels))
  }
}

# ============================================================================
# ENHANCED USER OPERATIONS STORAGE WITH EXACT LEVELS
# ============================================================================

# Enhanced numeric conversion storage - replaces existing function
storeNumericConversionOperationWithLevels <- function(values, variable_name, var_num, 
                                                      unique_values_count, conversion_method = "categorical",
                                                      validation_data = NULL) {
  
  # Capture exact numeric levels from validation data
  exact_levels <- NULL
  if (!is.null(validation_data)) {
    exact_levels <- captureExactLevelsFromValidation(validation_data, variable_name, is_numeric_conversion = TRUE)
  }
  
  # Fallback: try to get from current variable order if validation_data not available
  if (is.null(exact_levels) && !is.null(values[[paste0("var", var_num, "_order")]])) {
    current_order <- values[[paste0("var", var_num, "_order")]]
    # Check if these look like numeric values
    if (all(grepl("^[0-9.+-]+$", current_order))) {
      # Sort numerically for proper order
      exact_levels <- as.character(sort(as.numeric(current_order)))
    } else {
      exact_levels <- current_order
    }
  }
  
  operation_data <- list(
    action = "convert_to_categorical",
    original_type = "numeric",
    target_type = "factor",
    unique_values_count = unique_values_count,
    conversion_method = conversion_method,
    exact_levels = exact_levels,  # Store exact levels used in application
    levels_source = if (!is.null(exact_levels)) "numeric_sort" else "unavailable",
    threshold_used = USER_CHOICE_SETTINGS$numeric_conversion_threshold
  )
  
  metadata <- list(
    dialog_type = "numeric_variable",
    user_choice = TRUE,
    performance_impact = if (!is.null(unique_values_count)) {
      estimatePerformanceImpact(unique_values_count)
    } else NULL,
    levels_captured = !is.null(exact_levels),
    capture_timestamp = Sys.time()
  )
  
  storeUserOperation(values, "numeric_conversion", variable_name, var_num, 
                     operation_data, metadata)
  
  debugLog(paste("NUMERIC_OPERATION: Stored conversion for", variable_name, 
                 "with", if (!is.null(exact_levels)) length(exact_levels) else "no", "exact levels"), "USER_OPERATION")
           
}

# Enhanced filtering operation storage with exact modalities
storeFilteringOperationWithModalities <- function(values, variable_name, var_num, filtering_choice,
                                                  filtering_params = NULL, modality_info = NULL,
                                                  validation_data = NULL) {
  
  # Capture exact kept modalities
  exact_kept_modalities <- NULL
  if (!is.null(values[[paste0("var", var_num, "_selected")]])) {
    exact_kept_modalities <- values[[paste0("var", var_num, "_selected")]]
  }
  
  # Capture original order if available
  exact_original_levels <- NULL
  if (!is.null(validation_data)) {
    exact_original_levels <- captureExactLevelsFromValidation(validation_data, variable_name, is_numeric_conversion = FALSE)
  }
  
  operation_data <- list(
    action = filtering_choice,
    filtering_method = if (!is.null(filtering_params$method)) filtering_params$method else "auto",
    parameters = filtering_params,
    original_modality_count = if (!is.null(modality_info$total)) modality_info$total else NA,
    kept_modality_count = if (!is.null(modality_info$kept)) modality_info$kept else NA,
    filtered_modality_count = if (!is.null(modality_info$filtered)) modality_info$filtered else NA,
    exact_kept_modalities = exact_kept_modalities,  # Store exact modalities kept
    exact_original_levels = exact_original_levels   # Store original order before filtering
  )
  
  metadata <- list(
    dialog_type = if (filtering_choice %in% c("smart_filter", "custom_filter", "continue_all")) {
      "high_complexity"
    } else {
      "single_value_modalities"
    },
    user_choice = TRUE,
    complexity_level = if (!is.null(modality_info$total)) {
      getComplexityLevel(modality_info$total)
    } else NULL,
    modalities_captured = !is.null(exact_kept_modalities),
    capture_timestamp = Sys.time()
  )
  
  storeUserOperation(values, "custom_filtering", variable_name, var_num, 
                     operation_data, metadata)
  
  debugLog(paste("FILTERING_OPERATION: Stored filtering for", variable_name, 
                 "- Method:", filtering_choice, "- Kept:", if (!is.null(exact_kept_modalities)) length(exact_kept_modalities) else "unknown"), "USER_OPERATION")
                 
           
}

# # ============================================================================
# # COMPATIBILITY FUNCTIONS
# # ============================================================================
# 
# # Wrapper to maintain compatibility with existing calls
# storeFilteringOperation <- function(values, variable_name, var_num, filtering_choice,
#                                     filtering_params = NULL, modality_info = NULL) {
#   
#   # Call enhanced version without validation_data (will use fallback)
#   storeFilteringOperationWithModalities(
#     values = values,
#     variable_name = variable_name,
#     var_num = var_num,
#     filtering_choice = filtering_choice,
#     filtering_params = filtering_params,
#     modality_info = modality_info,
#     validation_data = NULL
#   )
# }

debugLog("OPERATION_TRACKING: User operations and data management functions loaded", "MODULE_LOAD")
      
# ============================================================================
# VARIABLE REUSE SYSTEM - Detection and Property Copying
# ============================================================================

# Find which variable slot already has this variable configured
findSourceVariableSlot <- function(variable_name, current_var_num, input, values) {
  debugLog(paste("DUPLICATE_REUSE: Searching for existing source of", variable_name, 
                 "excluding var", current_var_num), "USER_OPERATION")
  
  # Check all slots in priority order (1, 2, 3) excluding current
  for (var_num in 1:3) {
    if (var_num == current_var_num) next
    
    # Get variable name from input
    var_input_name <- paste0("independentVar", var_num)
    selected_var <- input[[var_input_name]]
    
    if (is.null(selected_var) || selected_var == "") next
    
    # Clean variable name - handle "no variable" labels
    if (var_num == 2) {
      noVar2Label <- tryCatch(
        getCachedLabel("noIndependentVar2Label", labels, "-- No independent variable 2 --"),
        error = function(e) "-- No independent variable 2 --"
      )
      if (selected_var == noVar2Label) next
    }
    
    if (var_num == 3) {
      noVar3Label <- tryCatch(
        getCachedLabel("noIndependentVar3Label", labels, "-- No independent variable 3 --"),
        error = function(e) "-- No independent variable 3 --"
      )
      if (selected_var == noVar3Label) next
    }
    
    # Check if this slot has the same variable with properties initialized
    if (selected_var == variable_name) {
      has_properties <- !is.null(values[[paste0("var", var_num, "_modalities")]]) &&
        !is.null(values[[paste0("var", var_num, "_selected")]]) &&
        length(values[[paste0("var", var_num, "_selected")]]) > 0
      
      if (has_properties) {
        debugLog(paste("DUPLICATE_REUSE: Found source var", var_num, "for", variable_name), 
                 "USER_OPERATION")
        return(var_num)
      }
    }
  }
  
  debugLog(paste("DUPLICATE_REUSE: No existing source found for", variable_name), 
           "USER_OPERATION")
  return(NULL)
}

# Check if a variable is already used elsewhere
isDuplicateVariableSelection <- function(variable_name, current_var_num, input, values) {
  source_slot <- findSourceVariableSlot(variable_name, current_var_num, input, values)
  
  result <- list(
    is_duplicate = !is.null(source_slot),
    source_slot = source_slot,
    can_reuse = !is.null(source_slot)
  )
  
  if (result$is_duplicate) {
    debugLog(paste("DUPLICATE_REUSE: Variable", variable_name, "is duplicate - source is var", 
                   source_slot), "USER_OPERATION")
  }
  
  return(result)
}

# Copy all properties from source to target variable slot
copyVariablePropertiesFromSlot <- function(source_var_num, target_var_num, 
                                           variable_name, values) {
  debugLog(paste("DUPLICATE_REUSE: Copying properties from var", source_var_num, 
                 "to var", target_var_num, "for", variable_name), "USER_OPERATION")
  
  tryCatch({
    # Copy core properties
    values[[paste0("var", target_var_num, "_modalities")]] <- 
      values[[paste0("var", source_var_num, "_modalities")]]
    
    values[[paste0("var", target_var_num, "_selected")]] <- 
      values[[paste0("var", source_var_num, "_selected")]]
    
    values[[paste0("var", target_var_num, "_order")]] <- 
      values[[paste0("var", source_var_num, "_order")]]
    
    # Copy colors (even for Var3 - stored silently for consistency)
    values[[paste0("var", target_var_num, "_colors")]] <- 
      values[[paste0("var", source_var_num, "_colors")]]
    
    # Mark target as copy
    values[[paste0("var", target_var_num, "_is_copy")]] <- TRUE
    values[[paste0("var", target_var_num, "_source_var")]] <- source_var_num
    
    # Update global reuse map
    if (is.null(values$var_reuse_map)) {
      values$var_reuse_map <- list()
    }
    values$var_reuse_map[[variable_name]] <- source_var_num
    
    # Mark timestamp for UI refresh
    values[[paste0("var", target_var_num, "_last_update")]] <- Sys.time()
    
    # Determine actual source (in case source is also a copy)
    actual_source <- if (!is.null(values[[paste0("var", source_var_num, "_source_var")]])) {
      values[[paste0("var", source_var_num, "_source_var")]]
    } else {
      source_var_num
    }
    
    modality_count <- length(values[[paste0("var", target_var_num, "_selected")]])
    
    debugLog(paste("DUPLICATE_REUSE: Successfully copied", modality_count, 
                   "modalities from var", actual_source, "to var", target_var_num), 
             "USER_OPERATION")
    
    return(list(
      success = TRUE,
      modality_count = modality_count,
      source_var = actual_source
    ))
    
  }, error = function(e) {
    debugLog(paste("DUPLICATE_REUSE: Error copying properties:", e$message), 
             "USER_OPERATION")
    return(list(
      success = FALSE, 
      error = e$message
    ))
  })
}

debugLog("OPERATION_TRACKING: Variable reuse detection and copying functions loaded", "MODULE_LOAD")

# Reorganize source/copy hierarchy when variable numbering changes
# Ensures lowest-numbered variable is always the source
reorganizeSourceHierarchy <- function(variable_name, current_var_num, input, values) {
  debugLog(paste("HIERARCHY: Checking reorganization for", variable_name, 
                 "in var", current_var_num), "USER_OPERATION")
  
  # Find all slots using this variable with initialized properties
  slots_with_variable <- c()
  for (var_num in 1:3) {
    var_input_name <- paste0("independentVar", var_num)
    selected_var <- input[[var_input_name]]
    
    if (!is.null(selected_var) && selected_var == variable_name) {
      has_properties <- !is.null(values[[paste0("var", var_num, "_selected")]]) &&
        length(values[[paste0("var", var_num, "_selected")]]) > 0
      if (has_properties) {
        slots_with_variable <- c(slots_with_variable, var_num)
      }
    }
  }
  
  if (length(slots_with_variable) <= 1) {
    return(FALSE)
  }
  
  # Lowest number becomes source
  new_source <- min(slots_with_variable)
  
  debugLog(paste("HIERARCHY: Setting var", new_source, "as source for", variable_name), 
           "USER_OPERATION")
  
  # Update all slots: lowest is source, others are copies
  for (var_num in slots_with_variable) {
    if (var_num == new_source) {
      values[[paste0("var", var_num, "_is_copy")]] <- FALSE
      values[[paste0("var", var_num, "_source_var")]] <- NULL
    } else {
      values[[paste0("var", var_num, "_is_copy")]] <- TRUE
      values[[paste0("var", var_num, "_source_var")]] <- new_source
    }
  }
  
  # Update reuse map
  values$var_reuse_map[[variable_name]] <- new_source
  
  return(TRUE)
}

# Transfer source status when a source variable is deselected
# Promotes the lowest-numbered copy to become the new source
transferSourceOnDeselection <- function(deselected_var_num, variable_name, values) {
  debugLog(paste("SOURCE_TRANSFER: Checking transfer for var", deselected_var_num, 
                 "variable:", variable_name), "USER_OPERATION")
  
  # Check if this was a copy (not a source)
  if (isTRUE(values[[paste0("var", deselected_var_num, "_is_copy")]])) {
    debugLog("SOURCE_TRANSFER: Variable was a copy, no transfer needed", "USER_OPERATION")
    return(list(transferred = FALSE))
  }
  
  # Find all copies pointing to this source
  copies <- c()
  for (var_num in 1:3) {
    if (var_num == deselected_var_num) next
    
    source_var <- values[[paste0("var", var_num, "_source_var")]]
    if (!is.null(source_var) && source_var == deselected_var_num) {
      copies <- c(copies, var_num)
    }
  }
  
  if (length(copies) == 0) {
    debugLog("SOURCE_TRANSFER: No copies found, no transfer needed", "USER_OPERATION")
    return(list(transferred = FALSE))
  }
  
  # Promote lowest-numbered copy to new source
  new_source <- min(copies)
  
  debugLog(paste("SOURCE_TRANSFER: Transferring source from var", deselected_var_num, 
                 "to var", new_source, "for", variable_name), "USER_OPERATION")
  
  # Update new source
  values[[paste0("var", new_source, "_is_copy")]] <- FALSE
  values[[paste0("var", new_source, "_source_var")]] <- NULL
  
  # Update remaining copies to point to new source
  remaining_copies <- copies[copies != new_source]
  for (var_num in remaining_copies) {
    values[[paste0("var", var_num, "_source_var")]] <- new_source
    debugLog(paste("SOURCE_TRANSFER: Updated var", var_num, 
                   "to point to new source var", new_source), "USER_OPERATION")
  }
  
  # Update reuse map
  values$var_reuse_map[[variable_name]] <- new_source
  
  debugLog(paste("SOURCE_TRANSFER: Transfer complete - var", new_source, 
                 "is now source with", length(remaining_copies), "copies"), "USER_OPERATION")
  
  return(list(
    transferred = TRUE,
    new_source = new_source,
    remaining_copies = remaining_copies
  ))
}

debugLog("OPERATION_TRACKING: Source transfer function loaded", "MODULE_LOAD")

# ============================================================================
# NA HANDLING OPERATIONS TRACKING
# ============================================================================

# Store NA handling operation in user operations history
storeNAHandlingOperation <- function(values, variable_name, var_num, handling_method,
                                     na_count = NULL, custom_label = NULL) {
  
  operation_data <- list(
    action = handling_method,
    na_count = na_count,
    custom_label = custom_label,
    method_description = switch(handling_method,
                                "filter_na" = paste("Filtered", na_count, "rows with NA values"),
                                "assign_code" = paste("Assigned code '", custom_label, "' to", na_count, "NA values"),
                                "keep_na" = paste("Kept", na_count, "NA values as separate category"),
                                "Unknown NA handling method"
    )
  )
  
  metadata <- list(
    dialog_type = "na_values",
    user_choice = TRUE,
    handling_method = handling_method
  )
  
  storeUserOperation(values, "na_handling", variable_name, var_num, 
                     operation_data, metadata)
  
  debugLog(paste("NA_OPERATION: Stored NA handling for", variable_name, "-", handling_method), 
           "USER_OPERATION")
}

debugLog("OPERATION_TRACKING: NA handling tracking functions loaded", "MODULE_LOAD")
