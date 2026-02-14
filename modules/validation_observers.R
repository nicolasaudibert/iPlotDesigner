# Validation observers module
# All observeEvent handlers with operation storage preservation

# ============================================================================
# Main observer creation function
# ============================================================================

# Create reactive observers for user choice dialog responses - preserves all operations
# Receives performValidationFn as parameter for proper dependency injection
createUserChoiceObservers <- function(input, output, session, values, labels, performValidationFn) {
  
  # Observer for numeric conversion - enhanced with numeric levels capture
  observeEvent(input$doNumericConversion, {
    conversion_data <- input$doNumericConversion
    
    if(!is.null(conversion_data)) {
      debugLog(paste("USER_CHOICE: Processing conversion with numeric ordering for", conversion_data$variable), "USER_OPERATION")
      
      tryCatch({
        # Enhanced - get validation data for proper level capture
        validation_data <- getValidationData(values)
        
        # Critical - capture numeric levels before conversion
        numeric_values <- validation_data[[conversion_data$variable]]
        
        # Sort numerically to preserve proper order (not alphabetically)
        unique_numeric_values <- sort(unique(numeric_values[!is.na(numeric_values)]))
        exact_levels <- as.character(unique_numeric_values)
        
        debugLog(paste("NUMERIC_CONVERSION: Captured", length(exact_levels), "levels:", 
                       paste(head(exact_levels, 5), collapse = ", ")), "USER_OPERATION")
        
        # Convert to factor with numeric ordering preserved in BOTH datasets
        factor_levels <- exact_levels  # Use captured numeric order
        
        # Apply to original data (preserved)
        values$originalData[[conversion_data$variable]] <- factor(
          values$originalData[[conversion_data$variable]], 
          levels = factor_levels
        )
        
        # Apply to filtered data
        if(!is.null(values$filteredData) && conversion_data$variable %in% names(values$filteredData)) {
          values$filteredData[[conversion_data$variable]] <- factor(
            values$filteredData[[conversion_data$variable]], 
            levels = factor_levels
          )
        }
        
        debugLog(paste("NUMERIC_CONVERSION: Applied to both datasets with levels:", 
                       paste(head(factor_levels, 5), collapse = ", ")), "USER_OPERATION")
        
        # Store numeric conversion operation with exact levels
        storeNumericConversionOperationWithLevels(
          values = values,
          variable_name = conversion_data$variable,
          var_num = conversion_data$varNum,
          unique_values_count = length(exact_levels),
          conversion_method = "categorical_with_numeric_order",
          validation_data = validation_data
        )
        
        # Clear the waiting flag
        values[[paste0("var", conversion_data$varNum, "_waiting_user_choice")]] <- FALSE
        
        # Force re-validation using injected function (no later::later)
        performValidationFn(
          conversion_data$variable,
          conversion_data$varNum,
          session,
          values,
          labels,
          input
        )
        
        # Localized success notification
        notification_text <- getCachedLabel("choiceAppliedNotification", labels, "Variable converted to categorical successfully!")
        showNotification(
          notification_text, 
          type = "message", 
          duration = 3
        )
        
      }, error = function(e) {
        debugLog(paste("USER_CHOICE: Error in conversion:", e$message), "USER_OPERATION")
        
        # Localized error notification  
        error_text <- paste(
          getCachedLabel("errorApplyingChoiceText", labels, "Error during conversion"),
          ":",
          e$message
        )
        showNotification(
          error_text, 
          type = "error", 
          duration = 5
        )
      })
    }
  }, ignoreInit = TRUE)
  
  # Observer for modalities choice results - integration preserved
  observeEvent(input$modalitiesChoiceResult, {
    
    choice_data <- input$modalitiesChoiceResult
    
    if(!is.null(choice_data)) {
      debugLog(paste("Processing modalities choice result:", choice_data$choice, "for", choice_data$variable), "USER_CHOICE_RESULT")
      
      # Extract callback info
      var_num <- extractVariableNumber(choice_data$callbackId)
      variable_name <- choice_data$variable
      
      switch(choice_data$choice,
             "auto_exclude" = {
               # Apply automatic filtering - use existing validation behavior
               debugLog("User chose automatic exclusion - proceeding with standard filtering", "USER_OPERATION")
               
               # Clear waiting flag FIRST to prevent re-triggering dialogs
               values[[paste0("var", var_num, "_waiting_user_choice")]] <- FALSE
               
               # Critical - set a flag to prevent dialog loops
               values[[paste0("var", var_num, "_auto_exclude_in_progress")]] <- TRUE
               debugLog("LOOP_PREVENTION: Set auto_exclude_in_progress flag to prevent dialog loops", "USER_OPERATION")
               
               # Apply automatic filtering directly instead of re-triggering validation
               tryCatch({
                 debugLog("AUTO_EXCLUDE: Applying automatic filtering directly to avoid validation loop", "USER_OPERATION")
                 
                 # Use validation data for re-validation
                 validation_data <- getValidationData(values)
                 validation_result <- validateGroupingVariable(validation_data, variable_name)
                 
                 if(validation_result$isValid && !is.null(validation_result$validModalities)) {
                   debugLog(paste("AUTO_EXCLUDE: Validation successful - applying", length(validation_result$validModalities), "valid modalities"), "USER_CHOICE_RESULT")
                   
                   # Apply the validated modalities immediately
                   values[[paste0("var", var_num, "_modalities")]] <- validation_data[[variable_name]]
                   values[[paste0("var", var_num, "_selected")]] <- validation_result$validModalities
                   values[[paste0("var", var_num, "_order")]] <- validation_result$validModalities
                   
                   # Initialize colors
                   if(length(validation_result$validModalities) > 0) {
                     colorPalette <- switch(var_num,
                                            "1" = defaultColors$var1,
                                            "2" = defaultColors$var2, 
                                            "3" = defaultColors$var3,
                                            defaultColors$var1
                     )
                     
                     colors <- colorPalette[1:length(validation_result$validModalities)]
                     values[[paste0("var", var_num, "_colors")]] <- setNames(colors, validation_result$validModalities)
                   }
                   
                   # Critical - apply data filtering to exclude invalid modalities from plots
                   applyUserChoiceFilteringToVariable(variable_name, validation_result$validModalities, values)
                   
                   # Store filtering operation - critical preservation
                   modality_info <- list(
                     total = validation_result$totalModalities %||% length(unique(validation_data[[variable_name]])),
                     kept = length(validation_result$validModalities),
                     filtered = if(!is.null(validation_result$filteredModalities)) length(validation_result$filteredModalities) else 0
                   )
                   
                   storeFilteringOperation(values, variable_name, var_num, "auto_exclude", 
                                           list(method = "automatic_quality_filter"), modality_info)
                   
                   # Force UI refresh
                   values[[paste0("var", var_num, "_last_update")]] <- Sys.time()
                   values[["validation_refresh_trigger"]] <- Sys.time()
                   values[["plot_refresh_trigger"]] <- Sys.time()
                   
                   debugLog(paste("AUTO_EXCLUDE: Successfully applied automatic filtering - kept", length(validation_result$validModalities), "modalities"), "USER_CHOICE_RESULT")
                   
                 } else {
                   debugLog("AUTO_EXCLUDE: Validation failed or no valid modalities", "USER_OPERATION")
                   
                   # If validation fails, reset the variable
                   resetInvalidVariableUI(var_num, session)
                   cleanupVariableProperties(var_num, values)
                 }
                 
                 # Critical - clear the loop prevention flag
                 values[[paste0("var", var_num, "_auto_exclude_in_progress")]] <- NULL
                 
                 # Resume plot generation
                 values[["plot_generation_paused"]] <- FALSE
                 
                 debugLog("LOOP_PREVENTION: Cleared auto_exclude_in_progress flag", "USER_OPERATION")
                 
               }, error = function(e) {
                 debugLog(paste("Error in auto-exclude processing:", e$message), "USER_OPERATION")
                 
                 # Critical - always clear the loop prevention flag on error
                 values[[paste0("var", var_num, "_auto_exclude_in_progress")]] <- NULL
                 values[["plot_generation_paused"]] <- FALSE
                 
                 debugLog("LOOP_PREVENTION: Cleared auto_exclude_in_progress flag after error", "USER_OPERATION")
               })
               
               # Localized notification
               notification_text <- getCachedLabel("automaticFilteringAppliedText", labels, "Modalities filtered automatically according to quality criteria")
               
               showNotification(
                 notification_text, 
                 type = "message", 
                 duration = 3
               )
             },
             
             "keep_all" = {
               # Override filtering - keep all modalities
               debugLog("User chose to keep all modalities - overriding filters", "USER_OPERATION")
               
               # Protection - pause plot generation during filtering
               values[["plot_generation_paused"]] <- TRUE
               
               tryCatch({
                 # Get all modalities from original data to preserve factor order
                 # Use originalData instead of validation_data to keep factor levels
                 variable_data <- values$originalData[[variable_name]]
                 
                 # CRITICAL: Get modalities in proper order
                 if(is.factor(variable_data)) {
                   # Use factor levels to preserve established order (e.g., numeric order)
                   all_modalities <- levels(variable_data)
                   debugLog(paste("KEEP_ALL: Using factor levels from originalData -", length(all_modalities), "modalities"), "USER_OPERATION")
                 } else {
                   # For non-factor, sort to get consistent order
                   all_modalities <- sort(unique(variable_data))
                   debugLog(paste("KEEP_ALL: Using sorted unique values -", length(all_modalities), "modalities"), "USER_OPERATION")
                 }
                 
                 # Apply to variable properties - keeping ALL modalities
                 values[[paste0("var", var_num, "_modalities")]] <- variable_data
                 values[[paste0("var", var_num, "_selected")]] <- all_modalities
                 values[[paste0("var", var_num, "_order")]] <- all_modalities
                 
                 # Initialize colors for all modalities
                 if(length(all_modalities) > 0) {
                   colorPalette <- switch(var_num,
                                          "1" = defaultColors$var1,
                                          "2" = defaultColors$var2, 
                                          "3" = defaultColors$var3,
                                          defaultColors$var1
                   )
                   
                   colors <- rep(colorPalette, ceiling(length(all_modalities) / length(colorPalette)))[1:length(all_modalities)]
                   values[[paste0("var", var_num, "_colors")]] <- setNames(colors, all_modalities)
                   
                   # Store filtering operation for keep_all - critical preservation
                   modality_info <- list(
                     total = length(all_modalities),
                     kept = length(all_modalities),
                     filtered = 0
                   )
                   
                   storeFilteringOperation(values, variable_name, var_num, "keep_all", 
                                           list(method = "keep_all_modalities"), modality_info)
                 }
                 
                 # Clear waiting flag
                 values[[paste0("var", var_num, "_waiting_user_choice")]] <- FALSE
                 
                 # Force UI refresh
                 values[[paste0("var", var_num, "_last_update")]] <- Sys.time()
                 
                 # Resume plot generation after keep_all processing
                 values[["plot_generation_paused"]] <- FALSE
                 values[["plot_refresh_trigger"]] <- Sys.time()
                 values[["validation_refresh_trigger"]] <- Sys.time()
                 
                 debugLog(paste("Successfully applied keep_all choice for var", var_num, "- keeping", length(all_modalities), "modalities"), "USER_CHOICE_RESULT")
                 
                 # Localized notification
                 notification_text <- paste(
                   getCachedLabel("allModalitiesKeptText", labels, "All modalities kept"),
                   " (",
                   length(all_modalities),
                   " ",
                   getCachedLabel("modalitiesLabel", labels, "modalities"),
                   ")"
                 )
                 
                 showNotification(
                   notification_text, 
                   type = "warning", 
                   duration = 4
                 )
                 
               }, error = function(e) {
                 debugLog(paste("Error applying keep_all choice:", e$message), "USER_OPERATION")
                 
                 # Resume plot generation even on error
                 values[["plot_generation_paused"]] <- FALSE
                 
                 # Localized error notification
                 error_text <- paste(
                   getCachedLabel("errorApplyingChoiceText", labels, "Error applying choice"),
                   ":",
                   e$message
                 )
                 
                 showNotification(
                   error_text, 
                   type = "error", 
                   duration = 5
                 )
               })
             },
             
             "cancel" = {
               # Reset variable selection
               debugLog("User cancelled variable selection", "USER_OPERATION")
               
               # Resume plot generation immediately on cancel
               values[["plot_generation_paused"]] <- FALSE
               
               resetInvalidVariableUI(var_num, session)
               cleanupVariableProperties(var_num, values)
               values[[paste0("var", var_num, "_waiting_user_choice")]] <- FALSE
             }
      )
    }
    
  }, ignoreInit = TRUE)
  
  # Observer for complexity choice results - integration preserved
  observeEvent(input$complexityChoiceResult, {
    
    choice_data <- input$complexityChoiceResult
    
    if(!is.null(choice_data)) {
      debugLog(paste("Processing complexity choice result:", choice_data$choice, "for", choice_data$variable), "USER_CHOICE_RESULT")
      
      # Extract callback info
      var_num <- extractVariableNumber(choice_data$callbackId)
      variable_name <- choice_data$variable
      
      switch(choice_data$choice,
             "smart_filter" = {
               debugLog("User chose smart filtering", "USER_OPERATION")
               
               # PROTECTION: Pause plot generation during filtering
               values[["plot_generation_paused"]] <- TRUE
               
               tryCatch({
                 # Use validation data for filtering
                 validation_data <- getValidationData(values)
                 modality_counts <- table_fast(validation_data[[variable_name]], alphabetical_order = TRUE)
                 
                 # Remove NA values before processing
                 modality_counts <- modality_counts[!is.na(names(modality_counts))]
                 
                 # Get top N most frequent modalities
                 top_n <- SMART_FILTERING_OPTIONS$top_n_default
                 sorted_counts <- sort(modality_counts, decreasing = TRUE)
                 selected_modalities <- names(sorted_counts)[1:min(top_n, length(sorted_counts))]
                 
                 # Remove any NA values from selected modalities
                 selected_modalities <- selected_modalities[!is.na(selected_modalities)]
                 
                 # Apply smart filtering to variable properties
                 values[[paste0("var", var_num, "_modalities")]] <- validation_data[[variable_name]]
                 values[[paste0("var", var_num, "_selected")]] <- selected_modalities
                 values[[paste0("var", var_num, "_order")]] <- selected_modalities
                 
                 # Store user choice filtering instead of modifying data directly
                 applyUserChoiceFilteringToVariable(variable_name, selected_modalities, values)
                 
                 # Initialize colors
                 if(length(selected_modalities) > 0) {
                   colorPalette <- switch(var_num,
                                          "1" = defaultColors$var1,
                                          "2" = defaultColors$var2, 
                                          "3" = defaultColors$var3,
                                          defaultColors$var1
                   )
                   
                   colors <- colorPalette[1:length(selected_modalities)]
                   values[[paste0("var", var_num, "_colors")]] <- setNames(colors, selected_modalities)
                   
                   # Store smart filtering operation - critical preservation
                   modality_info <- list(
                     total = length(modality_counts),
                     kept = length(selected_modalities),
                     filtered = length(modality_counts) - length(selected_modalities)
                   )
                   
                   filtering_params <- list(
                     method = "smart_filter_top_n",
                     top_n = SMART_FILTERING_OPTIONS$top_n_default
                   )
                   
                   storeFilteringOperation(values, variable_name, var_num, "smart_filter", 
                                           filtering_params, modality_info)
                 }
                 
                 # Clear waiting flag
                 values[[paste0("var", var_num, "_waiting_user_choice")]] <- FALSE
                 
                 # Resume plot generation and force refresh
                 values[["plot_generation_paused"]] <- FALSE
                 values[[paste0("var", var_num, "_last_update")]] <- Sys.time()
                 values[["plot_refresh_trigger"]] <- Sys.time()
                 values[["validation_refresh_trigger"]] <- Sys.time()   
                 
                 debugLog(paste("Smart filtering applied: kept", length(selected_modalities), "out of", length(modality_counts), "modalities"), "USER_CHOICE_RESULT")
                 
                 # Localized notification
                 notification_text <- paste(
                   getCachedLabel("smartFilteringAppliedText", labels, "Smart filtering applied"),
                   " - ",
                   length(selected_modalities),
                   " ",
                   getCachedLabel("modalitiesKeptText", labels, "modalities kept")
                 )
                 
                 showNotification(
                   notification_text, 
                   type = "message", 
                   duration = 4
                 )
                 
               }, error = function(e) {
                 debugLog(paste("Error in smart filtering:", e$message), "USER_OPERATION")
                 
                 # Localized error notification
                 error_text <- paste(
                   getCachedLabel("smartFilteringErrorText", labels, "Error during smart filtering"),
                   ":",
                   e$message
                 )
                 
                 showNotification(
                   error_text, 
                   type = "error", 
                   duration = 5
                 )
               })
             },
             
             "custom_filter" = {
               debugLog(paste("User chose custom filtering with params:", choice_data$customParams), "USER_CHOICE_RESULT")
               
               # PROTECTION: Pause plot generation during filtering
               values[["plot_generation_paused"]] <- TRUE
               
               tryCatch({
                 # Use validation data for filtering
                 validation_data <- getValidationData(values)
                 modality_counts <- table_fast(validation_data[[variable_name]], alphabetical_order = TRUE)
                 
                 # Remove NA values before processing
                 modality_counts <- modality_counts[!is.na(names(modality_counts))]
                 
                 custom_params <- choice_data$customParams
                 selected_modalities <- NULL
                 
                 # Handle the three filtering methods
                 if(!is.null(custom_params$method)) {
                   switch(custom_params$method,
                          "top_n" = {
                            # Top N filtering only
                            top_n <- custom_params$topN %||% SMART_FILTERING_OPTIONS$top_n_default
                            sorted_counts <- sort(modality_counts, decreasing = TRUE)
                            selected_modalities <- names(sorted_counts)[1:min(top_n, length(sorted_counts))]
                            debugLog(paste("CUSTOM_FILTER: Applied top_n method - keeping top", top_n), "USER_CHOICE_RESULT")
                          },
                          "min_freq" = {
                            # Minimum frequency filtering only
                            min_freq <- custom_params$minFreq %||% SMART_FILTERING_OPTIONS$min_frequency_default
                            selected_modalities <- names(modality_counts[modality_counts >= min_freq])
                            debugLog(paste("CUSTOM_FILTER: Applied min_freq method - threshold", min_freq), "USER_CHOICE_RESULT")
                          },
                          "combined" = {
                            # Combined: top N among those with min frequency
                            min_freq <- custom_params$minFreq %||% SMART_FILTERING_OPTIONS$min_frequency_default
                            top_n <- custom_params$topN %||% SMART_FILTERING_OPTIONS$top_n_default
                            
                            # First filter by minimum frequency
                            freq_filtered <- modality_counts[modality_counts >= min_freq]
                            
                            if(length(freq_filtered) > 0) {
                              # Then take top N from those
                              sorted_freq_filtered <- sort(freq_filtered, decreasing = TRUE)
                              selected_modalities <- names(sorted_freq_filtered)[1:min(top_n, length(sorted_freq_filtered))]
                            } else {
                              # Fallback: if no modality meets min frequency, take top N overall
                              sorted_counts <- sort(modality_counts, decreasing = TRUE)
                              selected_modalities <- names(sorted_counts)[1:min(top_n, length(sorted_counts))]
                            }
                            debugLog(paste("CUSTOM_FILTER: Applied combined method - min_freq:", min_freq, "top_n:", top_n), "USER_CHOICE_RESULT")
                          }
                   )
                 } else {
                   # Fallback to old logic if method not specified
                   if(!is.null(custom_params$topN)) {
                     sorted_counts <- sort(modality_counts, decreasing = TRUE)
                     selected_modalities <- names(sorted_counts)[1:min(custom_params$topN, length(sorted_counts))]
                   } else if(!is.null(custom_params$minFreq)) {
                     selected_modalities <- names(modality_counts[modality_counts >= custom_params$minFreq])
                   } else {
                     # Ultimate fallback
                     sorted_counts <- sort(modality_counts, decreasing = TRUE)
                     selected_modalities <- names(sorted_counts)[1:min(SMART_FILTERING_OPTIONS$top_n_default, length(sorted_counts))]
                   }
                 }
                 
                 # Remove any NA values from selected modalities
                 selected_modalities <- selected_modalities[!is.na(selected_modalities)]
                 
                 # Ensure we have at least some modalities
                 if(length(selected_modalities) == 0) {
                   debugLog("Custom filtering resulted in no modalities - falling back to top 10", "USER_OPERATION")
                   sorted_counts <- sort(modality_counts, decreasing = TRUE)
                   selected_modalities <- names(sorted_counts)[1:min(10, length(sorted_counts))]
                   selected_modalities <- selected_modalities[!is.na(selected_modalities)]
                 }
                 
                 # Apply custom filtering to variable properties
                 values[[paste0("var", var_num, "_modalities")]] <- validation_data[[variable_name]]
                 values[[paste0("var", var_num, "_selected")]] <- selected_modalities
                 values[[paste0("var", var_num, "_order")]] <- selected_modalities
                 
                 # Store user choice filtering instead of modifying data directly
                 applyUserChoiceFilteringToVariable(variable_name, selected_modalities, values)
                 
                 # Initialize colors
                 if(length(selected_modalities) > 0) {
                   colorPalette <- switch(var_num,
                                          "1" = defaultColors$var1,
                                          "2" = defaultColors$var2, 
                                          "3" = defaultColors$var3,
                                          defaultColors$var1
                   )
                   
                   colors <- colorPalette[1:length(selected_modalities)]
                   values[[paste0("var", var_num, "_colors")]] <- setNames(colors, selected_modalities)
                   
                   # Store custom filtering operation - critical preservation
                   modality_info <- list(
                     total = length(modality_counts),
                     kept = length(selected_modalities),
                     filtered = length(modality_counts) - length(selected_modalities)
                   )
                   
                   filtering_params <- list(
                     method = custom_params$method %||% "top_n",
                     topN = custom_params$topN,
                     minFreq = custom_params$minFreq,
                     combined_approach = custom_params$method == "combined"
                   )
                   
                   storeFilteringOperation(values, variable_name, var_num, "custom_filter", 
                                           filtering_params, modality_info)
                 }
                 
                 # Clear waiting flag
                 values[[paste0("var", var_num, "_waiting_user_choice")]] <- FALSE
                 
                 # Resume plot generation and force refresh
                 values[["plot_generation_paused"]] <- FALSE
                 values[[paste0("var", var_num, "_last_update")]] <- Sys.time()
                 values[["plot_refresh_trigger"]] <- Sys.time()
                 values[["validation_refresh_trigger"]] <- Sys.time()   
                 
                 debugLog(paste("Custom filtering applied: kept", length(selected_modalities), "out of", length(modality_counts), "modalities"), "USER_CHOICE_RESULT")
                 
                 # Localized notification
                 notification_text <- paste(
                   getCachedLabel("customFilteringAppliedText", labels, "Custom filtering applied"),
                   " - ",
                   length(selected_modalities),
                   " ",
                   getCachedLabel("modalitiesKeptText", labels, "modalities kept")
                 )
                 
                 showNotification(
                   notification_text, 
                   type = "message", 
                   duration = 4
                 )
                 
               }, error = function(e) {
                 debugLog(paste("Error in custom filtering:", e$message), "USER_OPERATION")
                 
                 # Localized error notification
                 error_text <- paste(
                   getCachedLabel("customFilteringErrorText", labels, "Error during custom filtering"),
                   ":",
                   e$message
                 )
                 
                 showNotification(
                   error_text, 
                   type = "error", 
                   duration = 5
                 )
               })
             },
             
             "continue_all" = {
               debugLog("User chose to continue with all modalities", "USER_OPERATION")
               
               # PROTECTION: Pause plot generation during filtering
               values[["plot_generation_paused"]] <- TRUE
               
               # Same as keep_all for modalities choice
               tryCatch({
                 # Use validation data
                 validation_data <- getValidationData(values)
                 all_modalities <- unique(validation_data[[variable_name]])
                 
                 values[[paste0("var", var_num, "_modalities")]] <- validation_data[[variable_name]]
                 values[[paste0("var", var_num, "_selected")]] <- all_modalities
                 values[[paste0("var", var_num, "_order")]] <- all_modalities
                 
                 # Initialize colors for all modalities
                 if(length(all_modalities) > 0) {
                   colorPalette <- switch(var_num,
                                          "1" = defaultColors$var1,
                                          "2" = defaultColors$var2, 
                                          "3" = defaultColors$var3,
                                          defaultColors$var1
                   )
                   
                   colors <- rep(colorPalette, ceiling(length(all_modalities) / length(colorPalette)))[1:length(all_modalities)]
                   values[[paste0("var", var_num, "_colors")]] <- setNames(colors, all_modalities)
                   
                   # Store continue all operation - critical preservation
                   modality_info <- list(
                     total = length(all_modalities),
                     kept = length(all_modalities), 
                     filtered = 0
                   )
                   
                   storeFilteringOperation(values, variable_name, var_num, "continue_all", 
                                           list(method = "keep_all_despite_complexity"), modality_info)
                 }
                 
                 # Clear waiting flag
                 values[[paste0("var", var_num, "_waiting_user_choice")]] <- FALSE
                 
                 # Resume plot generation and force refresh
                 values[["plot_generation_paused"]] <- FALSE
                 values[[paste0("var", var_num, "_last_update")]] <- Sys.time()
                 values[["plot_refresh_trigger"]] <- Sys.time()
                 values[["validation_refresh_trigger"]] <- Sys.time()   
                 
                 debugLog(paste("Continue with all modalities applied for var", var_num, "- keeping", length(all_modalities), "modalities"), "USER_CHOICE_RESULT")
                 
                 showNotification(
                   paste("Toutes les modalités conservées (", length(all_modalities), " modalités) - Performance réduite possible"), 
                   type = "warning", 
                   duration = 5
                 )
                 
               }, error = function(e) {
                 debugLog(paste("Error applying continue_all choice:", e$message), "USER_OPERATION")
                 showNotification(
                   paste("Erreur lors de l'application du choix:", e$message), 
                   type = "error", 
                   duration = 5
                 )
               })
             },
             
             "cancel" = {
               # Reset variable selection
               debugLog("User cancelled complex variable selection", "USER_OPERATION")
               resetInvalidVariableUI(var_num, session)
               cleanupVariableProperties(var_num, values)
               values[[paste0("var", var_num, "_waiting_user_choice")]] <- FALSE
             }
      )
    }
    
  }, ignoreInit = TRUE)
  
  # Observer for NA handling dialog confirmation
  observeEvent(input$confirmNAHandling, {
    
    choice <- input$naHandlingChoice
    custom_label <- if(!is.null(input$naCustomLabel)) input$naCustomLabel else "Unknown"
    
    # Extract variable info from stored context
    var_num <- values$current_user_choice_var_num
    variable_name <- values$current_user_choice_variable
    
    if(is.null(var_num) || is.null(variable_name)) {
      debugLog("NA_HANDLING: Missing context information", "USER_OPERATION")
      removeModal()
      return()
    }
    
    debugLog(paste("NA_HANDLING: User chose", choice, "for", variable_name), "USER_OPERATION")
    
    tryCatch({
      # Get validation data to count NA
      validation_data <- getValidationData(values)
      na_count <- sum(is.na(validation_data[[variable_name]]))
      
      # CRITICAL: Get original variable class BEFORE any transformation
      original_class <- class(values$originalData[[variable_name]])[1]
      is_originally_numeric <- original_class %in% c("numeric", "integer")
      
      debugLog(paste("NA_HANDLING: Original variable class:", original_class, 
                     "- Is numeric:", is_originally_numeric), "USER_OPERATION")
      
      switch(choice,
             "filter_na" = {
               # Filter rows with NA from both datasets
               values$originalData <- values$originalData[!is.na(values$originalData[[variable_name]]), ]
               if(!is.null(values$filteredData)) {
                 values$filteredData <- values$filteredData[!is.na(values$filteredData[[variable_name]]), ]
               }
               
               debugLog(paste("NA_HANDLING: Filtered", na_count, "rows with NA"), "USER_OPERATION")
               
               # Store operation
               storeNAHandlingOperation(values, variable_name, var_num, "filter_na", 
                                        na_count = na_count)
             },
             
             "assign_code" = {
               # CRITICAL: If variable was numeric, we need to preserve that for subsequent validation
               # Replace NA but keep the variable as numeric if it was originally numeric
               if(is_originally_numeric) {
                 # For numeric variables, we cannot assign text labels
                 # Instead, assign a numeric code (e.g., -999 or max+1)
                 # But this would interfere with numeric conversion dialog
                 # So we convert to character first, then assign label
                 values$originalData[[variable_name]] <- as.character(values$originalData[[variable_name]])
                 values$originalData[[variable_name]][is.na(values$originalData[[variable_name]])] <- custom_label
                 
                 if(!is.null(values$filteredData)) {
                   values$filteredData[[variable_name]] <- as.character(values$filteredData[[variable_name]])
                   values$filteredData[[variable_name]][is.na(values$filteredData[[variable_name]])] <- custom_label
                 }
                 
                 debugLog(paste("NA_HANDLING: Converted numeric to character and assigned code", custom_label), "USER_OPERATION")
               } else {
                 # For character/factor variables, simple replacement
                 values$originalData[[variable_name]][is.na(values$originalData[[variable_name]])] <- custom_label
                 if(!is.null(values$filteredData)) {
                   values$filteredData[[variable_name]][is.na(values$filteredData[[variable_name]])] <- custom_label
                 }
                 
                 debugLog(paste("NA_HANDLING: Assigned code", custom_label, "to", na_count, "NA values"), "USER_OPERATION")
               }
               
               # CRITICAL: Mark that user explicitly approved this modality
               # Set a flag to prevent auto-filtering of the custom label during validation
               values[[paste0("var", var_num, "_user_approved_modalities")]] <- c(
                 values[[paste0("var", var_num, "_user_approved_modalities")]],
                 custom_label
               )
               debugLog(paste("NA_HANDLING: Marked", custom_label, "as user-approved modality"), "USER_OPERATION")
               
               # Store operation
               storeNAHandlingOperation(values, variable_name, var_num, "assign_code",
                                        custom_label = custom_label,
                                        na_count = na_count)
             },
             
             "keep_na" = {
               # CRITICAL: For numeric variables, convert to character first to preserve all values including NA
               if(is_originally_numeric) {
                 values$originalData[[variable_name]] <- as.character(values$originalData[[variable_name]])
                 if(!is.null(values$filteredData)) {
                   values$filteredData[[variable_name]] <- as.character(values$filteredData[[variable_name]])
                 }
                 debugLog("NA_HANDLING: Converted numeric to character to preserve NA as category", "USER_OPERATION")
               }
               
               # Convert NA to explicit factor level
               if(is.factor(values$originalData[[variable_name]])) {
                 values$originalData[[variable_name]] <- addNA(values$originalData[[variable_name]])
               } else {
                 values$originalData[[variable_name]] <- addNA(factor(values$originalData[[variable_name]]))
               }
               
               if(!is.null(values$filteredData)) {
                 if(is.factor(values$filteredData[[variable_name]])) {
                   values$filteredData[[variable_name]] <- addNA(values$filteredData[[variable_name]])
                 } else {
                   values$filteredData[[variable_name]] <- addNA(factor(values$filteredData[[variable_name]]))
                 }
               }
               
               debugLog(paste("NA_HANDLING: Kept", na_count, "NA as separate category"), "USER_OPERATION")
               
               # CRITICAL: Mark NA level as user-approved modality
               # The NA level is represented as NA in the factor
               values[[paste0("var", var_num, "_user_approved_modalities")]] <- c(
                 values[[paste0("var", var_num, "_user_approved_modalities")]],
                 NA
               )
               debugLog("NA_HANDLING: Marked NA level as user-approved modality", "USER_OPERATION")
               
               # Store operation
               storeNAHandlingOperation(values, variable_name, var_num, "keep_na",
                                        na_count = na_count)
             }
      )
      
      # Close dialog
      removeModal()
      
      # Clear waiting flag
      values[[paste0("var", var_num, "_waiting_user_choice")]] <- FALSE
      
      # Force re-validation with cleaned data
      # The user-approved modalities flags will be used during validation
      performValidationFn(
        variable_name,
        var_num,
        session,
        values,
        labels,
        input
      )
      
      # Localized success notification
      notification_text <- getCachedLabel("choiceAppliedNotification", labels, 
                                          "NA values handled successfully")
      showNotification(notification_text, type = "message", duration = 3)
      
    }, error = function(e) {
      debugLog(paste("NA_HANDLING: Error applying choice:", e$message), "USER_OPERATION")
      
      # Localized error notification
      error_text <- paste(
        getCachedLabel("errorApplyingChoiceText", labels, "Error handling NA values"),
        ":",
        e$message
      )
      showNotification(error_text, type = "error", duration = 5)
    })
    
  }, ignoreInit = TRUE)
  
  # Observer for NA handling dialog cancellation
  observeEvent(input$cancelNAHandling, {
    
    # Extract variable info from stored context
    var_num <- values$current_user_choice_var_num
    variable_name <- values$current_user_choice_variable
    
    if(!is.null(var_num)) {
      debugLog("NA_HANDLING: User cancelled NA handling", "USER_OPERATION")
      
      # Resume plot generation immediately on cancel
      values[["plot_generation_paused"]] <- FALSE
      
      # Clear the variable selection directly
      var_input_name <- paste0("independentVar", var_num)
      updateSelectInput(session, var_input_name, selected = "")
      
      # Clean up variable properties
      values[[paste0("var", var_num, "_modalities")]] <- NULL
      values[[paste0("var", var_num, "_selected")]] <- NULL
      values[[paste0("var", var_num, "_colors")]] <- NULL
      values[[paste0("var", var_num, "_order")]] <- NULL
      values[[paste0("var", var_num, "_waiting_user_choice")]] <- FALSE
    }
    
    removeModal()
    
  }, ignoreInit = TRUE)
}

debugLog("VALIDATION_OBSERVERS: All user choice observers created with Phase 1A preservation", "MODULE_LOAD")
