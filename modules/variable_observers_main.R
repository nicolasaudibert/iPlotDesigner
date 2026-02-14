# Variable observers main module with cached labels
# Main observer functions for the three independent variables
# Includes duplicate variable reuse system

# Enhanced observer for var1 with timing and duplicate reuse
observeEvent(input$independentVar1, {
  startTime <- traceObserverTiming("independentVar1_observer", input$independentVar1)
  traceReactive("independentVar1_observer", "TRIGGERED")
  
  if(isProtected()) {
    traceObserver("independentVar1", input$independentVar1, "SKIPPED - protection active")
    traceObserverTiming("independentVar1_observer", "SKIPPED", startTime)
    return()
  }
  
  # Check if swap is in progress - skip property clearing
if(!is.null(values[["swap_in_progress"]]) && values[["swap_in_progress"]]) {
    debugLog("VAR1_OBSERVER: Swap in progress - skipping to preserve swapped properties", "VAR_SWAP")
    traceObserver("independentVar1", input$independentVar1, "SKIPPED - swap in progress")
    
    # Increment counter and clear flag when both observers have run
    values[["swap_observers_completed"]] <- values[["swap_observers_completed"]] + 1
    if(values[["swap_observers_completed"]] >= 2) {
      values[["swap_in_progress"]] <- FALSE
      debugLog("VAR_SWAP: Both swap observers completed, flag cleared", "VAR_SWAP")
    }
    
    traceObserverTiming("independentVar1_observer", "SWAP_SKIP", startTime)
    return()
  }
  
  # Detect variable change (not just deselection)
  if(!is.null(input$independentVar1) && input$independentVar1 != "") {
    previous_var <- values[["var1_current_variable"]]
    
    # If variable changed (not first selection)
    if (!is.null(previous_var) && previous_var != "" && previous_var != input$independentVar1) {
      debugLog(paste("VAR_CHANGE: Var1 changed from", previous_var, "to", input$independentVar1), "USER_OPERATION")
      
      # Transfer source if previous variable was a source
      transfer_result <- transferSourceOnDeselection(1, previous_var, values)
      
      if (transfer_result$transferred) {
        notification_text <- getCachedLabel("sourceVariableDeselected", labels,
                                            "Variable {var} is now the source (previous source deselected)")
        notification_text <- gsub("\\{var\\}", transfer_result$new_source, notification_text)
        showNotification(notification_text, type = "message", duration = 3)
      }
    }
  }
  
  # Check for duplicate variable selection
  if(!is.null(input$independentVar1) && input$independentVar1 != "") {
    duplicate_check <- isDuplicateVariableSelection(input$independentVar1, 1, input, values)
    
    if(duplicate_check$is_duplicate && duplicate_check$can_reuse) {
      debugLog(paste("DUPLICATE_REUSE: Var1 reusing", input$independentVar1, 
                     "from var", duplicate_check$source_slot), "USER_OPERATION")
      
      # Copy properties immediately - skip validation entirely
      copy_result <- copyVariablePropertiesFromSlot(
        duplicate_check$source_slot, 1, input$independentVar1, values
      )
      
      if(copy_result$success) {
        # Reorganize hierarchy to ensure lowest-numbered variable is source
        reorganizeSourceHierarchy(input$independentVar1, 1, input, values)
        
        # Force UI refresh
        values[["var1_last_update"]] <- Sys.time()
        values[["validation_refresh_trigger"]] <- Sys.time()
        values[["plot_refresh_trigger"]] <- Sys.time()
        
        # User notification
        notification_text <- getCachedLabel("duplicateVariableReuseNotification", labels, 
                                            "Variable automatically reused with existing properties")
        showNotification(notification_text, type = "message", duration = 3)
        
        # Store current variable name
        values[["var1_current_variable"]] <- input$independentVar1
        
        # Exit early - skip all validation
        traceObserver("independentVar1", input$independentVar1, "REUSED")
        traceObserverTiming("independentVar1_observer", "REUSED", startTime)
        return()
      } else {
        # Copy failed - log and continue with normal validation
        debugLog(paste("DUPLICATE_REUSE: Copy failed for var1, falling back to validation:", 
                       copy_result$error), "USER_OPERATION")
      }
    }
  }
  
  # Normal validation path (if not reused or reuse failed)
  if(!is.null(input$independentVar1) && input$independentVar1 != "") {
    measureTime({
      performVariableValidation(input$independentVar1, 1, session, values, labels, input)
    }, "Var1 complete validation")
    
    # Store current variable name after validation
    values[["var1_current_variable"]] <- input$independentVar1
    
  } else {
    traceObserver("independentVar1", "EMPTY", "CLEARING")
    
    # Transfer source status if this was a source with copies
    previous_var <- values[["var1_current_variable"]]
    if (!is.null(previous_var) && previous_var != "") {
      transfer_result <- transferSourceOnDeselection(1, previous_var, values)
      
      if (transfer_result$transferred) {
        notification_text <- getCachedLabel("sourceVariableDeselected", labels,
                                            "Variable {var} is now the source (previous source deselected)")
        notification_text <- gsub("\\{var\\}", transfer_result$new_source, notification_text)
        showNotification(notification_text, type = "message", duration = 3)
      }
    }
    
    # Clear current variable tracking
    values[["var1_current_variable"]] <- NULL
    
    # Clear properties
    values$var1_modalities <- NULL
    values$var1_selected <- NULL
    values$var1_colors <- NULL
    values$var1_order <- NULL
    values$var1_is_copy <- FALSE
    values$var1_source_var <- NULL
  }
  
  traceObserver("independentVar1", input$independentVar1, "FINISHED")
  traceObserverTiming("independentVar1_observer", "FINISHED", startTime)
}, ignoreInit = TRUE)

# Enhanced observer for var2 with synchronous reorganization and duplicate reuse
observeEvent(input$independentVar2, {
  startTime <- traceObserverTiming("independentVar2_observer", input$independentVar2)
  traceReactive("independentVar2_observer", "TRIGGERED")
  
  if(isProtected()) {
    traceObserver("independentVar2", input$independentVar2, "SKIPPED - protection active")
    traceObserverTiming("independentVar2_observer", "SKIPPED", startTime)
    return()
  }
  
  # Check if swap is in progress - skip property clearing
  if(!is.null(values[["swap_in_progress"]]) && values[["swap_in_progress"]]) {
    debugLog("VAR2_OBSERVER: Swap in progress - skipping to preserve swapped properties", "VAR_SWAP")
    traceObserver("independentVar2", input$independentVar2, "SKIPPED - swap in progress")
    
    # Increment counter and clear flag when both observers have run
    values[["swap_observers_completed"]] <- values[["swap_observers_completed"]] + 1
    if(values[["swap_observers_completed"]] >= 2) {
      values[["swap_in_progress"]] <- FALSE
      debugLog("VAR_SWAP: Both swap observers completed, flag cleared", "VAR_SWAP")
    }
    
    traceObserverTiming("independentVar2_observer", "SWAP_SKIP", startTime)
    return()
  }
  
  # Detect variable change (not just deselection)
  if(!is.null(input$independentVar2) && input$independentVar2 != "") {
    previous_var <- values[["var2_current_variable"]]
    
    # If variable changed (not first selection)
    if (!is.null(previous_var) && previous_var != "" && previous_var != input$independentVar2) {
      debugLog(paste("VAR_CHANGE: Var2 changed from", previous_var, "to", input$independentVar2), "USER_OPERATION")

      # Transfer source if previous variable was a source
      transfer_result <- transferSourceOnDeselection(2, previous_var, values)
      
      if (transfer_result$transferred) {
        notification_text <- getCachedLabel("sourceVariableDeselected", labels,
                                            "Variable {var} is now the source (previous source deselected)")
        notification_text <- gsub("\\{var\\}", transfer_result$new_source, notification_text)
        showNotification(notification_text, type = "message", duration = 3)
      }
    }
  }
  
  noVar2Label <- safeGetUILabel("noIndependentVar2Label", labels, "-- No independent variable 2 --")
  
  if(is.null(input$independentVar2) || input$independentVar2 == "" || input$independentVar2 == noVar2Label) {
    traceObserver("independentVar2", "DESELECTED", "checking reorganization")
    
    noVar3Label <- safeGetUILabel("noIndependentVar3Label", labels, "-- No independent variable 3 --")
    
    var3_has_content <- !is.null(input$independentVar3) && 
      input$independentVar3 != "" && 
      input$independentVar3 != noVar3Label
    
    if(var3_has_content) {
      traceObserver("independentVar2", "REORGANIZATION", "START")
      reorganizationInProgress(TRUE)
      globalCooldown(TRUE)
      
      var3_modalities <- values$var3_modalities
      var3_selected <- values$var3_selected
      var3_colors <- values$var3_colors
      var3_order <- values$var3_order
      
      measureTime({
        updateSelectInput(session, "independentVar2", selected = input$independentVar3)
        updateSelectInput(session, "independentVar3", selected = "")
        traceFunctionCall("updateSelectInput", "reorganization updates")
      }, "Reorganization UI updates")
      
      values$var2_modalities <- var3_modalities
      values$var2_selected <- var3_selected
      values$var2_colors <- var3_colors
      values$var2_order <- var3_order
      
      # Store the reorganized variable name
      values[["var2_current_variable"]] <- input$independentVar3
      
      values$var3_modalities <- NULL
      values$var3_selected <- NULL
      values$var3_colors <- NULL
      values$var3_order <- NULL
      values$var3_is_copy <- FALSE
      values$var3_source_var <- NULL
      
      # Synchronous reset - no delays
      resetAllFlags()
      
      traceObserver("independentVar2", "REORGANIZATION", "COMPLETE")
      traceObserverTiming("independentVar2_observer", "REORGANIZATION", startTime)
      return()
    }
    
    traceObserver("independentVar2", "DESELECTED", "simple cleanup")
    
    # Transfer source status if this was a source with copies
    previous_var <- values[["var2_current_variable"]]
    if (!is.null(previous_var) && previous_var != "") {
      transfer_result <- transferSourceOnDeselection(2, previous_var, values)
      
      if (transfer_result$transferred) {
        notification_text <- getCachedLabel("sourceVariableDeselected", labels,
                                            "Variable {var} is now the source (previous source deselected)")
        notification_text <- gsub("\\{var\\}", transfer_result$new_source, notification_text)
        showNotification(notification_text, type = "message", duration = 3)
      }
    }
    
    # Clear current variable tracking
    values[["var2_current_variable"]] <- NULL
    
    values$var2_modalities <- NULL
    values$var2_selected <- NULL
    values$var2_colors <- NULL
    values$var2_order <- NULL
    values$var2_is_copy <- FALSE
    values$var2_source_var <- NULL
    
    traceObserverTiming("independentVar2_observer", "CLEANUP", startTime)
    return()
  }
  
  # Check for duplicate variable selection
  if(!is.null(input$independentVar2) && input$independentVar2 != "" && input$independentVar2 != noVar2Label) {
    duplicate_check <- isDuplicateVariableSelection(input$independentVar2, 2, input, values)
    
    if(duplicate_check$is_duplicate && duplicate_check$can_reuse) {
      debugLog(paste("DUPLICATE_REUSE: Var2 reusing", input$independentVar2, 
                     "from var", duplicate_check$source_slot), "USER_OPERATION")
      
      # Copy properties immediately - skip validation entirely
      copy_result <- copyVariablePropertiesFromSlot(
        duplicate_check$source_slot, 2, input$independentVar2, values
      )
      
      if(copy_result$success) {
        # Reorganize hierarchy to ensure lowest-numbered variable is source
        reorganizeSourceHierarchy(input$independentVar2, 2, input, values)
        
        # Force UI refresh
        values[["var2_last_update"]] <- Sys.time()
        values[["validation_refresh_trigger"]] <- Sys.time()
        values[["plot_refresh_trigger"]] <- Sys.time()
        
        # User notification
        notification_text <- getCachedLabel("duplicateVariableReuseNotification", labels, 
                                            "Variable automatically reused with existing properties")
        showNotification(notification_text, type = "message", duration = 3)
        
        # Store current variable name
        values[["var2_current_variable"]] <- input$independentVar2
        
        # Exit early - skip all validation
        traceObserver("independentVar2", input$independentVar2, "REUSED")
        traceObserverTiming("independentVar2_observer", "REUSED", startTime)
        return()
      } else {
        # Copy failed - log and continue with normal validation
        debugLog(paste("DUPLICATE_REUSE: Copy failed for var2, falling back to validation:", 
                       copy_result$error), "USER_OPERATION")
      }
    }
  }
  
  # Normal validation path (if not reused or reuse failed)
  measureTime({
    performVariableValidation(input$independentVar2, 2, session, values, labels, input)
  }, "Var2 complete validation")
  
  # Store current variable name after validation
  values[["var2_current_variable"]] <- input$independentVar2
  
  traceObserver("independentVar2", input$independentVar2, "FINISHED")
  traceObserverTiming("independentVar2_observer", "FINISHED", startTime)
}, ignoreInit = TRUE)

# Enhanced observer for var3 with timing and duplicate reuse - identical pattern to VI1/VI2
observeEvent(input$independentVar3, {
  startTime <- traceObserverTiming("independentVar3_observer", input$independentVar3)
  traceReactive("independentVar3_observer", "TRIGGERED")
  
  if(isProtected()) {
    traceObserver("independentVar3", input$independentVar3, "SKIPPED - protection active")
    traceObserverTiming("independentVar3_observer", "SKIPPED", startTime)
    return()
  }
  
  # Check if swap is in progress - skip property clearing
  if(!is.null(values[["swap_in_progress"]]) && values[["swap_in_progress"]]) {
    debugLog("VAR3_OBSERVER: Swap in progress - skipping to preserve swapped properties", "VAR_SWAP")
    traceObserver("independentVar3", input$independentVar3, "SKIPPED - swap in progress")
    
    # Increment counter and clear flag when both observers have run
    values[["swap_observers_completed"]] <- values[["swap_observers_completed"]] + 1
    if(values[["swap_observers_completed"]] >= 2) {
      values[["swap_in_progress"]] <- FALSE
      debugLog("VAR_SWAP: Both swap observers completed, flag cleared", "VAR_SWAP")
    }
    
    traceObserverTiming("independentVar3_observer", "SWAP_SKIP", startTime)
    return()
  }
  
  # Detect variable change (not just deselection)
  if(!is.null(input$independentVar3) && input$independentVar3 != "") {
    previous_var <- values[["var3_current_variable"]]
    
    # If variable changed (not first selection)
    if (!is.null(previous_var) && previous_var != "" && previous_var != input$independentVar3) {
      debugLog(paste("VAR_CHANGE: Var3 changed from", previous_var, "to", input$independentVar3), "USER_OPERATION")
      
      # Transfer source if previous variable was a source
      transfer_result <- transferSourceOnDeselection(3, previous_var, values)
      
      if (transfer_result$transferred) {
        notification_text <- getCachedLabel("sourceVariableDeselected", labels,
                                            "Variable {var} is now the source (previous source deselected)")
        notification_text <- gsub("\\{var\\}", transfer_result$new_source, notification_text)
        showNotification(notification_text, type = "message", duration = 3)
      }
    }
  }
  
  noVar3Label <- safeGetUILabel("noIndependentVar3Label", labels, "-- No independent variable 3 --")
  
  if(is.null(input$independentVar3) || input$independentVar3 == "" || input$independentVar3 == noVar3Label) {
    traceObserver("independentVar3", "DESELECTED", "simple cleanup")
    
    # Transfer source status if this was a source with copies
    previous_var <- values[["var3_current_variable"]]
    if (!is.null(previous_var) && previous_var != "") {
      transfer_result <- transferSourceOnDeselection(3, previous_var, values)
      
      if (transfer_result$transferred) {
        notification_text <- getCachedLabel("sourceVariableDeselected", labels,
                                            "Variable {var} is now the source (previous source deselected)")
        notification_text <- gsub("\\{var\\}", transfer_result$new_source, notification_text)
        showNotification(notification_text, type = "message", duration = 3)
      }
    }
    
    # Clear current variable tracking
    values[["var3_current_variable"]] <- NULL
    
    values$var3_modalities <- NULL
    values$var3_selected <- NULL
    values$var3_colors <- NULL
    values$var3_order <- NULL
    values$var3_is_copy <- FALSE
    values$var3_source_var <- NULL
    
    traceObserverTiming("independentVar3_observer", "CLEANUP", startTime)
    return()
  }
  
  # Check for duplicate variable selection
  if(!is.null(input$independentVar3) && input$independentVar3 != "" && input$independentVar3 != noVar3Label) {
    duplicate_check <- isDuplicateVariableSelection(input$independentVar3, 3, input, values)
    
    if(duplicate_check$is_duplicate && duplicate_check$can_reuse) {
      debugLog(paste("DUPLICATE_REUSE: Var3 reusing", input$independentVar3, 
                     "from var", duplicate_check$source_slot), "USER_OPERATION")
      
      # Copy properties immediately - skip validation entirely
      copy_result <- copyVariablePropertiesFromSlot(
        duplicate_check$source_slot, 3, input$independentVar3, values
      )
      
      if(copy_result$success) {
        # Reorganize hierarchy to ensure lowest-numbered variable is source
        reorganizeSourceHierarchy(input$independentVar3, 3, input, values)
        
        # Force UI refresh
        values[["var3_last_update"]] <- Sys.time()
        values[["validation_refresh_trigger"]] <- Sys.time()
        values[["plot_refresh_trigger"]] <- Sys.time()
        
        # User notification
        notification_text <- getCachedLabel("duplicateVariableReuseNotification", labels, 
                                            "Variable automatically reused with existing properties")
        showNotification(notification_text, type = "message", duration = 3)
        
        # Store current variable name
        values[["var3_current_variable"]] <- input$independentVar3
        
        # Exit early - skip all validation
        traceObserver("independentVar3", input$independentVar3, "REUSED")
        traceObserverTiming("independentVar3_observer", "REUSED", startTime)
        return()
      } else {
        # Copy failed - log and continue with normal validation
        debugLog(paste("DUPLICATE_REUSE: Copy failed for var3, falling back to validation:", 
                       copy_result$error), "USER_OPERATION")
      }
    }
  }
  
  # Normal validation path (if not reused or reuse failed) - identical to VI1/VI2
  measureTime({
    performVariableValidation(input$independentVar3, 3, session, values, labels, input)
  }, "Var3 complete validation")
  
  # Store current variable name after validation
  values[["var3_current_variable"]] <- input$independentVar3
  
  traceObserver("independentVar3", input$independentVar3, "FINISHED")
  traceObserverTiming("independentVar3_observer", "FINISHED", startTime)
}, ignoreInit = TRUE)
