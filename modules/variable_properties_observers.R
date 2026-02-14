# Variable properties observers module
# Reactive observers for property updates with real-time synchronization
# Handles source-to-copy propagation for duplicate variable selections

# Variable 1 property updates observer with synchronization
observe({
  current_var1 <- input$independentVar1
  
  if(!is.null(current_var1) && current_var1 != "") {
    # Update properties from inputs (original behavior preserved)
    # This works for both sources and non-duplicated variables
    if (!isTRUE(values$var1_is_copy)) {
      values$var1_selected <- getSelectedModalities(1, values$var1_modalities, input)
      values$var1_colors <- getModalityColors(1, values$var1_modalities, input, values$var1_colors)
      
      # Propagate to copies if this is a source
      for (copy_var_num in 2:3) {
        is_copy <- values[[paste0("var", copy_var_num, "_is_copy")]]
        source_var <- values[[paste0("var", copy_var_num, "_source_var")]]
        
        if (isTRUE(is_copy) && !is.null(source_var) && source_var == 1) {
          # Synchronize properties: selected, colors, and current order
          values[[paste0("var", copy_var_num, "_selected")]] <- values$var1_selected
          values[[paste0("var", copy_var_num, "_colors")]] <- values$var1_colors
          values[[paste0("var", copy_var_num, "_order")]] <- values$var1_order
          
          debugLog(paste("SYNC: Propagated var1 properties to var", copy_var_num), 
                   "USER_OPERATION")
        }
      }
      
      debugLog("var1Properties - Properties updated via natural reactivity", "VAR_DEBUG")
    }
  }
})

# Variable 2 property updates observer with synchronization
observe({
  current_var2 <- input$independentVar2
  
  tryCatch({
    noVar2Label <- getCachedLabel("noIndependentVar2Label", labels, "-- No independent variable 2 --")
    if(!is.null(current_var2) && current_var2 != "" && current_var2 != noVar2Label) {
      # Update properties from inputs (original behavior preserved)
      if (!isTRUE(values$var2_is_copy)) {
        values$var2_selected <- getSelectedModalities(2, values$var2_modalities, input)
        values$var2_colors <- getModalityColors(2, values$var2_modalities, input, values$var2_colors)
        
        # Propagate to copies if this is a source
        for (copy_var_num in c(1, 3)) {
          is_copy <- values[[paste0("var", copy_var_num, "_is_copy")]]
          source_var <- values[[paste0("var", copy_var_num, "_source_var")]]
          
          if (isTRUE(is_copy) && !is.null(source_var) && source_var == 2) {
            # Synchronize properties: selected, colors, and current order
            values[[paste0("var", copy_var_num, "_selected")]] <- values$var2_selected
            values[[paste0("var", copy_var_num, "_colors")]] <- values$var2_colors
            values[[paste0("var", copy_var_num, "_order")]] <- values$var2_order
            
            debugLog(paste("SYNC: Propagated var2 properties to var", copy_var_num), 
                     "USER_OPERATION")
          }
        }
        
        debugLog("var2Properties - Properties updated via natural reactivity", "VAR_DEBUG")
      }
    }
  }, error = function(e) {
    debugLog(paste("Error in var2 reactive observe:", e$message), "OBSERVER_TRACE")
  })
})

# Variable 3 property updates observer with synchronization
observe({
  current_var3 <- input$independentVar3
  
  tryCatch({
    noVar3Label <- getCachedLabel("noIndependentVar3Label", labels, "-- No independent variable 3 --")
    if(!is.null(current_var3) && current_var3 != "" && current_var3 != noVar3Label) {
      # Update properties from inputs (original behavior preserved)
      if (!isTRUE(values$var3_is_copy)) {
        values$var3_selected <- getSelectedModalities(3, values$var3_modalities, input)
        values$var3_colors <- getModalityColors(3, values$var3_modalities, input, values$var3_colors)
        
        # Propagate to copies if this is a source
        for (copy_var_num in 1:2) {
          is_copy <- values[[paste0("var", copy_var_num, "_is_copy")]]
          source_var <- values[[paste0("var", copy_var_num, "_source_var")]]
          
          if (isTRUE(is_copy) && !is.null(source_var) && source_var == 3) {
            # Synchronize properties: selected, colors, and current order
            values[[paste0("var", copy_var_num, "_selected")]] <- values$var3_selected
            values[[paste0("var", copy_var_num, "_colors")]] <- values$var3_colors
            values[[paste0("var", copy_var_num, "_order")]] <- values$var3_order
            
            debugLog(paste("SYNC: Propagated var3 properties to var", copy_var_num), 
                     "USER_OPERATION")
          }
        }
        
        debugLog("var3Properties - Properties updated via natural reactivity", "VAR_DEBUG")
      }
    }
  }, error = function(e) {
    debugLog(paste("Error in var3 reactive observe:", e$message), "OBSERVER_TRACE")
  })
})

debugLog("VARIABLE_PROPERTIES_OBSERVERS: Property observers loaded with real-time synchronization", "VAR_INIT")
