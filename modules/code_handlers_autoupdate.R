# Code Handlers Auto-Update Module
# Automatic code updates when variable properties change
# Enhanced with synchronized advanced options

# ============================================================================
# Main property change observer
# ============================================================================

# Observer for variable property changes that affect code generation
observe({
  # Monitor all variable properties that affect code generation
  var1_selected <- values$var1_selected
  var1_colors <- values$var1_colors  
  var1_order <- values$var1_order
  
  var2_selected <- values$var2_selected
  var2_colors <- values$var2_colors
  var2_order <- values$var2_order
  
  var3_selected <- values$var3_selected
  var3_order <- values$var3_order
  
  # Also monitor main variable selections that affect code structure
  dep_var <- input$dependentVar
  ind_var1 <- input$independentVar1
  ind_var2 <- input$independentVar2
  ind_var3 <- input$independentVar3
  
  # Monitor plot settings that affect code
  text_size <- input$textSize
  font_family <- input$defaultFont
  plot_bg <- input$plotBackgroundColor
  panel_bg <- input$panelBackgroundColor
  plot_transparent <- input$plotBackgroundTransparent
  panel_transparent <- input$panelBackgroundTransparent
  
  # Only update if we have valid data and at least one variable
  if(!is.null(dep_var) && dep_var != "" && 
     !is.null(ind_var1) && ind_var1 != "" &&
     !is.null(values$filedata)) {
    
    # Update all visible code editors
    updateAllVisibleCodeEditors()
  }
})

# ============================================================================
# Plot-specific settings observers
# ============================================================================

# Plot-specific settings observers
observe({
  # Bar plot settings
  bar_error_type <- input$barErrorType
  bar_error_width <- input$barErrorWidth
  bar_alpha <- input$barAlpha
  bar_show_labels <- input$barShowDataLabels
  
  # Update bar code if visible
  if(!is.null(input$barShowCode) && input$barShowCode && 
     !is.null(input$dependentVar) && input$dependentVar != "" &&
     !is.null(input$independentVar1) && input$independentVar1 != "") {
    code <- generateBarPlotCode(input, values, labels)
    updateAceEditor(session, "barPlotCode", value = code)
  }
})

observe({
  # Box plot settings
  box_alpha <- input$boxAlpha
  box_show_points <- input$boxShowPoints
  box_point_size <- input$boxPointSize
  box_show_mean <- input$boxShowMean
  
  # Update box code if visible
  if(!is.null(input$boxShowCode) && input$boxShowCode && 
     !is.null(input$dependentVar) && input$dependentVar != "" &&
     !is.null(input$independentVar1) && input$independentVar1 != "") {
    code <- generateBoxPlotCode(input, values, labels)
    updateAceEditor(session, "boxPlotCode", value = code)
  }
})

observe({
  # Violin plot settings
  violin_alpha <- input$violinAlpha
  violin_show_box <- input$violinShowBox
  violin_show_points <- input$violinShowPoints
  violin_trim <- input$violinTrim
  
  # Update violin code if visible
  if(!is.null(input$violinShowCode) && input$violinShowCode && 
     !is.null(input$dependentVar) && input$dependentVar != "" &&
     !is.null(input$independentVar1) && input$independentVar1 != "") {
    code <- generateViolinPlotCode(input, values, labels)
    updateAceEditor(session, "violinPlotCode", value = code)
  }
})

observe({
  # Histogram settings
  hist_bins <- input$histBins
  hist_alpha <- input$histAlpha
  hist_position <- input$histPosition
  hist_density_overlay <- input$histDensityOverlay
  
  # Update histogram code if visible
  if(!is.null(input$histShowCode) && input$histShowCode && 
     !is.null(input$dependentVar) && input$dependentVar != "" &&
     !is.null(input$independentVar1) && input$independentVar1 != "") {
    code <- generateHistogramCode(input, values, labels)
    updateAceEditor(session, "histPlotCode", value = code)
  }
})

observe({
  # Density plot settings
  density_alpha <- input$densityAlpha
  density_line_size <- input$densityLineSize
  density_fill <- input$densityFill
  density_show_rug <- input$densityShowRug
  
  # Update density code if visible
  if(!is.null(input$densityShowCode) && input$densityShowCode && 
     !is.null(input$dependentVar) && input$dependentVar != "" &&
     !is.null(input$independentVar1) && input$independentVar1 != "") {
    code <- generateDensityPlotCode(input, values, labels)
    updateAceEditor(session, "densityPlotCode", value = code)
  }
})

observe({
  # Ridge plot settings
  ridge_scale <- input$ridgeScale
  ridge_alpha <- input$ridgeAlpha
  ridge_type <- input$ridgeType
  ridge_show_quantiles <- input$ridgeShowQuantiles
  
  # Update ridge code if visible
  if(!is.null(input$ridgeShowCode) && input$ridgeShowCode && 
     !is.null(input$dependentVar) && input$dependentVar != "" &&
     !is.null(input$independentVar1) && input$independentVar1 != "") {
    code <- generateRidgePlotCode(input, values, labels)
    updateAceEditor(session, "ridgePlotCode", value = code)
  }
})

# ============================================================================
# Enhanced code generation options observers - synchronized across plot types
# ============================================================================

# List of plot types that have advanced options
PLOT_TYPES_WITH_OPTIONS <- c("bar", "box", "violin", "hist", "density", "ridge")

# Function to update all plot type options when one changes
updateAllPlotTypeOptions <- function(option_name, new_value) {
  for (plot_type in PLOT_TYPES_WITH_OPTIONS) {
    input_id <- paste0(option_name, "_", plot_type)
    tryCatch({
      updateCheckboxInput(session, input_id, value = new_value)
    }, error = function(e) {
      # Silent error - option might not exist for all plot types yet
    })
  }
}

# Observer for metadata inclusion setting (synchronized across plot types)
observe({
  for (plot_type in PLOT_TYPES_WITH_OPTIONS) {
    local({
      current_plot_type <- plot_type
      input_id <- paste0("includeOperationsMetadata_", current_plot_type)
      
      observeEvent(input[[input_id]], {
        if(!is.null(input[[input_id]])) {
          ENHANCED_CODE_CONFIG$include_session_info <<- input[[input_id]]
          debugLog(paste("CODE_CONFIG: Session info set to", input[[input_id]], "from", current_plot_type), "CODE_CONFIG")
          
          # Synchronize with other plot types
          updateAllPlotTypeOptions("includeOperationsMetadata", input[[input_id]])
          
          # Trigger code regeneration for all visible code editors
          updateAllVisibleCodeEditors()
        }
      }, ignoreInit = TRUE)
    })
  }
})

# Observer for timestamps inclusion setting (synchronized)
observe({
  for (plot_type in PLOT_TYPES_WITH_OPTIONS) {
    local({
      current_plot_type <- plot_type
      input_id <- paste0("includeOperationsTimestamps_", current_plot_type)
      
      observeEvent(input[[input_id]], {
        if(!is.null(input[[input_id]])) {
          ENHANCED_CODE_CONFIG$include_operation_timestamps <<- input[[input_id]]
          debugLog(paste("CODE_CONFIG: Operation timestamps set to", input[[input_id]], "from", current_plot_type), "CODE_CONFIG")
          
          # Synchronize with other plot types
          updateAllPlotTypeOptions("includeOperationsTimestamps", input[[input_id]])
          
          # Trigger code regeneration for all visible code editors
          updateAllVisibleCodeEditors()
        }
      }, ignoreInit = TRUE)
    })
  }
})

# Observer for platform info inclusion setting (synchronized)
observe({
  for (plot_type in PLOT_TYPES_WITH_OPTIONS) {
    local({
      current_plot_type <- plot_type
      input_id <- paste0("includePlatformInfo_", current_plot_type)
      
      observeEvent(input[[input_id]], {
        if(!is.null(input[[input_id]])) {
          ENHANCED_CODE_CONFIG$include_r_version <<- input[[input_id]]
          ENHANCED_CODE_CONFIG$include_platform_info <<- input[[input_id]]
          debugLog(paste("CODE_CONFIG: Platform info set to", input[[input_id]], "from", current_plot_type), "CODE_CONFIG")
          
          # Synchronize with other plot types
          updateAllPlotTypeOptions("includePlatformInfo", input[[input_id]])
          
          # Trigger code regeneration for all visible code editors
          updateAllVisibleCodeEditors()
        }
      }, ignoreInit = TRUE)
    })
  }
})

# Observer for error handling setting (synchronized)
observe({
  for (plot_type in PLOT_TYPES_WITH_OPTIONS) {
    local({
      current_plot_type <- plot_type
      input_id <- paste0("includeErrorHandling_", current_plot_type)
      
      observeEvent(input[[input_id]], {
        if(!is.null(input[[input_id]])) {
          ENHANCED_CODE_CONFIG$include_error_handling <<- input[[input_id]]
          debugLog(paste("CODE_CONFIG: Error handling set to", input[[input_id]], "from", current_plot_type), "CODE_CONFIG")
          
          # Synchronize with other plot types
          updateAllPlotTypeOptions("includeErrorHandling", input[[input_id]])
          
          # Trigger code regeneration for all visible code editors
          updateAllVisibleCodeEditors()
        }
      }, ignoreInit = TRUE)
    })
  }
})

# Observer for verification steps setting (synchronized)
observe({
  for (plot_type in PLOT_TYPES_WITH_OPTIONS) {
    local({
      current_plot_type <- plot_type
      input_id <- paste0("includeVerificationSteps_", current_plot_type)
      
      observeEvent(input[[input_id]], {
        if(!is.null(input[[input_id]])) {
          ENHANCED_CODE_CONFIG$include_verification_steps <<- input[[input_id]]
          debugLog(paste("CODE_CONFIG: Verification steps set to", input[[input_id]], "from", current_plot_type), "CODE_CONFIG")
          
          # Synchronize with other plot types
          updateAllPlotTypeOptions("includeVerificationSteps", input[[input_id]])
          
          # Trigger code regeneration for all visible code editors
          updateAllVisibleCodeEditors()
        }
      }, ignoreInit = TRUE)
    })
  }
})

# Observer for performance optimization setting (synchronized)
observe({
  for (plot_type in PLOT_TYPES_WITH_OPTIONS) {
    local({
      current_plot_type <- plot_type
      input_id <- paste0("usePerformanceOptimization_", current_plot_type)
      
      observeEvent(input[[input_id]], {
        if(!is.null(input[[input_id]])) {
          ENHANCED_CODE_CONFIG$use_performance_optimization <<- input[[input_id]]
          debugLog(paste("CODE_CONFIG: Performance optimization set to", input[[input_id]], "from", current_plot_type), "CODE_CONFIG")
          
          # Synchronize with other plot types
          updateAllPlotTypeOptions("usePerformanceOptimization", input[[input_id]])
          
          # Trigger code regeneration for all visible code editors
          updateAllVisibleCodeEditors()
        }
      }, ignoreInit = TRUE)
    })
  }
})

# ============================================================================
# Helper function for code regeneration
# ============================================================================

# Enhanced function to update all visible code editors when configuration changes
updateAllVisibleCodeEditors <- function() {
  
  # Only update if we have valid data and at least one variable
  if(!is.null(input$dependentVar) && input$dependentVar != "" && 
     !is.null(input$independentVar1) && input$independentVar1 != "" &&
     !is.null(values$filedata)) {
    
    tryCatch({
      # Update each plot type's code if it's currently visible
      
      # Bar plot
      if(!is.null(input$barShowCode) && input$barShowCode) {
        code <- generateBarPlotCode(input, values, labels)
        updateAceEditor(session, "barPlotCode", value = code)
      }
      
      # Box plot
      if(!is.null(input$boxShowCode) && input$boxShowCode) {
        code <- generateBoxPlotCode(input, values, labels)
        updateAceEditor(session, "boxPlotCode", value = code)
      }
      
      # Violin plot
      if(!is.null(input$violinShowCode) && input$violinShowCode) {
        code <- generateViolinPlotCode(input, values, labels)
        updateAceEditor(session, "violinPlotCode", value = code)
      }
      
      # Histogram
      if(!is.null(input$histShowCode) && input$histShowCode) {
        code <- generateHistogramCode(input, values, labels)
        updateAceEditor(session, "histPlotCode", value = code)
      }
      
      # Density plot
      if(!is.null(input$densityShowCode) && input$densityShowCode) {
        code <- generateDensityPlotCode(input, values, labels)
        updateAceEditor(session, "densityPlotCode", value = code)
      }
      
      # Ridge plot
      if(!is.null(input$ridgeShowCode) && input$ridgeShowCode) {
        code <- generateRidgePlotCode(input, values, labels)
        updateAceEditor(session, "ridgePlotCode", value = code)
      }
      
      debugLog("CODE_CONFIG: All visible code editors updated", "CODE_CONFIG")
      
    }, error = function(e) {
      debugLog(paste("CODE_CONFIG: Error updating code editors:", e$message), "CODE_CONFIG")
    })
  }
}

debugLog("CODE_HANDLERS_AUTOUPDATE: Enhanced auto-update code handlers with synchronized options loaded", "CODE_INIT")
