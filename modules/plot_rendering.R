# Plot rendering logic with validation for performance
# Prevents plot generation with invalid variables

# Helper function to validate all variables before plot generation
validate_all_variables_for_plotting <- function(input, values, labels) {
  # Check required variables
  if(is.null(values$filteredData) || is.null(input$dependentVar) || is.null(input$independentVar1)) {
    return(list(valid = FALSE, reason = "Missing required data or variables"))
  }
  
  # Validate VI1 (always required)
  var1_validation <- validateGroupingVariable(values$filteredData, input$independentVar1)
  if(!var1_validation$isValid) {
    return(list(valid = FALSE, reason = paste("VI1 invalid:", var1_validation$reason)))
  }
  
  # Get clean variable names for VI2 and VI3
  cleanVars <- getCleanVariables(input, labels)
  
  # Validate VI2 if present
  if(!is.null(cleanVars$indVar2) && cleanVars$indVar2 != "") {
    var2_validation <- validateGroupingVariable(values$filteredData, cleanVars$indVar2)
    if(!var2_validation$isValid) {
      debugLog(paste("PLOT_VALIDATION: VI2", cleanVars$indVar2, "invalid, skipping plot generation"), "PERFORMANCE_OPTIMIZATION")
      return(list(valid = FALSE, reason = paste("VI2 invalid:", var2_validation$reason)))
    }
  }
  
  # Validate VI3 if present - CRITICAL FOR PERFORMANCE
  if(!is.null(cleanVars$indVar3) && cleanVars$indVar3 != "") {
    var3_validation <- validateGroupingVariable(values$filteredData, cleanVars$indVar3)
    if(!var3_validation$isValid) {
      debugLog(paste("PLOT_VALIDATION: VI3", cleanVars$indVar3, "invalid, preventing plot generation"), "PERFORMANCE_OPTIMIZATION")
      return(list(valid = FALSE, reason = paste("VI3 invalid:", var3_validation$reason)))
    }
  }
  
  return(list(valid = TRUE, reason = "All variables valid"))
}

# Bar plot with validation
output$barPlot <- renderPlot({
  req(values$filteredData, input$dependentVar, input$independentVar1)
  
  # Check if plot generation is paused during filtering
  if(!is.null(values[["plot_generation_paused"]]) && values[["plot_generation_paused"]]) {
    debugLog("PLOT_PROTECTION: Bar plot generation paused during filtering", "PLOT_PROTECT")
    return(NULL)
  }
  
  # Critical validation - check all variables before plot generation
  validation <- validate_all_variables_for_plotting(input, values, labels)
  if(!validation$valid) {
    debugLog(paste("PLOT_SKIP: Bar plot generation skipped -", validation$reason), "PERFORMANCE_OPTIMIZATION")
    return(NULL)  # Return empty plot instead of processing invalid variables
  }
  
  debugLog("PLOT_GENERATION: Bar plot validation passed, generating plot", "PERFORMANCE_OPTIMIZATION")
  
  # Get clean variable names
  cleanVars <- getCleanVariables(input, labels)
  
  values$currentPlot <- createBarPlot(
    values$filteredData,
    input$dependentVar,
    cleanVars$indVar1,
    cleanVars$indVar2,
    cleanVars$indVar3,
    values$var1_selected,
    values$var1_colors,
    values$var1_order,
    values$var2_selected,
    values$var2_colors,
    values$var2_order,
    values$var3_selected,
    values$var3_order,
    ifelse(is.null(input$barType), "mean", input$barType),
    ifelse(is.null(input$barErrorType), "se", input$barErrorType),
    ifelse(is.null(input$barErrorWidth), defaultErrorBarWidth, input$barErrorWidth),
    ifelse(is.null(input$barAlpha), defaultAlpha, input$barAlpha),
    ifelse(is.null(input$barShowDataLabels), FALSE, input$barShowDataLabels),
    getPlotTheme(),
    labels
  )
  
  values$currentPlot
})

# Boxplot with validation
output$boxPlot <- renderPlot({
  req(values$filteredData, input$dependentVar, input$independentVar1)
  
  # PROTECTION: Check if plot generation is paused during filtering
  if(!is.null(values[["plot_generation_paused"]]) && values[["plot_generation_paused"]]) {
    debugLog("PLOT_PROTECTION: Box plot generation paused during filtering", "PLOT_PROTECT")
    return(NULL)
  }
  
  # CRITICAL VALIDATION: Check all variables before plot generation
  validation <- validate_all_variables_for_plotting(input, values, labels)
  if(!validation$valid) {
    debugLog(paste("PLOT_SKIP: Box plot generation skipped -", validation$reason), "PERFORMANCE_OPTIMIZATION")
    return(NULL)
  }
  
  debugLog("PLOT_GENERATION: Box plot validation passed, generating plot", "PERFORMANCE_OPTIMIZATION")
  
  # Get clean variable names
  cleanVars <- getCleanVariables(input, labels)
  
  values$currentPlot <- createBoxPlot(
    values$filteredData,
    input$dependentVar,
    cleanVars$indVar1,
    cleanVars$indVar2,
    cleanVars$indVar3,
    values$var1_selected,
    values$var1_colors,
    values$var1_order,
    values$var2_selected,
    values$var2_colors,
    values$var2_order,
    values$var3_selected,
    values$var3_order,
    ifelse(is.null(input$boxAlpha), defaultAlpha, input$boxAlpha),
    ifelse(is.null(input$boxShowPoints), FALSE, input$boxShowPoints),
    ifelse(is.null(input$boxPointSize), defaultPointSize, input$boxPointSize),
    ifelse(is.null(input$boxShowMean), FALSE, input$boxShowMean),
    getPlotTheme()
  )
  
  values$currentPlot
})

# Violin plot with validation
output$violinPlot <- renderPlot({
  req(values$filteredData, input$dependentVar, input$independentVar1)
  
  # PROTECTION: Check if plot generation is paused during filtering
  if(!is.null(values[["plot_generation_paused"]]) && values[["plot_generation_paused"]]) {
    debugLog("PLOT_PROTECTION: Violin plot generation paused during filtering", "PLOT_PROTECT")
    return(NULL)
  }
  
  # CRITICAL VALIDATION: Check all variables before plot generation
  validation <- validate_all_variables_for_plotting(input, values, labels)
  if(!validation$valid) {
    debugLog(paste("PLOT_SKIP: Violin plot generation skipped -", validation$reason), "PERFORMANCE_OPTIMIZATION")
    return(NULL)
  }
  
  debugLog("PLOT_GENERATION: Violin plot validation passed, generating plot", "PERFORMANCE_OPTIMIZATION")
  
  # Get clean variable names
  cleanVars <- getCleanVariables(input, labels)
  
  values$currentPlot <- createViolinPlot(
    values$filteredData,
    input$dependentVar,
    cleanVars$indVar1,
    cleanVars$indVar2,
    cleanVars$indVar3,
    values$var1_selected,
    values$var1_colors,
    values$var1_order,
    values$var2_selected,
    values$var2_colors,
    values$var2_order,
    values$var3_selected,
    values$var3_order,
    ifelse(is.null(input$violinAlpha), defaultAlpha, input$violinAlpha),
    ifelse(is.null(input$violinShowBox), TRUE, input$violinShowBox),
    ifelse(is.null(input$violinShowPoints), FALSE, input$violinShowPoints),
    ifelse(is.null(input$violinTrim), TRUE, input$violinTrim),
    getPlotTheme()
  )
  
  values$currentPlot
})

# Histogram with validation
output$histPlot <- renderPlot({
  req(values$filteredData, input$dependentVar, input$independentVar1)
  
  # PROTECTION: Check if plot generation is paused during filtering
  if(!is.null(values[["plot_generation_paused"]]) && values[["plot_generation_paused"]]) {
    debugLog("PLOT_PROTECTION: Histogram generation paused during filtering", "PLOT_PROTECT")
    return(NULL)
  }
  
  # CRITICAL VALIDATION: Check all variables before plot generation
  validation <- validate_all_variables_for_plotting(input, values, labels)
  if(!validation$valid) {
    debugLog(paste("PLOT_SKIP: Histogram generation skipped -", validation$reason), "PERFORMANCE_OPTIMIZATION")
    return(NULL)
  }
  
  debugLog("PLOT_GENERATION: Histogram validation passed, generating plot", "PERFORMANCE_OPTIMIZATION")
  
  # Get clean variable names
  cleanVars <- getCleanVariables(input, labels)
  
  values$currentPlot <- createHistogram(
    values$filteredData,
    input$dependentVar,
    cleanVars$indVar1,
    cleanVars$indVar2,
    cleanVars$indVar3,
    values$var1_selected,
    values$var1_colors,
    values$var1_order,
    values$var2_selected,
    values$var2_order,
    values$var3_selected,
    values$var3_order,
    ifelse(is.null(input$histBins), defaultBinNumber, input$histBins),
    ifelse(is.null(input$histAlpha), 0.5, input$histAlpha),
    ifelse(is.null(input$histPosition), "identity", input$histPosition),
    ifelse(is.null(input$histDensityOverlay), FALSE, input$histDensityOverlay),
    getPlotTheme()
  )
  
  values$currentPlot
})

# Density plot with validation
output$densityPlot <- renderPlot({
  req(values$filteredData, input$dependentVar, input$independentVar1)
  
  # PROTECTION: Check if plot generation is paused during filtering
  if(!is.null(values[["plot_generation_paused"]]) && values[["plot_generation_paused"]]) {
    debugLog("PLOT_PROTECTION: Density plot generation paused during filtering", "PLOT_PROTECT")
    return(NULL)
  }
  
  # CRITICAL VALIDATION: Check all variables before plot generation
  validation <- validate_all_variables_for_plotting(input, values, labels)
  if(!validation$valid) {
    debugLog(paste("PLOT_SKIP: Density plot generation skipped -", validation$reason), "PERFORMANCE_OPTIMIZATION")
    return(NULL)
  }
  
  debugLog("PLOT_GENERATION: Density plot validation passed, generating plot", "PERFORMANCE_OPTIMIZATION")
  
  # Get clean variable names
  cleanVars <- getCleanVariables(input, labels)
  
  values$currentPlot <- createDensityPlot(
    values$filteredData,
    input$dependentVar,
    cleanVars$indVar1,
    cleanVars$indVar2,
    cleanVars$indVar3,
    values$var1_selected,
    values$var1_colors,
    values$var1_order,
    values$var2_selected,
    values$var2_order,
    values$var3_selected,
    values$var3_order,
    ifelse(is.null(input$densityAlpha), 0.5, input$densityAlpha),
    ifelse(is.null(input$densityLineSize), defaultLineSize, input$densityLineSize),
    ifelse(is.null(input$densityFill), TRUE, input$densityFill),
    ifelse(is.null(input$densityShowRug), FALSE, input$densityShowRug),
    getPlotTheme()
  )
  
  values$currentPlot
})

# Ridge plot with validation
output$ridgePlot <- renderPlot({
  req(values$filteredData, input$dependentVar, input$independentVar1)
  
  # PROTECTION: Check if plot generation is paused during filtering
  if(!is.null(values[["plot_generation_paused"]]) && values[["plot_generation_paused"]]) {
    debugLog("PLOT_PROTECTION: Ridge plot generation paused during filtering", "PLOT_PROTECT")
    return(NULL)
  }
  
  # CRITICAL VALIDATION: Check all variables before plot generation
  validation <- validate_all_variables_for_plotting(input, values, labels)
  if(!validation$valid) {
    debugLog(paste("PLOT_SKIP: Ridge plot generation skipped -", validation$reason), "PERFORMANCE_OPTIMIZATION")
    return(NULL)
  }
  
  debugLog("PLOT_GENERATION: Ridge plot validation passed, generating plot", "PERFORMANCE_OPTIMIZATION")
  
  # Get clean variable names
  cleanVars <- getCleanVariables(input, labels)
  
  values$currentPlot <- createRidgePlot(
    values$filteredData,
    input$dependentVar,
    cleanVars$indVar1,
    cleanVars$indVar2,
    cleanVars$indVar3,
    values$var1_selected,
    values$var1_colors,
    values$var1_order,
    values$var2_selected,
    values$var2_order,
    values$var3_selected,
    values$var3_order,
    ifelse(is.null(input$ridgeScale), 1.5, input$ridgeScale),
    ifelse(is.null(input$ridgeAlpha), 0.7, input$ridgeAlpha),
    ifelse(is.null(input$ridgeType), "density", input$ridgeType),
    ifelse(is.null(input$histBins), defaultBinNumber, input$histBins),
    ifelse(is.null(input$ridgeShowQuantiles), FALSE, input$ridgeShowQuantiles),
    getPlotTheme()
  )
  
  values$currentPlot
})

# Download handlers with intelligent naming - bar plot
output$downloadBarPlot <- downloadHandler(
  filename = function() {
    ext <- availableOutputFormatExtensions[which(availableOutputFormats == input$exportFormat)]
    generateIntelligentFilename("bar_plot", input, ext, labels)
  },
  content = function(file) {
    ggsave(file, plot = values$currentPlot, width = input$exportWidth, 
           height = input$exportHeight, units = input$exportUnits)
  }
)

# Download handlers with intelligent naming - box plot
output$downloadBoxPlot <- downloadHandler(
  filename = function() {
    ext <- availableOutputFormatExtensions[which(availableOutputFormats == input$exportFormat)]
    generateIntelligentFilename("box_plot", input, ext, labels)
  },
  content = function(file) {
    ggsave(file, plot = values$currentPlot, width = input$exportWidth, 
           height = input$exportHeight, units = input$exportUnits)
  }
)

# Download handlers with intelligent naming - violin plot
output$downloadViolinPlot <- downloadHandler(
  filename = function() {
    ext <- availableOutputFormatExtensions[which(availableOutputFormats == input$exportFormat)]
    generateIntelligentFilename("violin_plot", input, ext, labels)
  },
  content = function(file) {
    ggsave(file, plot = values$currentPlot, width = input$exportWidth, 
           height = input$exportHeight, units = input$exportUnits)
  }
)

# Download handlers with intelligent naming - histogram
output$downloadHistPlot <- downloadHandler(
  filename = function() {
    ext <- availableOutputFormatExtensions[which(availableOutputFormats == input$exportFormat)]
    generateIntelligentFilename("histogram", input, ext, labels)
  },
  content = function(file) {
    ggsave(file, plot = values$currentPlot, width = input$exportWidth, 
           height = input$exportHeight, units = input$exportUnits)
  }
)

# Download handlers with intelligent naming - density plot
output$downloadDensityPlot <- downloadHandler(
  filename = function() {
    ext <- availableOutputFormatExtensions[which(availableOutputFormats == input$exportFormat)]
    generateIntelligentFilename("density_plot", input, ext, labels)
  },
  content = function(file) {
    ggsave(file, plot = values$currentPlot, width = input$exportWidth, 
           height = input$exportHeight, units = input$exportUnits)
  }
)

# Download handlers with intelligent naming - ridge plot
output$downloadRidgePlot <- downloadHandler(
  filename = function() {
    ext <- availableOutputFormatExtensions[which(availableOutputFormats == input$exportFormat)]
    generateIntelligentFilename("ridge_plot", input, ext, labels)
  },
  content = function(file) {
    ggsave(file, plot = values$currentPlot, width = input$exportWidth, 
           height = input$exportHeight, units = input$exportUnits)
  }
)
