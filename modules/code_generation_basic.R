# Basic plot code generation - Bar and Box plots

# Bar plot code generation with conditional fill
generateBarPlotCode <- function(input, values, labels = NULL) {
  if(is.null(labels)) labels <- list()
  
  code <- generateCodeHeader(input, labels, values)
  
  # Clean variable names to remove placeholders
  cleanVar2 <- cleanVariableForCodeGeneration(input$independentVar2, labels)
  cleanVar3 <- cleanVariableForCodeGeneration(input$independentVar3, labels)
  
  # Get all modalities for optimization checks
  var1_all <- if(!is.null(values$var1_modalities)) unique(values$var1_modalities) else NULL
  var2_all <- if(!is.null(values$var2_modalities)) unique(values$var2_modalities) else NULL
  var3_all <- if(!is.null(values$var3_modalities)) unique(values$var3_modalities) else NULL
  
  # Add filtering code using cleaned variables
  code <- paste0(code, generateFilteringCode(input$independentVar1, values$var1_selected, var1_all, labels))
  if(!is.null(cleanVar2)) {
    code <- paste0(code, generateFilteringCode(cleanVar2, values$var2_selected, var2_all, labels))
  }
  if(!is.null(cleanVar3)) {
    code <- paste0(code, generateFilteringCode(cleanVar3, values$var3_selected, var3_all, labels))
  }
  
  # Add factor ordering using cleaned variables
  code <- paste0(code, generateFactorOrderCode(input$independentVar1, values$var1_order, values$var1_selected, labels, values))
  if(!is.null(cleanVar2)) {
    code <- paste0(code, generateFactorOrderCode(cleanVar2, values$var2_order, values$var2_selected, labels, values))
  }
  if(!is.null(cleanVar3)) {
    code <- paste0(code, generateFactorOrderCode(cleanVar3, values$var3_order, values$var3_selected, labels, values))
  }
  
  # Calculate summary statistics using cleaned variables in group_by
  calculateSummaryComment <- safeGetUILabel("codeCalculateSummaryComment", labels, "Calculate summary statistics")
  
  groupVars <- escapeVarName(input$independentVar1)
  if(!is.null(cleanVar2)) {
    groupVars <- paste0(groupVars, ", ", escapeVarName(cleanVar2))
  }
  if(!is.null(cleanVar3)) {
    groupVars <- paste0(groupVars, ", ", escapeVarName(cleanVar3))
  }
  
  code <- paste0(code,
    "# ", calculateSummaryComment, "\n",
    "summary_data <- data %>%\n",
    "  group_by(", groupVars, ") %>%\n",
    "  summarise(\n",
    "    mean = mean(", escapeVarName(input$dependentVar), ", na.rm = TRUE),\n",
    "    se = sd(", escapeVarName(input$dependentVar), ", na.rm = TRUE) / sqrt(n()),\n",
    "    sd = sd(", escapeVarName(input$dependentVar), ", na.rm = TRUE),\n",
    "    n = n(),\n",
    "    .groups = 'drop'\n",
    "  )\n\n"
  )
  
  # Create plot with conditional fill
  createBarPlotComment <- safeGetUILabel("codeCreateBarPlotComment", labels, "Create bar plot")
  code <- paste0(code, "# ", createBarPlotComment, "\n")
  
  # Determine if fill aesthetic should be used
  use_fill <- !is.null(cleanVar2)
  
  if(use_fill) {
    # Use Var2 for fill when available
    code <- paste0(code,
      "p <- ggplot(summary_data, aes(x = ", escapeVarName(input$independentVar1), 
      ", y = mean, fill = ", escapeVarName(cleanVar2), ")) +\n",
      "  geom_col(position = 'dodge', alpha = ", input$barAlpha, ") +\n",
      "  geom_errorbar(aes(ymin = mean - ", input$barErrorType, ", ymax = mean + ", input$barErrorType, "),\n",
      "                position = position_dodge(0.9), width = ", input$barErrorWidth, ") +\n"
    )
    
    if(input$barShowDataLabels) {
      code <- paste0(code, "  geom_text(aes(label = round(mean, 2)), position = position_dodge(0.9), vjust = -0.5) +\n")
    }
    
    code <- paste0(code, generateColorScaleCode(cleanVar2, values$var2_colors, values$var2_selected), " +\n")
  } else {
    # No fill aesthetic, use uniform color
    code <- paste0(code,
      "p <- ggplot(summary_data, aes(x = ", escapeVarName(input$independentVar1), ", y = mean)) +\n",
      "  geom_col(fill = 'steelblue', alpha = ", input$barAlpha, ") +\n",
      "  geom_errorbar(aes(ymin = mean - ", input$barErrorType, ", ymax = mean + ", input$barErrorType, "),\n",
      "                width = ", input$barErrorWidth, ") +\n"
    )
    
    if(input$barShowDataLabels) {
      code <- paste0(code, "  geom_text(aes(label = round(mean, 2)), vjust = -0.5) +\n")
    }
  }
  
  # Get localized "Mean" label
  meanLabel <- safeGetUILabel("codeMeanLabel", labels, "Mean")
  
  # Add labels and theme
  code <- paste0(code,
                 "  labs(x = '", escapeVariableNameForLabels(input$independentVar1), "', y = '", meanLabel, " ", escapeVariableNameForLabels(input$dependentVar), "') +\n",
                 generateThemeCode(input, labels)
  )
  
  # Add facets with correct logic
  facet_code <- generateFacetCode(
    cleanVar2, 
    cleanVar3, 
    labels, 
    var2_used_for_fill = use_fill
  )
  if(!is.null(facet_code)) {
    code <- paste0(code, " +\n", facet_code)
  }
  
  # Add save code using base filename without extension
  code <- paste0(code, generateSaveCode("bar_plot", input, labels, list(
    dependent_var = input$dependentVar,
    independent_var1 = input$independentVar1,
    independent_var2 = cleanVar2,
    independent_var3 = cleanVar3
  )))
  
  return(code)
}

debugLog("CODE_GEN_BASIC: Bar and box plot code generation functions loaded", "CODE_INIT")

# Boxplot code generation with conditional fill
generateBoxPlotCode <- function(input, values, labels = NULL) {
  if(is.null(labels)) labels <- list()
  
  code <- generateCodeHeader(input, labels, values)
  
  # Clean variable names to remove placeholders
  cleanVar2 <- cleanVariableForCodeGeneration(input$independentVar2, labels)
  cleanVar3 <- cleanVariableForCodeGeneration(input$independentVar3, labels)
  
  # Get all modalities for optimization checks
  var1_all <- if(!is.null(values$var1_modalities)) unique(values$var1_modalities) else NULL
  var2_all <- if(!is.null(values$var2_modalities)) unique(values$var2_modalities) else NULL
  var3_all <- if(!is.null(values$var3_modalities)) unique(values$var3_modalities) else NULL
  
  # Add filtering and ordering
  code <- paste0(code, generateFilteringCode(input$independentVar1, values$var1_selected, var1_all, labels))
  if(!is.null(cleanVar2)) {
    code <- paste0(code, generateFilteringCode(cleanVar2, values$var2_selected, var2_all, labels))
  }
  if(!is.null(cleanVar3)) {
    code <- paste0(code, generateFilteringCode(cleanVar3, values$var3_selected, var3_all, labels))
  }
  
  code <- paste0(code, generateFactorOrderCode(input$independentVar1, values$var1_order, values$var1_selected, labels, values))
  if(!is.null(cleanVar2)) {
    code <- paste0(code, generateFactorOrderCode(cleanVar2, values$var2_order, values$var2_selected, labels, values))
  }
  if(!is.null(cleanVar3)) {
    code <- paste0(code, generateFactorOrderCode(cleanVar3, values$var3_order, values$var3_selected, labels, values))
  }
  
  # Create plot with conditional fill
  createBoxPlotComment <- safeGetUILabel("codeCreateBoxPlotComment", labels, "Create boxplot")
  code <- paste0(code, "# ", createBoxPlotComment, "\n")
  
  # Determine if fill aesthetic should be used
  use_fill <- !is.null(cleanVar2)
  
  if(use_fill) {
    # Use Var2 for fill when available
    code <- paste0(code,
      "p <- ggplot(data, aes(x = ", escapeVarName(input$independentVar1), 
      ", y = ", escapeVarName(input$dependentVar), ", fill = ", escapeVarName(cleanVar2), ")) +\n",
      "  geom_boxplot(alpha = ", input$boxAlpha, ") +\n"
    )
    
    if(input$boxShowPoints) {
      code <- paste0(code, "  geom_jitter(aes(color = ", escapeVarName(cleanVar2), 
                     "), width = 0.2, size = ", input$boxPointSize, ", alpha = 0.5) +\n")
    }
    
    if(input$boxShowMean) {
      code <- paste0(code, "  stat_summary(fun = mean, geom = 'point', shape = 23, size = 3, fill = 'red') +\n")
    }
    
    code <- paste0(code, generateColorScaleCode(cleanVar2, values$var2_colors, values$var2_selected), " +\n")
  } else {
    # No fill aesthetic, use uniform color
    code <- paste0(code,
      "p <- ggplot(data, aes(x = ", escapeVarName(input$independentVar1), 
      ", y = ", escapeVarName(input$dependentVar), ")) +\n",
      "  geom_boxplot(fill = 'steelblue', alpha = ", input$boxAlpha, ") +\n"
    )
    
    if(input$boxShowPoints) {
      code <- paste0(code, "  geom_jitter(width = 0.2, size = ", input$boxPointSize, 
                     ", alpha = 0.5, color = 'gray30') +\n")
    }
    
    if(input$boxShowMean) {
      code <- paste0(code, "  stat_summary(fun = mean, geom = 'point', shape = 23, size = 3, fill = 'red') +\n")
    }
  }
  
  # Add labels and theme
  code <- paste0(code,
                 "  labs(x = '", escapeVariableNameForLabels(input$independentVar1), "', y = '", escapeVariableNameForLabels(input$dependentVar), "') +\n",
                 generateThemeCode(input, labels)
  )
  
  # Add facets with correct logic
  facet_code <- generateFacetCode(
    cleanVar2, 
    cleanVar3, 
    labels, 
    var2_used_for_fill = use_fill
  )
  if(!is.null(facet_code)) {
    code <- paste0(code, " +\n", facet_code)
  }
  
  # Add save code
  code <- paste0(code, generateSaveCode("boxplot", input, labels, list(
    dependent_var = input$dependentVar,
    independent_var1 = input$independentVar1,
    independent_var2 = cleanVar2,
    independent_var3 = cleanVar3
  )))
  
  return(code)
}