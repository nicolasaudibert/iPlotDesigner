# Violin plot code generation with conditional fill

# Violin plot code generation
generateViolinPlotCode <- function(input, values, labels = NULL) {
  if(is.null(labels)) labels <- list()
  
  code <- generateCodeHeader(input, labels, values)
  
  # Clean variable names to remove placeholders
  cleanVar2 <- cleanVariableForCodeGeneration(input$independentVar2, labels)
  cleanVar3 <- cleanVariableForCodeGeneration(input$independentVar3, labels)
  
  # Get all modalities for optimization checks
  var1_all <- if(!is.null(values$var1_modalities)) unique(values$var1_modalities) else NULL
  var2_all <- if(!is.null(values$var2_modalities)) unique(values$var2_modalities) else NULL
  var3_all <- if(!is.null(values$var3_modalities)) unique(values$var3_modalities) else NULL
  
  # Add filtering and ordering using cleaned variables
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
  createViolinPlotComment <- safeGetUILabel("codeCreateViolinPlotComment", labels, "Create violin plot")
  code <- paste0(code, "# ", createViolinPlotComment, "\n")
  
  # Determine if fill aesthetic should be used
  use_fill <- !is.null(cleanVar2)
  
  # Generate violin plot code with proper alignment when boxplot is shown
  if(use_fill) {
    # Use Var2 for fill when available
    violin_params <- paste0("alpha = ", input$violinAlpha, ", trim = ", tolower(input$violinTrim))
    if(input$violinShowBox) {
      violin_params <- paste0(violin_params, ", position = position_dodge(width = 0.9), drop = FALSE")
    }
    
    code <- paste0(code,
                   "p <- ggplot(data, aes(x = ", escapeVarName(input$independentVar1), 
                   ", y = ", escapeVarName(input$dependentVar), ", fill = ", escapeVarName(cleanVar2), ")) +\n",
                   "  geom_violin(", violin_params, ") +\n"
    )
    
    code <- paste0(code, generateColorScaleCode(cleanVar2, values$var2_colors, values$var2_selected), " +\n")
  } else {
    # No fill aesthetic, use uniform color
    violin_params <- paste0("fill = 'steelblue', alpha = ", input$violinAlpha, ", trim = ", tolower(input$violinTrim))
    if(input$violinShowBox) {
      violin_params <- paste0(violin_params, ", position = position_dodge(width = 0.9), drop = FALSE")
    }
    
    code <- paste0(code,
                   "p <- ggplot(data, aes(x = ", escapeVarName(input$independentVar1), 
                   ", y = ", escapeVarName(input$dependentVar), ")) +\n",
                   "  geom_violin(", violin_params, ") +\n"
    )
  }
  
  # Add optional boxplot overlay with conditional fill
  if(input$violinShowBox) {
    if(use_fill) {
      # Boxplot with Var2 fill for consistency
      code <- paste0(code, 
                     "  geom_boxplot(aes(fill = ", escapeVarName(cleanVar2), "), width = 0.1, alpha = 0.5, position = position_dodge(width = 0.9), outliers = ", 
                     if(input$violinShowPoints) "FALSE" else "TRUE", ") +\n")
    } else {
      # White boxplot for uniform appearance
      code <- paste0(code, 
                     "  geom_boxplot(width = 0.1, fill = 'white', alpha = 0.5, position = position_dodge(width = 0.9), outliers = ", 
                     if(input$violinShowPoints) "FALSE" else "TRUE", ") +\n")
    }
  }
  
  # Add optional individual points with conditional positioning
  if(input$violinShowPoints) {
    if(use_fill) {
      # Use geom_point with position_jitterdodge for grouped data
      code <- paste0(code,
                     "  geom_point(aes(color = ", escapeVarName(cleanVar2), "), size = 1, alpha = 0.3, position = position_jitterdodge(dodge.width = 0.9)) +\n",
                     generateColorScaleCode(cleanVar2, values$var2_colors, values$var2_selected, fill = FALSE), " +\n",
                     "  guides(color = 'none') +\n")
    } else {
      # Simple jitter for ungrouped data
      code <- paste0(code, "  geom_jitter(width = 0.2, size = 1, alpha = 0.3, color = 'gray30') +\n")
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
  
  code <- paste0(code, generateSaveCode("violin_plot", input, labels, list(
    dependent_var = input$dependentVar,
    independent_var1 = input$independentVar1,
    independent_var2 = cleanVar2,
    independent_var3 = cleanVar3
  )))
  
  return(code)
}

debugLog("CODE_GEN_VIOLIN: Violin plot code generation functions loaded", "CODE_INIT")
