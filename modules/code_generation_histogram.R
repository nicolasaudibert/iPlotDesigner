# Histogram code generation with user operations integration
# Requires: code_generation_core.R

# Histogram code generation
generateHistogramCode <- function(input, values, labels = NULL) {
  if(is.null(labels)) labels <- list()
  
  # Pass values to header generation for user operations support
  code <- generateCodeHeader(input, labels, values)
  
  # Clean variable names to remove placeholders
  cleanVar2 <- cleanVariableForCodeGeneration(input$independentVar2, labels)
  cleanVar3 <- cleanVariableForCodeGeneration(input$independentVar3, labels)
  
  # Get all modalities for optimization checks
  var1_all <- if(!is.null(values$var1_modalities)) unique(values$var1_modalities) else NULL
  var3_all <- if(!is.null(values$var3_modalities)) unique(values$var3_modalities) else NULL
  
  # Add filtering and ordering (optimized) using cleaned variables
  code <- paste0(code, generateFilteringCode(input$independentVar1, values$var1_selected, var1_all, labels))
  if(!is.null(cleanVar3)) {
    code <- paste0(code, generateFilteringCode(cleanVar3, values$var3_selected, var3_all, labels))
  }
  
  code <- paste0(code, generateFactorOrderCode(input$independentVar1, values$var1_order, values$var1_selected, labels, values))
  if(!is.null(cleanVar3)) {
    code <- paste0(code, generateFactorOrderCode(cleanVar3, values$var3_order, values$var3_selected, labels, values))
  }
  
  # Create plot
  createHistogramComment <- safeGetUILabel("codeCreateHistogramComment", labels, "Create histogram")
  code <- paste0(code, "# ", createHistogramComment, "\n")
  
  code <- paste0(code,
                 "p <- ggplot(data, aes(x = ", escapeVarName(input$dependentVar), ")) +\n",
                 "  geom_histogram(aes(fill = ", escapeVarName(input$independentVar1), "), \n",
                 "                bins = ", input$histBins, ",\n",
                 "                alpha = ", input$histAlpha, ",\n",
                 "                position = '", input$histPosition, "') +\n"
  )
  
  # Add color scale
  code <- paste0(code, generateColorScaleCode(input$independentVar1, values$var1_colors, values$var1_selected), " +\n")
  
  # Add density overlay if requested
  if(input$histDensityOverlay) {
    code <- paste0(code,
                   "  geom_density(aes(y = after_stat(count) * ", input$histBins, " / diff(range(", escapeVarName(input$dependentVar), ")), \n",
                   "                  color = ", escapeVarName(input$independentVar1), "), \n",
                   "              size = 1.2) +\n",
                   generateColorScaleCode(input$independentVar1, values$var1_colors, values$var1_selected, fill = FALSE), " +\n"
    )
  }
  
  countLabel <- safeGetUILabel("codeCountLabel", labels, "Count")
  code <- paste0(code,
                 "  labs(x = '", escapeVariableNameForLabels(input$dependentVar), "', y = '", countLabel, "') +\n",
                 generateThemeCode(input, labels)
  )
  
  # Add facets with corrected logic
  facet_code <- generateFacetCode(cleanVar2, cleanVar3, labels)
  if(!is.null(facet_code)) {
    code <- paste0(code, " +\n", facet_code)
  }
  
  code <- paste0(code, generateSaveCode("histogram", input, labels, list(
	  dependent_var = input$dependentVar,
	  independent_var1 = input$independentVar1,
	  independent_var2 = cleanVar2,
	  independent_var3 = cleanVar3
	))
  )
  
  return(code)
}
