# Density plot code generation - USER OPERATIONS INTEGRATION
# Requires: code_generation_core.R

# Density plot code generation
generateDensityPlotCode <- function(input, values, labels = NULL) {
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
  
  # Add subgroup filtering for density calculation
  filterSubgroupsComment <- safeGetUILabel("codeFilterSubgroupsComment", labels, 
    "Filter subgroups with insufficient data for density calculation")
  code <- paste0(code, "# ", filterSubgroupsComment, "\n")
  
  # Build grouping variables list
  group_vars <- paste0("'", input$independentVar1, "'")
  if(!is.null(cleanVar2)) {
    group_vars <- paste0(group_vars, ", '", cleanVar2, "'")
  }
  if(!is.null(cleanVar3)) {
    group_vars <- paste0(group_vars, ", '", cleanVar3, "'")
  }
  
  code <- paste0(code,
    "data <- data %>%\n",
    "  group_by(across(all_of(c(", group_vars, ")))) %>%\n",
    "  filter(n() > 1) %>%\n",
    "  ungroup()\n\n"
  )
  
  # Create plot
  createDensityPlotComment <- safeGetUILabel("codeCreateDensityPlotComment", labels, "Create density plot")
  code <- paste0(code, "# ", createDensityPlotComment, "\n")
  
  if(input$densityFill) {
    code <- paste0(code,
                   "p <- ggplot(data, aes(x = ", escapeVarName(input$dependentVar), ", fill = ", escapeVarName(input$independentVar1), ")) +\n",
                   "  geom_density(alpha = ", input$densityAlpha, ", linewidth = ", input$densityLineSize, ") +\n"
    )
    
    code <- paste0(code, generateColorScaleCode(input$independentVar1, values$var1_colors, values$var1_selected, fill = TRUE), " +\n")
  } else {
    code <- paste0(code,
                   "p <- ggplot(data, aes(x = ", escapeVarName(input$dependentVar), ", color = ", escapeVarName(input$independentVar1), ")) +\n",
                   "  geom_density(size = ", input$densityLineSize, ") +\n"
    )
    
    code <- paste0(code, generateColorScaleCode(input$independentVar1, values$var1_colors, values$var1_selected, fill = FALSE), " +\n")
  }
  
  if(input$densityShowRug) {
    code <- paste0(code,
                   "  geom_rug(aes(color = ", escapeVarName(input$independentVar1), "), alpha = 0.5) +\n"
    )
    
    if(input$densityFill) {
      code <- paste0(code, generateColorScaleCode(input$independentVar1, values$var1_colors, values$var1_selected, fill = FALSE), " +\n",
                     "  guides(color = 'none') +\n")
    }
  }
  
  densityLabel <- safeGetUILabel("codeDensityLabel", labels, "Density")
  code <- paste0(code,
                 "  labs(x = '", escapeVariableNameForLabels(input$dependentVar), "', y = '", densityLabel, "') +\n",
                 generateThemeCode(input, labels)
  )
  
  # Add facets with corrected logic
  facet_code <- generateFacetCode(cleanVar2, cleanVar3, labels)
  if(!is.null(facet_code)) {
    code <- paste0(code, " +\n", facet_code)
  }
  
  code <- paste0(code, generateSaveCode("density_plot", input, labels, list(
	  dependent_var = input$dependentVar,
	  independent_var1 = input$independentVar1,
	  independent_var2 = cleanVar2,
	  independent_var3 = cleanVar3
	))
  )
  
  return(code)
}
