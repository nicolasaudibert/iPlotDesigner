# Ridge plot code generation with user operations integration
# Requires: code_generation_core.R

# Ridge plot code generation with localization and optimization for user operations
generateRidgePlotCode <- function(input, values, labels = NULL) {
  if(is.null(labels)) labels <- list()
  
  # Pass values to header generation for user operations support
  code <- generateCodeHeader(input, labels, values, needsGgridges = TRUE)
  
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
  
  # Add subgroup filtering for ridge plot calculation
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
  createRidgePlotComment <- safeGetUILabel("codeCreateRidgePlotComment", labels, "Create ridge plot")
  code <- paste0(code, "# ", createRidgePlotComment, "\n")
  
  if(input$ridgeType == "density") {
    code <- paste0(code,
      "p <- ggplot(data, aes(x = ", escapeVarName(input$dependentVar), ", y = ", escapeVarName(input$independentVar1), ", fill = ", escapeVarName(input$independentVar1), ")) +\n",
      "  geom_density_ridges(\n",
      "    scale = ", input$ridgeScale, ", \n",
      "    alpha = ", input$ridgeAlpha
    )
    
    if(input$ridgeShowQuantiles) {
      code <- paste0(code, ",\n    quantile_lines = TRUE,\n    quantiles = c(0.25, 0.5, 0.75)")
    }
    
    code <- paste0(code, "\n  ) +\n")
  } else {
    code <- paste0(code,
      "p <- ggplot(data, aes(x = ", escapeVarName(input$dependentVar), ", y = ", escapeVarName(input$independentVar1), ", fill = ", escapeVarName(input$independentVar1), ")) +\n",
      "  geom_density_ridges(\n",
      "    stat = 'binline', \n",
      "    bins = ", input$histBins, ", \n",
      "    scale = ", input$ridgeScale, ", \n",
      "    alpha = ", input$ridgeAlpha, "\n",
      "  ) +\n"
    )
  }
  
  # Add color scale
  code <- paste0(code, generateColorScaleCode(input$independentVar1, values$var1_colors, values$var1_selected), " +\n")
  
  # Add labels and theme
  code <- paste0(code,
                 "  labs(x = '", escapeVariableNameForLabels(input$dependentVar), "', y = '", escapeVariableNameForLabels(input$independentVar1), "') +\n",
                 generateThemeCode(input, labels)
  )
  
  # Add facets using cleaned variables (handle both var2 and var3)
  if(!is.null(cleanVar2) && !is.null(cleanVar3)) {
    # Both variables: use facet_grid
    code <- paste0(code, " +\n  facet_grid(", escapeVarName(cleanVar3), " ~ ", escapeVarName(cleanVar2), ", scales = 'free_x')")
  } else if(!is.null(cleanVar2)) {
    # Only var2: use facet_wrap
    code <- paste0(code, " +\n  facet_wrap(~ ", escapeVarName(cleanVar2), ", scales = 'free_x')")
  } else if(!is.null(cleanVar3)) {
    # Only var3: use facet_wrap
    code <- paste0(code, " +\n  facet_wrap(~ ", escapeVarName(cleanVar3), ", scales = 'free_x')")
  }
  
  # Add save code using base filename without extension
  code <- paste0(code, generateSaveCode("ridge_plot", input, labels, list(
	  dependent_var = input$dependentVar,
	  independent_var1 = input$independentVar1,
	  independent_var2 = cleanVar2,
	  independent_var3 = cleanVar3
	))
  )
  
  return(code)
}
