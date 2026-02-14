# Plot Bar and Box Module
# Bar charts and box plots with conditional fill aesthetic

# Bar plot function with conditional fill based on Var2 presence
createBarPlot <- function(data, depVar, indVar1, indVar2, indVar3, 
                         var1Selected, var1Colors, var1Order,
                         var2Selected, var2Colors, var2Order,
                         var3Selected, var3Order,
                         barType, errorType, errorWidth, alpha, showLabels, theme, labels) {
  
  # Clean selected modalities to remove undefined values
  var1Selected <- cleanSelectedModalities(var1Selected, var1Order, data, indVar1)
  if (!is.null(indVar2) && indVar2 != "") {
    var2Selected <- cleanSelectedModalities(var2Selected, var2Order, data, indVar2)
  }
  if (!is.null(indVar3) && indVar3 != "") {
    var3Selected <- cleanSelectedModalities(var3Selected, var3Order, data, indVar3)
  }
  
  # Validate colors against selected modalities
  var1Colors <- validateColors(var1Colors, var1Selected)
  var2Colors <- validateColors(var2Colors, var2Selected)
  
  # Apply color fallbacks
  colorResults <- applyColorFallbacks(var1Colors, var2Colors, var1Selected, var2Selected)
  var1Colors <- colorResults$var1Colors
  var2Colors <- colorResults$var2Colors
  
  # Filter and reorder data
  data <- filterAndReorderData(data, indVar1, indVar2, indVar3,
                              var1Selected, var1Order,
                              var2Selected, var2Order,
                              var3Selected, var3Order)
  
  # Calculate summary statistics based on barType
  groupVars <- c(indVar1)
  if(!is.null(indVar2) && indVar2 != "") groupVars <- c(groupVars, indVar2)
  if(!is.null(indVar3) && indVar3 != "") groupVars <- c(groupVars, indVar3)
  
  if (barType == "count") {
    # Count mode: just count observations per group
    summary_data <- data %>%
      group_by(across(all_of(groupVars))) %>%
      summarise(
        value = n(),
        .groups = 'drop'
      )
    y_var <- "value"
    error_var <- NULL
  } else {
    # Mean mode: calculate mean and error bars
    summary_data <- data %>%
      group_by(across(all_of(groupVars))) %>%
      summarise(
        mean = mean(!!sym(depVar), na.rm = TRUE),
        se = sd(!!sym(depVar), na.rm = TRUE) / sqrt(n()),
        sd = sd(!!sym(depVar), na.rm = TRUE),
        n = n(),
        .groups = 'drop'
      )
    y_var <- "mean"
    error_var <- if(errorType == "se") "se" else "sd"
  }
  
  # Determine if fill aesthetic should be used
  use_fill <- !is.null(indVar2) && indVar2 != ""
  
  # Create plot with conditional fill
  if(use_fill) {
    # Use Var2 for fill when available
    p <- ggplot(summary_data, aes(x = !!sym(indVar1), y = !!sym(y_var), fill = !!sym(indVar2))) +
      geom_col(position = "dodge", alpha = alpha)
    
    # Add error bars only in mean mode
    if (barType == "mean" && !is.null(error_var)) {
      p <- p + geom_errorbar(aes(ymin = !!sym(y_var) - !!sym(error_var), 
                                 ymax = !!sym(y_var) + !!sym(error_var)),
                            position = position_dodge(0.9), width = errorWidth)
    }
    
    if(!is.null(var2Colors) && length(var2Colors) > 0) {
      p <- p + scale_fill_manual(values = var2Colors)
    }
  } else {
    # No fill aesthetic, use uniform color
    p <- ggplot(summary_data, aes(x = !!sym(indVar1), y = !!sym(y_var))) +
      geom_col(fill = "steelblue", alpha = alpha)
    
    # Add error bars only in mean mode
    if (barType == "mean" && !is.null(error_var)) {
      p <- p + geom_errorbar(aes(ymin = !!sym(y_var) - !!sym(error_var), 
                                 ymax = !!sym(y_var) + !!sym(error_var)),
                            width = errorWidth)
    }
  }
  
  if(showLabels) {
    if(use_fill) {
      p <- p + geom_text(aes(label = round(!!sym(y_var), 2)), 
                        position = position_dodge(0.9), 
                        vjust = -0.5)
    } else {
      p <- p + geom_text(aes(label = round(!!sym(y_var), 2)), vjust = -0.5)
    }
  }
  
  # Get localized labels for Y axis
  countLabel <- getCachedLabel("barTypeCountLabel", labels, "Count")
  meanLabel <- getCachedLabel("barTypeMeanLabel", labels, "Mean")
  
  p <- p + 
    labs(x = indVar1, 
         y = if (barType == "count") countLabel else paste(meanLabel, depVar)) +
    theme
  
  # Add facets if var3 is specified (var2 used for fill when present)
  addFacets(p, if(use_fill) NULL else indVar2, indVar3)
}

# Boxplot function with conditional fill based on Var2 presence
createBoxPlot <- function(data, depVar, indVar1, indVar2, indVar3,
                          var1Selected, var1Colors, var1Order,
                          var2Selected, var2Colors, var2Order,
                          var3Selected, var3Order,
                          alpha, showPoints, pointSize, showMean, theme) {
  
  # Clean selected modalities to remove undefined values
  var1Selected <- cleanSelectedModalities(var1Selected, var1Order, data, indVar1)
  if (!is.null(indVar2) && indVar2 != "") {
    var2Selected <- cleanSelectedModalities(var2Selected, var2Order, data, indVar2)
  }
  if (!is.null(indVar3) && indVar3 != "") {
    var3Selected <- cleanSelectedModalities(var3Selected, var3Order, data, indVar3)
  }
  
  # Validate colors against selected modalities
  var1Colors <- validateColors(var1Colors, var1Selected)
  var2Colors <- validateColors(var2Colors, var2Selected)
  
  # Apply color fallbacks
  colorResults <- applyColorFallbacks(var1Colors, var2Colors, var1Selected, var2Selected)
  var1Colors <- colorResults$var1Colors
  var2Colors <- colorResults$var2Colors
  
  # Filter and reorder data
  data <- filterAndReorderData(data, indVar1, indVar2, indVar3,
                               var1Selected, var1Order,
                               var2Selected, var2Order,
                               var3Selected, var3Order)
  
  # Determine if fill aesthetic should be used
  use_fill <- !is.null(indVar2) && indVar2 != ""
  
  # Create plot with conditional fill
  if(use_fill) {
    # Use Var2 for fill when available
    p <- ggplot(data, aes(x = !!sym(indVar1), y = !!sym(depVar), fill = !!sym(indVar2))) +
      geom_boxplot(alpha = alpha, position = position_dodge(width = 0.9), outliers = !showPoints)
    
    if(!is.null(var2Colors) && length(var2Colors) > 0) {
      p <- p + scale_fill_manual(values = var2Colors)
    }
  } else {
    # No fill aesthetic, use uniform color
    p <- ggplot(data, aes(x = !!sym(indVar1), y = !!sym(depVar))) +
      geom_boxplot(fill = "steelblue", alpha = alpha, outliers = !showPoints)
  }
  
  if(showPoints) {
    if(use_fill) {
      # Color points by Var2 when fill is used
      p <- p + geom_point(
        aes(color = !!sym(indVar2)), 
        size = pointSize, alpha = 0.5,
        position = position_jitterdodge(dodge.width = 0.9)
      ) +
        scale_color_manual(values = var2Colors, guide = "none")
    } else {
      # Uniform gray points when no fill
      p <- p + geom_jitter(width = 0.2, size = pointSize, alpha = 0.5, color = "gray30")
    }
  }
  
  if(showMean) {
    if(use_fill) {
      # Show mean per subgroup with matching fill color
      p <- p + stat_summary(
        aes(fill = !!sym(indVar2)),
        fun = mean, 
        geom = "point", 
        shape = 23, 
        size = 3,
        position = position_dodge(width = 0.9),
        show.legend = FALSE
      )
    } else {
      # Single overall mean in red when no grouping
      p <- p + stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red")
    }
  }
  
  p <- p + 
    labs(x = indVar1, y = depVar) +
    theme
  
  # Add facets if var3 is specified (var2 used for fill when present)
  addFacets(p, if(use_fill) NULL else indVar2, indVar3)
}

debugLog("PLOT_BAR_BOX: Bar and box plot functions loaded", "PLOT_INIT")
