# Plot Histogram and Density Module
# Histogram and density plots with VI3 support

# Histogram function
createHistogram <- function(data, depVar, indVar1, indVar2, indVar3,
                            var1Selected, var1Colors, var1Order,
                            var2Selected, var2Order,
                            var3Selected, var3Order,
                            bins, alpha, position, showDensity, theme) {
  
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
  
  # Apply color fallbacks for var1 only (histograms use var1 for coloring)
  if(is.null(var1Colors) && !is.null(var1Selected)) {
    var1Colors <- setNames(defaultColors$var1[1:length(var1Selected)], var1Selected)
  }
  
  # Filter and reorder data - keep all groups for histogram
  data <- filterAndReorderData(data, indVar1, indVar2, indVar3,
                               var1Selected, var1Order,
                               var2Selected, var2Order,
                               var3Selected, var3Order,
                               filterSingletons = FALSE)
  
  # Create base histogram plot
  p <- ggplot(data, aes(x = !!sym(depVar))) +
    geom_histogram(aes(fill = !!sym(indVar1)), 
                   bins = bins,
                   alpha = alpha,
                   position = position) +
    labs(x = depVar, y = "Count") +
    theme
  
  # Add colors only if available and valid
  if(!is.null(var1Colors) && length(var1Colors) > 0) {
    p <- p + scale_fill_manual(values = var1Colors)
  }
  
  if(showDensity) {
    # For density overlay, filter to groups with at least 2 observations
    # to avoid geom_density errors while preserving histogram for all groups
    group_vars <- c(indVar1)
    if(!is.null(indVar2) && indVar2 != "") {
      group_vars <- c(group_vars, indVar2)
    }
    if(!is.null(indVar3) && indVar3 != "") {
      group_vars <- c(group_vars, indVar3)
    }
    
    # Create filtered dataset for density curves only
    density_data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      filter(n() > 1) %>%
      ungroup()
    
    # Calculate density scaling factor using the full data range
    data_range <- diff(range(data[[depVar]], na.rm = TRUE))
    
    # Only add density if we have valid data after filtering
    if(nrow(density_data) > 0 && data_range > 0) {
      suppressWarnings({
        p <- p + 
          geom_density(
            data = density_data,
            aes(y = after_stat(count) * bins / data_range, color = !!sym(indVar1)), 
            linewidth = 1.2
          )
        
        # Add color scale for density lines
        if(!is.null(var1Colors) && length(var1Colors) > 0) {
          p <- p + scale_color_manual(values = var1Colors)
        }
      })
    }
  }
  
  addFacets(p, indVar2, indVar3)
}

# Density plot function
createDensityPlot <- function(data, depVar, indVar1, indVar2, indVar3,
                             var1Selected, var1Colors, var1Order,
                             var2Selected, var2Order,
                             var3Selected, var3Order,
                             alpha, lineSize, fill, showRug, theme) {
  
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
  
  # Apply color fallbacks for var1 only (density plots use var1 for coloring)
  if(is.null(var1Colors) && !is.null(var1Selected)) {
    var1Colors <- setNames(defaultColors$var1[1:length(var1Selected)], var1Selected)
  }
  
  # Filter and reorder data
  data <- filterAndReorderData(data, indVar1, indVar2, indVar3,
                               var1Selected, var1Order,
                               var2Selected, var2Order,
                               var3Selected, var3Order,
                               filterSingletons = TRUE)
  
  # Additional filtering: remove subgroups with insufficient data for density calculation
  # Build list of grouping variables for subgroup identification
  group_vars <- c(indVar1)
  if(!is.null(indVar2) && indVar2 != "") {
    group_vars <- c(group_vars, indVar2)
  }
  if(!is.null(indVar3) && indVar3 != "") {
    group_vars <- c(group_vars, indVar3)
  }
  
  # Filter to keep only subgroups with at least 2 observations
  data <- data %>%
    group_by(across(all_of(group_vars))) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Suppress warnings about groups with fewer than two data points
  suppressWarnings({
    if(fill) {
      p <- ggplot(data, aes(x = !!sym(depVar), fill = !!sym(indVar1))) +
        geom_density(alpha = alpha, linewidth = lineSize)
      
      # Add colors only if available and valid
      if(!is.null(var1Colors) && length(var1Colors) > 0) {
        p <- p + scale_fill_manual(values = var1Colors)
      }
    } else {
      p <- ggplot(data, aes(x = !!sym(depVar), color = !!sym(indVar1))) +
        geom_density(linewidth = lineSize)
      
      # Add colors only if available and valid
      if(!is.null(var1Colors) && length(var1Colors) > 0) {
        p <- p + scale_color_manual(values = var1Colors)
      }
    }
    
    if(showRug) {
      if(!fill) {
        p <- p + geom_rug(aes(color = !!sym(indVar1)), alpha = 0.5)
      } else {
        p <- p + geom_rug(aes(color = !!sym(indVar1)), alpha = 0.5)
        
        # Add color scale for rug if not already added
        if(!is.null(var1Colors) && length(var1Colors) > 0) {
          p <- p + scale_color_manual(values = var1Colors, guide = "none")
        }
      }
    }
    
    p <- p + 
      labs(x = depVar, y = "Density") +
      theme
    
    # Add facets inside suppressWarnings to catch all warnings
    p <- addFacets(p, indVar2, indVar3)
  })
  
  p
}

debugLog("PLOT_HISTOGRAM_DENSITY: Histogram and density plot functions loaded", "PLOT_INIT")
