# Plot Violin and Ridge Module
# Violin plots and ridge plots

# Calculate default bandwidth for geom_density_ridges
# This function replicates ggridges internal bandwidth calculation to avoid
# informational messages during plot rendering
calculate_ridges_bandwidth <- function(data, x, group, panel = NULL) {
  
  x_col <- enquo(x)
  group_col <- enquo(group)
  panel_col <- enquo(panel)
  
  # Prepare data
  if (quo_is_null(panel_col)) {
    # Single panel case
    df <- data %>%
      select(x = !!x_col, group = !!group_col) %>%
      drop_na() %>%
      mutate(PANEL = 1)
  } else {
    # Multi-panel case: handle single variable only (adapted for !! instead of !!!)
    df <- data %>%
      select(x = !!x_col, group = !!group_col, panel_var = !!panel_col) %>%
      drop_na() %>%
      group_by(panel_var) %>%
      mutate(PANEL = cur_group_id()) %>%
      ungroup()
  }
  
  # Calculate bandwidth per panel
  result <- df %>%
    group_by(PANEL) %>%
    summarise(
      bandwidth = {
        # Split by group and calculate bandwidth for each
        groups_data <- split(x, group)
        
        # Keep only groups with more than 1 observation
        groups_data <- groups_data[sapply(groups_data, length) > 1]
        
        # Calculate bandwidth for each group
        bws <- sapply(groups_data, bw.nrd0)
        
        # Return mean bandwidth
        mean(bws, na.rm = TRUE)
      },
      from = min(x, na.rm = TRUE) - 3 * bandwidth,
      to = max(x, na.rm = TRUE) + 3 * bandwidth,
      .groups = "drop"
    )
  
  # Add panel labels if provided
  if (!quo_is_null(panel_col)) {
    panel_labels <- df %>%
      select(PANEL, panel_var) %>%
      distinct()
    
    result <- result %>%
      left_join(panel_labels, by = "PANEL")
  }
  
  result
}

# Violin plot function with conditional fill based on Var2 presence
createViolinPlot <- function(data, depVar, indVar1, indVar2, indVar3,
                            var1Selected, var1Colors, var1Order,
                            var2Selected, var2Colors, var2Order,
                            var3Selected, var3Order,
                            alpha, showBox, showPoints, trim, theme) {
  
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
                              var3Selected, var3Order,
                              filterSingletons = TRUE)
  
  # Determine if fill aesthetic should be used
  use_fill <- !is.null(indVar2) && indVar2 != ""
  
  # Suppress warnings about groups with fewer than two data points
  suppressWarnings({
    # Create plot with conditional fill
    if(use_fill) {
      # Use Var2 for fill when available
      violin_params <- list(alpha = alpha, trim = trim)
      if(showBox) {
        violin_params$position <- position_dodge(width = 0.9)
        violin_params$drop <- FALSE
      }
      
      p <- ggplot(data, aes(x = !!sym(indVar1), y = !!sym(depVar), fill = !!sym(indVar2))) +
        do.call(geom_violin, violin_params)
      
      if(!is.null(var2Colors) && length(var2Colors) > 0) {
        p <- p + scale_fill_manual(values = var2Colors)
      }
    } else {
      # No fill aesthetic, use uniform color
      violin_params <- list(fill = "steelblue", alpha = alpha, trim = trim)
      if(showBox) {
        violin_params$position <- position_dodge(width = 0.9)
        violin_params$drop <- FALSE
      }
      
      p <- ggplot(data, aes(x = !!sym(indVar1), y = !!sym(depVar))) +
        do.call(geom_violin, violin_params)
    }
    
    # Add optional boxplot overlay
    if(showBox) {
      if(use_fill) {
        # Hide outliers when points are shown to avoid redundancy
        p <- p + geom_boxplot(aes(fill = !!sym(indVar2)),
                             width = 0.1, alpha = 0.5, 
                             position = position_dodge(width = 0.9),
                             outliers = !showPoints)
      } else {
        p <- p + geom_boxplot(width = 0.1, fill = "white", alpha = 0.5, 
                             position = position_dodge(width = 0.9),
                             outliers = !showPoints)
      }
    }
    
    # Add optional individual points
    if(showPoints) {
      if(use_fill) {
        # Color points by Var2 when fill is used
        p <- p + geom_point(
            aes(color = !!sym(indVar2)), 
            size = 1, alpha = 0.3,
            position = position_jitterdodge(dodge.width = 0.9)
          ) +
          scale_color_manual(values = var2Colors, guide = "none")
      } else {
        # Uniform gray points when no fill
        p <- p + geom_jitter(width = 0.2, size = 1, alpha = 0.3, color = "gray30")
      }
    }
    
    p <- p + 
      labs(x = indVar1, y = depVar) +
      theme
  })
  
  # Add facets if var3 is specified (var2 used for fill when present)
  addFacets(p, if(use_fill) NULL else indVar2, indVar3)
}

# Ridge plot function
createRidgePlot <- function(data, depVar, indVar1, indVar2, indVar3,
                           var1Selected, var1Colors, var1Order,
                           var2Selected, var2Order,
                           var3Selected, var3Order,
                           scale, alpha, type, bins, showQuantiles, theme) {
  
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
  
  # Apply color fallbacks for var1 only (ridge plots use var1 for coloring)
  if(is.null(var1Colors) && !is.null(var1Selected)) {
    var1Colors <- setNames(defaultColors$var1[1:length(var1Selected)], var1Selected)
  }
  
  # Filter data
  if(!is.null(var1Selected) && length(var1Selected) > 0) {
    data <- data %>% filter(!!sym(indVar1) %in% var1Selected)
  }
  if(!is.null(indVar3) && indVar3 != "" && !is.null(var3Selected) && length(var3Selected) > 0) {
    data <- data %>% filter(!!sym(indVar3) %in% var3Selected)
  }
  
  # Reorder factor (reverse for ridge plots)
  if(!is.null(var1Order) && length(var1Order) > 0) {
    data[[indVar1]] <- factor(data[[indVar1]], levels = rev(var1Order))
  }
  if(!is.null(indVar3) && indVar3 != "" && !is.null(var3Order) && length(var3Order) > 0) {
    data[[indVar3]] <- factor(data[[indVar3]], levels = var3Order)
  }
  
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
  
  # Calculate bandwidth parameters to avoid informational messages during rendering
  # Determine panel variable for bandwidth calculation (use combined variable if both var2 and var3)
  panel_var_for_bw <- NULL
  if(!is.null(indVar2) && indVar2 != "" && !is.null(indVar3) && indVar3 != "") {
    # When both var2 and var3 are present, create combined panel variable
    data$combined_panel <- interaction(data[[indVar2]], data[[indVar3]], sep = " / ", drop = TRUE)
    panel_var_for_bw <- sym("combined_panel")
  } else if(!is.null(indVar2) && indVar2 != "") {
    panel_var_for_bw <- sym(indVar2)
  } else if(!is.null(indVar3) && indVar3 != "") {
    panel_var_for_bw <- sym(indVar3)
  }
  
  # Calculate bandwidth only for density type (binline doesn't use it)
  bandwidth_params <- list()
  if(type == "density") {
    bw_data <- tryCatch({
      calculate_ridges_bandwidth(data, !!sym(depVar), !!sym(indVar1), !!panel_var_for_bw)
    }, error = function(e) {
      NULL
    })
    
    if(!is.null(bw_data) && nrow(bw_data) > 0) {
      # Use first panel's values (works for single panel or consistent multi-panel)
      bandwidth_params <- list(
        bandwidth = bw_data$bandwidth[1],
        from = bw_data$from[1],
        to = bw_data$to[1]
      )
    }
  }
  
  # Suppress remaining warnings about groups with fewer than two data points
  suppressWarnings({
    # Create plot based on type
    if(type == "density") {
      # Build geom_density_ridges parameters
      ridge_params <- c(
        list(
          scale = scale, 
          alpha = alpha,
          quantile_lines = showQuantiles,
          quantiles = if(showQuantiles) c(0.25, 0.5, 0.75) else NULL
        ),
        bandwidth_params
      )
      
      p <- ggplot(data, aes(x = !!sym(depVar), y = !!sym(indVar1), fill = !!sym(indVar1))) +
        do.call(geom_density_ridges, ridge_params)
    } else {
      p <- ggplot(data, aes(x = !!sym(depVar), y = !!sym(indVar1), fill = !!sym(indVar1))) +
        geom_density_ridges(
          stat = "binline", 
          bins = bins, 
          scale = scale, 
          alpha = alpha
        )
    }
    
    # Add colors only if available and valid
    if(!is.null(var1Colors) && length(var1Colors) > 0) {
      p <- p + scale_fill_manual(values = var1Colors)
    }
    
    p <- p +
      labs(x = depVar, y = indVar1) +
      theme
  })
  
  # For ridge plots, handle faceting for var2 and/or var3
  if(!is.null(indVar2) && indVar2 != "" && !is.null(indVar3) && indVar3 != "") {
    # Both var2 and var3: use facet_grid
    indVar2_escaped <- escapeVarName(indVar2)
    indVar3_escaped <- escapeVarName(indVar3)
    p <- p + facet_grid(as.formula(paste(indVar3_escaped, "~", indVar2_escaped)), scales = "free_x")
  } else if(!is.null(indVar2) && indVar2 != "") {
    # Only var2: use facet_wrap
    indVar2_escaped <- escapeVarName(indVar2)
    p <- p + facet_wrap(as.formula(paste("~", indVar2_escaped)), scales = "free_x")
  } else if(!is.null(indVar3) && indVar3 != "") {
    # Only var3: use facet_wrap
    indVar3_escaped <- escapeVarName(indVar3)
    p <- p + facet_wrap(as.formula(paste("~", indVar3_escaped)), scales = "free_x")
  }
  
  p
}

debugLog("PLOT_VIOLIN_RIDGE: Violin and ridge plot functions loaded", "PLOT_INIT")
