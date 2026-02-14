# Plot Core Helpers Module
# Common helper functions used across all plot types

# Helper function to validate colors against selected modalities
validateColors <- function(colors, selectedModalities) {
  if(is.null(colors) || is.null(selectedModalities)) {
    return(NULL)
  }
  
  # Keep only colors for selected modalities
  validColors <- colors[names(colors) %in% selectedModalities]
  
  # If no valid colors, return NULL
  if(length(validColors) == 0) {
    return(NULL)
  }
  
  return(validColors)
}

# Helper function to add facets with proper variable name escaping - CORRECTED VERSION
addFacets <- function(plot, var2, var3, var2Name = NULL, var3Name = NULL) {
  # Safety check: if var2 and var3 are identical, avoid facet_grid
  if(!is.null(var2) && var2 != "" && !is.null(var3) && var3 != "" && var2 == var3) {
    # cat("DEBUG addFacets - Identical faceting variables detected:", var2, "==", var3, "\n")
    # Use only one variable for faceting to avoid duplication error
    var2_escaped <- escapeVarName(var2)
    return(plot + facet_wrap(as.formula(paste("~", var2_escaped)), scales = "free"))
  }
  
  # Normal faceting logic
  if(!is.null(var3) && var3 != "") {
    if(!is.null(var2) && var2 != "") {
      # Both var2 and var3: use facet_grid with proper escaping
      var2_escaped <- escapeVarName(var2)
      var3_escaped <- escapeVarName(var3)
      plot + facet_grid(as.formula(paste(var2_escaped, "~", var3_escaped)))
    } else {
      # Only var3: use facet_wrap with proper escaping
      var3_escaped <- escapeVarName(var3)
      plot + facet_wrap(as.formula(paste("~", var3_escaped)), scales = "free")
    }
  } else if(!is.null(var2) && var2 != "") {
    # Only var2: use facet_wrap with proper escaping
    var2_escaped <- escapeVarName(var2)
    plot + facet_wrap(as.formula(paste("~", var2_escaped)), scales = "free")
  } else {
    plot
  }
}

# Helper function to apply color fallbacks
applyColorFallbacks <- function(var1Colors, var2Colors, var1Selected, var2Selected) {
  # Fallback if no valid colors for var1
  if(is.null(var1Colors) && !is.null(var1Selected)) {
    var1Colors <- setNames(defaultColors$var1[1:length(var1Selected)], var1Selected)
  }
  
  # Fallback if no valid colors for var2
  if(is.null(var2Colors) && !is.null(var2Selected)) {
    var2Colors <- setNames(defaultColors$var2[1:length(var2Selected)], var2Selected)
  }
  
  return(list(var1Colors = var1Colors, var2Colors = var2Colors))
}

# Helper function to filter and reorder data
# When filterSingletons is TRUE, removes groups with only one observation based on 
# the combination of all defined grouping variables
filterAndReorderData <- function(data, indVar1, indVar2, indVar3, 
                                 var1Selected, var1Order,
                                 var2Selected, var2Order,
                                 var3Selected, var3Order,
                                 filterSingletons = FALSE) {
  
  # Filter data based on selected modalities
  if(!is.null(var1Selected) && length(var1Selected) > 0) {
    data <- data %>% filter(!!sym(indVar1) %in% var1Selected)
  }
  if(!is.null(indVar2) && indVar2 != "" && !is.null(var2Selected) && length(var2Selected) > 0) {
    data <- data %>% filter(!!sym(indVar2) %in% var2Selected)
  }
  if(!is.null(indVar3) && indVar3 != "" && !is.null(var3Selected) && length(var3Selected) > 0) {
    data <- data %>% filter(!!sym(indVar3) %in% var3Selected)
  }
  
  # Filter out singleton groups if requested
  if(filterSingletons) {
    # Determine grouping variables based on which are defined
    group_vars <- c(indVar1)
    if(!is.null(indVar2) && indVar2 != "") {
      group_vars <- c(group_vars, indVar2)
    }
    if(!is.null(indVar3) && indVar3 != "") {
      group_vars <- c(group_vars, indVar3)
    }
    
    # Count observations per group combination
    initial_rows <- nrow(data)
    data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      filter(n() > 1) %>%
      ungroup()
    
    # Log filtering if any groups were removed
    filtered_rows <- initial_rows - nrow(data)
    if(filtered_rows > 0) {
      debugLog(paste("PLOT_FILTER: Removed", filtered_rows, 
                     "observations from singleton groups in", 
                     paste(group_vars, collapse = " × ")), 
               "PERFORMANCE_OPTIMIZATION")
    }
  }
  
  # Reorder factors using current order
  if(!is.null(var1Order) && length(var1Order) > 0) {
    data[[indVar1]] <- factor(data[[indVar1]], levels = var1Order)
  }
  if(!is.null(indVar2) && indVar2 != "" && !is.null(var2Order) && length(var2Order) > 0) {
    data[[indVar2]] <- factor(data[[indVar2]], levels = var2Order)
  }
  if(!is.null(indVar3) && indVar3 != "" && !is.null(var3Order) && length(var3Order) > 0) {
    data[[indVar3]] <- factor(data[[indVar3]], levels = var3Order)
  }
  
  return(data)
}

# Clean selected modalities by removing phantom values
# Note: NA values are preserved as they may be valid modalities when user chose keep_na
cleanSelectedModalities <- function(selected_modalities, modalities_order, data, variable_name) {
  if (is.null(selected_modalities) || length(selected_modalities) == 0) {
    return(selected_modalities)
  }
  
  # Keep all selected modalities including NA (may be valid when keep_na is used)
  clean_selected <- selected_modalities
  
  # Clean selected modalities based on order (remove phantom values)
  # Use special comparison that handles NA correctly
  if (!is.null(modalities_order) && length(modalities_order) > 0) {
    clean_selected <- selected_modalities[selected_modalities %in% modalities_order | (is.na(selected_modalities) & any(is.na(modalities_order)))]
  }
  
  # Check against actual data if variable exists
  if (!is.null(data) && variable_name %in% names(data)) {
    actual_modalities <- unique(data[[variable_name]])
    
    # Keep modalities that exist in data (including NA if present)
    clean_selected <- selected_modalities[selected_modalities %in% actual_modalities | (is.na(selected_modalities) & any(is.na(actual_modalities)))]
    
    # Log if filtering occurred
    removed_count <- length(selected_modalities) - length(clean_selected)
    if (removed_count > 0) {
      debugLog(paste("PLOT_CLEAN: Filtered", removed_count, 
                     "phantom modalities from", variable_name), "PERFORMANCE_OPTIMIZATION")
    }
  }
  
  return(clean_selected)
}

debugLog("PLOT_CORE_HELPERS: Helper functions loaded", "PLOT_INIT")
