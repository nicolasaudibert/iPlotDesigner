# Validation functions module - optimized version
# Enhanced validation for variables and combinations with data preprocessing optimization

# Performance optimization - count-filter-sort approach with adaptive method selection
# Uses performance_utilities for optimal method selection

# ============================================================================
# User choice utility functions
# ============================================================================

# Get user choice settings with fallback protection
getUserChoiceSetting <- function(key, fallback = NULL) {
  tryCatch({
    if (key %in% names(USER_CHOICE_SETTINGS)) {
      return(USER_CHOICE_SETTINGS[[key]])
    } else {
      return(fallback)
    }
  }, error = function(e) {
    debugLog(paste("Error accessing user choice setting:", key, "-", e$message), "APP_ERROR")
    return(fallback)
  })
}

# Check if user choice dialogs are enabled
isUserChoiceEnabled <- function() {
  getUserChoiceSetting("enable_user_choice_dialogs", FALSE)
}

# Get complexity level based on modality count
getComplexityLevel <- function(modality_count) {
  if (modality_count <= COMPLEXITY_THRESHOLDS$simple) {
    return("simple")
  } else if (modality_count <= COMPLEXITY_THRESHOLDS$moderate) {
    return("moderate")
  } else if (modality_count <= COMPLEXITY_THRESHOLDS$complex) {
    return("complex")
  } else if (modality_count <= COMPLEXITY_THRESHOLDS$very_complex) {
    return("very_complex")
  } else {
    return("extreme")
  }
}

# Estimate performance impact for given complexity
estimatePerformanceImpact <- function(modality_count, sparsity = 0.0) {
  base_time <- PERFORMANCE_ESTIMATION$base_ui_time
  modality_factor <- log10(max(modality_count, 1)) * PERFORMANCE_ESTIMATION$modality_factor
  sparsity_factor <- sparsity * PERFORMANCE_ESTIMATION$sparsity_penalty
  
  estimated_time <- base_time + modality_factor + sparsity_factor
  
  # Determine performance level
  performance_level <- if (estimated_time < PERFORMANCE_ESTIMATION$good_threshold) {
    "good"
  } else if (estimated_time < PERFORMANCE_ESTIMATION$acceptable_threshold) {
    "acceptable"
  } else if (estimated_time < PERFORMANCE_ESTIMATION$slow_threshold) {
    "slow"
  } else {
    "very_slow"
  }
  
  return(list(
    estimated_ui_time = estimated_time,
    dom_elements = modality_count * PERFORMANCE_ESTIMATION$dom_elements_per_modality,
    memory_usage_mb = modality_count * PERFORMANCE_ESTIMATION$memory_mb_per_modality,
    performance_level = performance_level,
    modality_count = modality_count,
    sparsity = sparsity
  ))
}

# Get recommended filtering method based on complexity
getRecommendedFilteringMethod <- function(modality_count, complexity_level = NULL) {
  if (is.null(complexity_level)) {
    complexity_level <- getComplexityLevel(modality_count)
  }
  
  switch(complexity_level,
    "simple" = "keep_all",
    "moderate" = "min_frequency",
    "complex" = "top_n",
    "very_complex" = "auto_group",
    "extreme" = "top_n"  # Aggressive filtering for extreme cases
  )
}

# ============================================================================
# Core validation functions
# ============================================================================

# Enhanced validation for final combinations with better error handling
validateSelectedCombination <- function(data, depVar, indVar1, indVar2 = NULL, indVar3 = NULL, 
                                        var1Selected = NULL, var2Selected = NULL, var3Selected = NULL) {
  
  debugLog("DEBUG validateSelectedCombination - Starting validation", "VALIDATION_PROCESS")
  
  if(is.null(data) || nrow(data) == 0) {
    debugLog("DEBUG validateSelectedCombination - No data available", "VALIDATION_PROCESS")
    return(list(
      isValid = FALSE,
      reason = "No data available",
      hasData = FALSE,
      dataCount = 0
    ))
  }
  
  if(is.null(depVar) || depVar == "" || is.null(indVar1) || indVar1 == "") {
    debugLog("DEBUG validateSelectedCombination - Missing required variables", "VALIDATION_PROCESS")
    return(list(
      isValid = FALSE,
      reason = "Missing required variables",
      hasData = FALSE,
      dataCount = 0
    ))
  }
  
  if(!(depVar %in% names(data)) || !(indVar1 %in% names(data))) {
    debugLog("DEBUG validateSelectedCombination - Variables not found in data", "VALIDATION_PROCESS")
    return(list(
      isValid = FALSE,
      reason = "Variables not found in data",
      hasData = FALSE,
      dataCount = 0
    ))
  }
  
  debugLog(paste("Variables:", indVar1, "|", indVar2, "|", indVar3), "VALIDATION_PROCESS")
  
  workingData <- data
  originalCount <- nrow(workingData)
  
  tryCatch({
    if(!is.null(var1Selected) && length(var1Selected) > 0) {
      workingData <- workingData %>% filter(!!sym(indVar1) %in% var1Selected)
      debugLog(paste("DEBUG - After var1 filter:", nrow(workingData), "rows"), "VALIDATION_PROCESS")
    }
    
    if(!is.null(indVar2) && indVar2 != "" && !is.null(var2Selected) && length(var2Selected) > 0) {
      if(indVar2 %in% names(workingData)) {
        workingData <- workingData %>% filter(!!sym(indVar2) %in% var2Selected)
        debugLog(paste("DEBUG - After var2 filter:", nrow(workingData), "rows"), "VALIDATION_PROCESS")
      }
    }
    
    if(!is.null(indVar3) && indVar3 != "" && !is.null(var3Selected) && length(var3Selected) > 0) {
      if(indVar3 %in% names(workingData)) {
        workingData <- workingData %>% filter(!!sym(indVar3) %in% var3Selected)
        debugLog(paste("DEBUG - After var3 filter:", nrow(workingData), "rows"), "VALIDATION_PROCESS")
      }
    }
  }, error = function(e) {
    debugLog(paste("DEBUG validateSelectedCombination - Error during filtering:", e$message), "VALIDATION_PROCESS")
    return(list(
      isValid = FALSE,
      reason = paste("Error during data filtering:", e$message),
      hasData = FALSE,
      dataCount = 0
    ))
  })
  
  finalCount <- nrow(workingData)
  isValid <- finalCount >= 2
  
  validGroups <- 0
  if(isValid && !is.null(indVar1) && indVar1 %in% names(workingData)) {
    tryCatch({
      groupCounts <- workingData %>%
        group_by(!!sym(indVar1)) %>%
        summarise(n = n(), .groups = 'drop')
      
      validGroups <- sum(groupCounts$n >= 2)
      if(validGroups == 0) {
        isValid <- FALSE
        debugLog("DEBUG validateSelectedCombination - No valid groups found", "VALIDATION_PROCESS")
      }
    }, error = function(e) {
      debugLog(paste("DEBUG validateSelectedCombination - Error checking groups:", e$message), "VALIDATION_PROCESS")
      isValid <- FALSE
      validGroups <- 0
    })
  }
  
  debugLog(paste("DEBUG validateSelectedCombination - Final:", finalCount, "points, valid:", isValid), "VALIDATION_PROCESS")
  
  return(list(
    isValid = isValid,
    reason = if(!isValid) {
      if(finalCount == 0) "No data points match the selected combination"
      else if(finalCount == 1) "Only 1 data point matches the selected combination"
      else paste("Only", finalCount, "data points - insufficient for analysis")
    } else "Valid",
    hasData = finalCount > 0,
    dataCount = finalCount,
    validGroups = validGroups
  ))
}

debugLog("VALIDATION_FUNCTIONS: Validation functions module loaded with utility functions", "APP_STARTUP")
