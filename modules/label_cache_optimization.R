# Label cache optimization system with detailed tracking
# Caches frequently used labels for performance optimization

# Global label cache
labelCache <- new.env(parent = emptyenv())
labelCacheEnabled <- TRUE

# Cache statistics
labelCacheStats <- list(
  hits = 0,
  misses = 0,
  cache_size = 0,
  cascade_prevention_count = 0
)

# Detailed tracking for optimization (only when monitoring is enabled)
labelAccessTracking <- new.env(parent = emptyenv())
labelTrackingEnabled <- FALSE  # Set via enableLabelCacheTracking()

# Enable/disable detailed tracking (called from debug system)
enableLabelCacheTracking <- function(enable = TRUE) {
  labelTrackingEnabled <<- enable
  if(enable) {
    debugLog("LABEL_CACHE: Detailed tracking ENABLED", "CACHE_TRACE")
  } else {
    debugLog("LABEL_CACHE: Detailed tracking DISABLED", "CACHE_TRACE")
    # Clear tracking data when disabled
    rm(list = ls(envir = labelAccessTracking), envir = labelAccessTracking)
  }
}

# Initialize cache with frequently used labels only
initializeLabelCache <- function(labels) {
  debugLog("LABEL_CACHE: Initializing cache with frequently used labels", "CACHE_TRACE")
  
  if(is.null(labels) || !is.data.frame(labels)) {
    debugLog("LABEL_CACHE: No valid labels provided, cache disabled", "CACHE_TRACE")
    return()
  }
  
  # ONLY cache labels that are:
  # 1. Used in reactive contexts (observers, renders)
  # 2. Used in loops or repeated operations
  # 3. Part of core UI that refreshes often
  critical_labels <- c(
    # === CORE UI LABELS (always visible) ===
    "dependentVarLabel",
    "independentVar1Label",
    "independentVar2Label",
    "independentVar3Label",
    "noIndependentVar1Label",
    "noIndependentVar2Label", 
    "noIndependentVar3Label",
    "variable",
    
    # === COMMON BUTTONS (used everywhere) ===
    "okLabel",
    "cancelLabel",
    "confirmLabel",
    "applyLabel",
    "closeLabel",
    
    # === ERROR/WARNING MESSAGES (frequent) ===
    "errorTitle",
    "warningTitle",
    "invalidVariableTitle",
    "invalidVariableMessage",
    "numericVariableError",
    "insufficientDataError",
    "insufficientDataTitle",
    "insufficientDataMessage",
    
    # === VALIDATION STATES (reactive updates) ===
    "validatingVariableLabel",
    "validationCompleteLabel",
    "validationInProgressLabel",
    "automaticModeLabel",
    "userChoiceModeLabel",
    
    # === MODAL DIALOGS (v1.9.13 - keyboard nav) ===
    "naHandlingTitle",
    "naHandlingPrompt",
    "complexityWarningTitle",
    "modalitiesChoiceTitle",
    "numericConversionTitle",
    "filteredModalitiesTitle",
    "fallbackDialogTitle",
    
    # === DATA IMPORT (used at startup) ===
    "datafileInputLabel",
    "browseInputFileLabel",
    "noFileSelectedPrompt",
    "loadedFilePreviewDialogLabel",
    
    # === TAB LABELS (navigation) ===
    "variableSelectionTabLabel",
    "variable1TabLabel",
    "variable2TabLabel",
    "variable3TabLabel",
    "dataFilteringTabLabel",
    
    # === SELECTION ACTIONS (frequent operations) ===
    "selectAllLabel",
    "selectNoneLabel",
    "swapVar1Var2Label",
    "swapVar2Var3Label",
    "swapVar1Var3Label"
  )
  
  # Cache each label and track if monitoring is enabled
  cached_count <- 0
  for(label_key in critical_labels) {
    tryCatch({
      if("entry" %in% names(labels) && "displayedLabel" %in% names(labels)) {
        selectedRow <- labels[labels$entry == label_key, ]
        if(nrow(selectedRow) > 0 && !is.na(selectedRow$displayedLabel[1])) {
          value <- selectedRow$displayedLabel[1]
          assign(label_key, value, envir = labelCache)
          cached_count <- cached_count + 1
          
          # Initialize tracking if enabled
          if(labelTrackingEnabled) {
            assign(label_key, list(hits = 0, misses = 0, cached = TRUE), 
                   envir = labelAccessTracking)
          }
        }
      }
    }, error = function(e) {
      # Silent fail - label will be fetched on demand
    })
  }
  
  labelCacheStats$cache_size <<- cached_count
  debugLog(paste("LABEL_CACHE: Initialized with", cached_count, "frequently used labels"), "CACHE_TRACE")
}

# Track individual label access
trackLabelAccess <- function(key, isHit) {
  if(!labelTrackingEnabled) return()
  
  tryCatch({
    if(!exists(key, envir = labelAccessTracking)) {
      # First access - create tracking entry
      assign(key, list(hits = 0, misses = 0, cached = exists(key, envir = labelCache)), 
             envir = labelAccessTracking)
    }
    
    # Update counts
    tracking <- get(key, envir = labelAccessTracking)
    if(isHit) {
      tracking$hits <- tracking$hits + 1
    } else {
      tracking$misses <- tracking$misses + 1
    }
    assign(key, tracking, envir = labelAccessTracking)
  }, error = function(e) {
    # Silent fail - tracking is not critical
  })
}

# Main function - replaces both getCachedLabel and safeGetUILabel
safeGetUILabel <- function(key, labels = NULL, fallback = NULL) {
  # Quick return for disabled cache
  if(!labelCacheEnabled && exists("getLabelOrPrompt", envir = .GlobalEnv)) {
    return(getLabelOrPrompt(key, labels))
  }
  
  # Check cache first (fastest path)
  if(exists(key, envir = labelCache)) {
    labelCacheStats$hits <<- labelCacheStats$hits + 1
    trackLabelAccess(key, TRUE)  # Track hit
    return(get(key, envir = labelCache))
  }
  
  # Cache miss - try to get from labels
  labelCacheStats$misses <<- labelCacheStats$misses + 1
  trackLabelAccess(key, FALSE)  # Track miss
  
  result_value <- NULL
  if(!is.null(labels) && is.data.frame(labels) && 
     "entry" %in% names(labels) && "displayedLabel" %in% names(labels)) {
    tryCatch({
      selectedRow <- labels[labels$entry == key, ]
      if(nrow(selectedRow) > 0 && !is.na(selectedRow$displayedLabel[1])) {
        result_value <- selectedRow$displayedLabel[1]
        
        # Adaptive caching: add to cache if accessed frequently
        if(labelTrackingEnabled) {
          tracking <- get0(key, envir = labelAccessTracking)
          if(!is.null(tracking) && tracking$misses >= 5 && !tracking$cached) {
            # This label is accessed frequently but not cached - add it
            assign(key, result_value, envir = labelCache)
            tracking$cached <- TRUE
            assign(key, tracking, envir = labelAccessTracking)
            labelCacheStats$cache_size <<- labelCacheStats$cache_size + 1
            debugLog(paste("LABEL_CACHE: Auto-cached frequently accessed label:", key), "CACHE_TRACE")
          }
        }
      }
    }, error = function(e) {
      # Silent fail - use fallback
    })
  }
  
  # Return cascade: found value > fallback > key itself
  if(!is.null(result_value)) {
    return(result_value)
  } else if(!is.null(fallback)) {
    return(fallback)
  } else {
    return(key)
  }
}

# Backward compatibility alias
getCachedLabel <- safeGetUILabel

# Clear cache (useful for language changes)
clearLabelCache <- function() {
  rm(list = ls(envir = labelCache), envir = labelCache)
  labelCacheStats$cache_size <<- 0
  labelCacheStats$hits <<- 0
  labelCacheStats$misses <<- 0
  labelCacheStats$cascade_prevention_count <<- 0
  debugLog("LABEL_CACHE: Cache cleared", "CACHE_TRACE")
}

# Get basic cache statistics
getLabelCacheStats <- function() {
  total_accesses <- labelCacheStats$hits + labelCacheStats$misses
  hit_rate <- if(total_accesses > 0) {
    round(100 * labelCacheStats$hits / total_accesses, 2)
  } else {
    0
  }
  
  list(
    hits = labelCacheStats$hits,
    misses = labelCacheStats$misses,
    cache_size = labelCacheStats$cache_size,
    hit_rate_percent = hit_rate,
    total_accesses = total_accesses,
    cascade_prevented = labelCacheStats$cascade_prevention_count,
    tracking_enabled = labelTrackingEnabled
  )
}

# Get detailed label-by-label statistics and recommendations
getLabelCacheDetailedStats <- function(min_accesses = 5) {
  if(!labelTrackingEnabled) {
    return(list(
      error = "Detailed tracking is not enabled. Call enableLabelCacheTracking(TRUE) first.",
      tracking_enabled = FALSE
    ))
  }
  
  # Collect all tracking data
  all_labels <- ls(envir = labelAccessTracking)
  if(length(all_labels) == 0) {
    return(list(
      error = "No tracking data available yet.",
      tracking_enabled = TRUE
    ))
  }
  
  # Build detailed statistics
  details <- data.frame(
    label = character(),
    hits = numeric(),
    misses = numeric(),
    total = numeric(),
    hit_rate = numeric(),
    cached = logical(),
    recommendation = character(),
    stringsAsFactors = FALSE
  )
  
  for(label in all_labels) {
    tracking <- get(label, envir = labelAccessTracking)
    total <- tracking$hits + tracking$misses
    hit_rate <- if(total > 0) tracking$hits / total * 100 else 0
    
    # Determine recommendation
    recommendation <- ""
    if(total >= min_accesses) {
      if(tracking$cached && hit_rate < 20) {
        recommendation <- "REMOVE from cache (underused)"
      } else if(!tracking$cached && tracking$misses >= 10) {
        recommendation <- "ADD to cache (frequently accessed)"
      } else if(tracking$cached && hit_rate > 80) {
        recommendation <- "KEEP in cache (well used)"
      }
    }
    
    details <- rbind(details, data.frame(
      label = label,
      hits = tracking$hits,
      misses = tracking$misses,
      total = total,
      hit_rate = round(hit_rate, 1),
      cached = tracking$cached,
      recommendation = recommendation,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by total accesses (most used first)
  details <- details[order(details$total, decreasing = TRUE), ]
  
  # Generate summary recommendations
  to_add <- details$label[details$recommendation == "ADD to cache (frequently accessed)"]
  to_remove <- details$label[details$recommendation == "REMOVE from cache (underused)"]
  
  list(
    details = details,
    recommendations = list(
      to_add = to_add,
      to_remove = to_remove,
      summary = paste0(
        "Add ", length(to_add), " labels to cache, ",
        "Remove ", length(to_remove), " labels from cache"
      )
    ),
    tracking_enabled = TRUE,
    total_tracked = nrow(details),
    cached_count = sum(details$cached),
    uncached_count = sum(!details$cached)
  )
}

# Export detailed statistics to CSV file
exportLabelCacheStats <- function(filename = NULL) {
  if(is.null(filename)) {
    filename <- paste0("label_cache_stats_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  }
  
  stats <- getLabelCacheDetailedStats()
  
  if(!is.null(stats$error)) {
    return(list(success = FALSE, error = stats$error))
  }
  
  tryCatch({
    write.csv(stats$details, filename, row.names = FALSE)
    
    # Also write recommendations to a text file
    rec_filename <- sub("\\.csv$", "_recommendations.txt", filename)
    rec_text <- c(
      paste("Label Cache Analysis - ", Sys.time()),
      "",
      "SUMMARY:",
      stats$recommendations$summary,
      "",
      "LABELS TO ADD TO CACHE:",
      if(length(stats$recommendations$to_add) > 0) {
        paste("  - ", stats$recommendations$to_add)
      } else {
        "  (none)"
      },
      "",
      "LABELS TO REMOVE FROM CACHE:",
      if(length(stats$recommendations$to_remove) > 0) {
        paste("  - ", stats$recommendations$to_remove)
      } else {
        "  (none)"
      }
    )
    writeLines(rec_text, rec_filename)
    
    return(list(
      success = TRUE,
      stats_file = filename,
      recommendations_file = rec_filename,
      message = paste("Exported", nrow(stats$details), "label statistics")
    ))
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}
