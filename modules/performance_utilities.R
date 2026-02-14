# Performance Utilities Module
# Generic utility functions with automatic performance optimization

# Adaptive table function with automatic method selection
# Uses base R table() for small vectors, data.table for large vectors
table_fast <- function(vect, alphabetical_order = TRUE, threshold = 15000, useNA = FALSE) {
  n <- length(vect)
  
  debugLog(paste("PERFORMANCE_UTILS: table_fast called with", n, "elements"), "PERFORMANCE_OPTIMIZATION")
  start_time <- Sys.time()
  
  # Use base table for small vectors (better performance)
  if (n < threshold) {
    debugLog("PERFORMANCE_UTILS: Using base R table() for small vector", "PERFORMANCE_OPTIMIZATION")
    result <- table(vect, useNA = if(useNA) "always" else "no")
  } else {
    # Use data.table for large vectors (better performance)
    debugLog("PERFORMANCE_UTILS: Using data.table for large vector", "PERFORMANCE_OPTIMIZATION")
    
    dt <- data.table::data.table(modality = vect)
    if (alphabetical_order) {
      counts_dt <- dt[, .N, by = modality][order(modality)]
    } else {
      counts_dt <- dt[, .N, by = modality]
    }
    
    # Convert to table object for compatibility
    result <- setNames(counts_dt$N, counts_dt$modality)
    result <- as.table(result)
  }
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  debugLog(paste("PERFORMANCE_UTILS: table_fast completed in", round(elapsed_time, 4), "seconds"), "PERFORMANCE_OPTIMIZATION")
  
  return(result)
}

# Adaptive unique function with automatic method selection - currently unused, kept for possible future uses
unique_fast <- function(vect, sort_result = FALSE, threshold = 1e7) {
  n <- length(vect)
  
  # Use base unique for small vectors
  if (n < threshold) {
    result <- unique(vect)
  } else {
    # Use data.table for large vectors
    debugLog("PERFORMANCE_UTILS: Using data.table unique for large vector", "PERFORMANCE_OPTIMIZATION")
    dt <- data.table::data.table(values = vect)
    result <- dt[, unique(values)]
  }
  
  # Apply sorting if requested
  if (sort_result) {
    result <- sort(result)
  }
  
  return(result)
}

# Adaptive count-filter function for validation operations
count_filter_fast <- function(data, variable, min_count = 2, sort_result = TRUE, threshold = 1e5, user_approved_modalities = NULL) {
  n <- nrow(data)
  
  debugLog(paste("PERFORMANCE_UTILS: count_filter_fast called with", n, "rows"), "PERFORMANCE_OPTIMIZATION")
  start_time <- Sys.time()
  
  if (n < threshold) {
    # Use base R approach for small datasets
    debugLog("PERFORMANCE_UTILS: Using base R table() for small dataset", "PERFORMANCE_OPTIMIZATION")
    modalityCounts <- table(data[[variable]], useNA = "no")
    validModalities <- names(modalityCounts[modalityCounts >= min_count])
    filteredModalities <- names(modalityCounts[modalityCounts < min_count])
    
    # Add user-approved modalities to valid list even if they have low counts
    if (!is.null(user_approved_modalities) && length(user_approved_modalities) > 0) {
      approved_in_data <- intersect(names(modalityCounts), user_approved_modalities)
      if (length(approved_in_data) > 0) {
        validModalities <- unique(c(validModalities, approved_in_data))
        filteredModalities <- setdiff(filteredModalities, approved_in_data)
        debugLog(paste("PERFORMANCE_UTILS: Added", length(approved_in_data), "user-approved modalities to valid list"), "PERFORMANCE_OPTIMIZATION")
      }
      debugLog(paste("PERFORMANCE_UTILS: Final valid modalities:", paste(validModalities, collapse=", ")), "PERFORMANCE_OPTIMIZATION")
    }
    
    result <- list(
      valid_modalities = validModalities,
      all_counts = modalityCounts,
      filtered_modalities = filteredModalities,
      method_used = "base_table"
    )
  } else {
    # Use data.table for large datasets
    debugLog("PERFORMANCE_UTILS: Using data.table for large dataset", "PERFORMANCE_OPTIMIZATION")
    
    dt <- data.table::setDT(data.table::copy(data))
    
    # COUNT AND FILTER with explicit sorting if requested
    if (sort_result) {
      result_dt <- dt[, .N, by = c(variable)][N >= min_count][order(get(variable))]
      all_counts_dt <- dt[, .N, by = c(variable)][order(get(variable))]
    } else {
      result_dt <- dt[, .N, by = c(variable)][N >= min_count]
      all_counts_dt <- dt[, .N, by = c(variable)]
    }
    
    if (nrow(result_dt) == 0) {
      result <- list(
        valid_modalities = character(0),
        all_counts = setNames(integer(0), character(0)),
        filtered_modalities = unique(data[[variable]]),
        method_used = "data.table"
      )
    } else {
      # Extract results
      valid_modalities <- result_dt[[variable]]
      all_counts <- setNames(all_counts_dt$N, all_counts_dt[[variable]])
      
      # Add user-approved modalities to valid list even if they have low counts
      if (!is.null(user_approved_modalities) && length(user_approved_modalities) > 0) {
        all_modalities_in_data <- unique(data[[variable]])
        approved_in_data <- intersect(all_modalities_in_data, user_approved_modalities)
        if (length(approved_in_data) > 0) {
          valid_modalities <- unique(c(valid_modalities, approved_in_data))
          debugLog(paste("PERFORMANCE_UTILS: Added", length(approved_in_data), "user-approved modalities to valid list"), "PERFORMANCE_OPTIMIZATION")
        }
      }
      
      # Find filtered modalities
      all_modalities <- if (sort_result) {
        sort(unique(data[[variable]]))
      } else {
        unique(data[[variable]])
      }
      filtered_modalities <- setdiff(all_modalities, valid_modalities)
      
      result <- list(
        valid_modalities = valid_modalities,
        all_counts = all_counts,
        filtered_modalities = filtered_modalities,
        method_used = "data.table"
      )
    }
  }
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  debugLog(paste("PERFORMANCE_UTILS: count_filter_fast completed in", round(elapsed_time, 4), "seconds"), "PERFORMANCE_OPTIMIZATION")
  
  return(result)
}

debugLog("PERFORMANCE_UTILITIES: Performance utilities module loaded", "APP_STARTUP")
