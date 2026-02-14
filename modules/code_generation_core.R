# Enhanced Code Generation Core - Executable Operations

# Load theme code generation modules
source("modules/code_generation_theme_base.R")
debugLog("Loaded code_generation_theme_base.R", "MODULE_LOAD")
source("modules/code_generation_theme_backgrounds.R")
debugLog("Loaded code_generation_theme_backgrounds.R", "MODULE_LOAD")
source("modules/code_generation_theme_titles.R")
debugLog("Loaded code_generation_theme_titles.R", "MODULE_LOAD")
source("modules/code_generation_theme_axis.R")
debugLog("Loaded code_generation_theme_axis.R", "MODULE_LOAD")
source("modules/code_generation_theme_grids.R")
debugLog("Loaded code_generation_theme_grids.R", "MODULE_LOAD")
source("modules/code_generation_fonts.R")
debugLog("Loaded code_generation_fonts.R", "MODULE_LOAD")

# ============================================================================
# Executable code generation functions
# ============================================================================

# Get summary of all operations for code generation
getUserOperationsSummary <- function(values) {
  if (is.null(values$user_operations_history) || 
      is.null(values$user_operations_history$operations)) {
    return(list(
      has_operations = FALSE,
      total_operations = 0,
      by_type = list(),
      by_variable = list()
    ))
  }
  
  operations <- values$user_operations_history$operations
  
  summary <- list(
    has_operations = length(operations) > 0,
    total_operations = length(operations),
    session_id = values$user_operations_history$session_id,
    created_at = values$user_operations_history$created_at,
    last_updated = values$user_operations_history$last_updated,
    by_type = list(),
    by_variable = list()
  )
  
  if (length(operations) > 0) {
    # Group by type
    for (op_type in c("na_handling", "numeric_conversion", "custom_filtering")) {
      type_ops <- operations[sapply(operations, function(op) op$type == op_type)]
      if (length(type_ops) > 0) {
        summary$by_type[[op_type]] <- list(
          count = length(type_ops),
          operations = type_ops
        )
      }
    }
    
    # Group by variable
    variables <- unique(sapply(operations, function(op) op$variable_name))
    for (var_name in variables) {
      var_ops <- operations[sapply(operations, function(op) op$variable_name == var_name)]
      summary$by_variable[[var_name]] <- list(
        count = length(var_ops),
        operations = var_ops
      )
    }
  }
  
  return(summary)
}

# Main executable user operations code generation
generateExecutableUserOperationsCode <- function(values, labels) {
  
  tryCatch({
    operations_summary <- getUserOperationsSummary(values)
    
    if (!operations_summary$has_operations) {
      debugLog("USER_OPERATIONS_CODE: No operations found", "CODE_DEBUG")
      return("")
    }
    
    debugLog(paste("USER_OPERATIONS_CODE: Found", operations_summary$total_operations, "operations"), "CODE_DEBUG")
    
    # Check configuration - simplified approach
    include_session_info <- TRUE  # Default enabled
    include_timestamps <- TRUE    # Default enabled
    
    # Try to get from input if available
    if (exists("input", envir = parent.frame())) {
      input_env <- get("input", envir = parent.frame())
      if (!is.null(input_env$includeOperationsMetadata_bar)) {
        include_session_info <- input_env$includeOperationsMetadata_bar
      }
      if (!is.null(input_env$includeOperationsTimestamps_bar)) {
        include_timestamps <- input_env$includeOperationsTimestamps_bar
      }
    }
    
    # Localized headers
    headerTitle <- safeGetUILabel("codeHeaderUserOperations", labels, "USER OPERATIONS HISTORY")
    sessionLabel <- safeGetUILabel("codeSessionLabel", labels, "Session")
    generatedLabel <- safeGetUILabel("codeGeneratedOnLabel", labels, "Generated on")
    totalLabel <- safeGetUILabel("codeTotalOperationsLabel", labels, "Total operations")
    
    code_lines <- c(
      "",
      "# ============================================================================",
      paste("#", headerTitle)
    )
    
    # Add session info only if enabled
    if (include_session_info) {
      code_lines <- c(code_lines,
                      paste("#", sessionLabel, ":", operations_summary$session_id)
      )
    }
    
    # Add timestamp only if enabled  
    if (include_timestamps) {
      code_lines <- c(code_lines,
                      paste("#", generatedLabel, ":", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      )
    }
    
    code_lines <- c(code_lines,
                    paste("#", totalLabel, ":", operations_summary$total_operations),
                    "# ============================================================================",
                    ""
    )
    
    # Generate executable NA handling operations (highest priority)
    if ("na_handling" %in% names(operations_summary$by_type)) {
      na_code <- generateExecutableNAHandlingOperations(operations_summary$by_type$na_handling$operations, labels)
      if (length(na_code) > 0) {
        code_lines <- c(code_lines, na_code)
        debugLog("USER_OPERATIONS_CODE: Added NA handling operations", "CODE_DEBUG")
      }
    }
    
    # Generate executable numeric conversions
    if ("numeric_conversion" %in% names(operations_summary$by_type)) {
      numeric_code <- generateExecutableNumericConversions(operations_summary$by_type$numeric_conversion$operations, labels)
      if (length(numeric_code) > 0) {
        code_lines <- c(code_lines, numeric_code)
        debugLog("USER_OPERATIONS_CODE: Added numeric conversions", "CODE_DEBUG")
      }
    }
    
    # Generate executable filtering operations  
    if ("custom_filtering" %in% names(operations_summary$by_type)) {
      filtering_code <- generateExecutableFilteringOperations(operations_summary$by_type$custom_filtering$operations, labels)
      if (length(filtering_code) > 0) {
        code_lines <- c(code_lines, filtering_code)
        debugLog("USER_OPERATIONS_CODE: Added filtering operations", "CODE_DEBUG")
      }
    }
    
    # Add verification code if enabled
    include_verification <- TRUE  # Default enabled
    if (exists("input", envir = parent.frame())) {
      input_env <- get("input", envir = parent.frame())
      if (!is.null(input_env$includeVerificationSteps_bar)) {
        include_verification <- input_env$includeVerificationSteps_bar
      }
    }
    
    if (include_verification) {
      verification_code <- generateDataVerificationCode(labels)
      if (length(verification_code) > 0) {
        code_lines <- c(code_lines, verification_code)
        debugLog("USER_OPERATIONS_CODE: Added verification code", "CODE_DEBUG")
      }
    }
    
    code_lines <- c(code_lines,
                    "# ============================================================================",
                    ""
    )
    
    result_code <- paste(code_lines, collapse = "\n")
    debugLog(paste("USER_OPERATIONS_CODE: Generated", nchar(result_code), "characters of code"), "CODE_DEBUG")
    return(result_code)
    
  }, error = function(e) {
    debugLog(paste("ERROR in generateExecutableUserOperationsCode:", e$message), "CODE_PROCESS")
    return(paste("# Error generating executable user operations code:", e$message, "\n"))
  })
}

# Generate executable filtering operations with localization and safe string handling
generateExecutableFilteringOperations <- function(filtering_operations, labels) {
  if (length(filtering_operations) == 0) {
    return(c())
  }
  
  # Localized titles and labels
  filteringTitle <- safeGetUILabel("codeCustomFilteringTitle", labels, "CUSTOM MODALITY FILTERING")
  variableLabel <- safeGetUILabel("codeVariableLabel", labels, "Variable")
  filteringMethodLabel <- safeGetUILabel("codeFilteringMethodLabel", labels, "Filtering method")
  filteringReproductionNote <- safeGetUILabel("codeFilteringReproductionNote", labels, "To reproduce this filtering exactly")
  originalModalitiesLabel <- safeGetUILabel("codeOriginalModalitiesLabel", labels, "Original modalities")
  keptModalitiesLabel <- safeGetUILabel("codeKeptModalitiesLabel", labels, "Kept modalities")
  
  # Method descriptions with localization
  smartFilterMethodDescription <- safeGetUILabel("codeSmartFilterMethodDescription", labels, "Keep the N most frequent modalities")
  topNMethodDescription <- safeGetUILabel("codeTopNMethodDescription", labels, "Keep the N top modalities")
  minFreqMethodDescription <- safeGetUILabel("codeMinFreqMethodDescription", labels, "Keep modalities with frequency")
  combinedMethodDescription <- safeGetUILabel("codeCombinedMethodDescription", labels, "Combined - top N among those with frequency")
  autoExcludeMethodDescription <- safeGetUILabel("codeAutoExcludeMethodDescription", labels, "Automatic exclusion of modalities with < 2 observations")
  specificModalitiesMethodDescription <- safeGetUILabel("codeSpecificModalitiesMethodDescription", labels, "Keep specific modalities selected by user")
  
  # Success messages with localization
  filteringAppliedMessage <- safeGetUILabel("codeFilteringAppliedMessage", labels, "Filtering applied for")
  topNFilteringAppliedMessage <- safeGetUILabel("codeTopNFilteringAppliedMessage", labels, "Top-N filtering applied for")
  freqFilteringAppliedMessage <- safeGetUILabel("codeFreqFilteringAppliedMessage", labels, "Frequency filtering applied for")
  combinedFilteringAppliedMessage <- safeGetUILabel("codeCombinedFilteringAppliedMessage", labels, "Combined filtering applied for")
  autoExclusionAppliedMessage <- safeGetUILabel("codeAutoExclusionAppliedMessage", labels, "Automatic exclusion applied for")
  specificFilteringAppliedMessage <- safeGetUILabel("codeSpecificFilteringAppliedMessage", labels, "Specific filtering applied for")
  
  # Error messages with localization
  filteringErrorMessage <- safeGetUILabel("codeFilteringErrorMessage", labels, "Error during filtering for")
  
  code_lines <- c(
    paste("#", filteringTitle),
    paste("#", filteringReproductionNote),
    ""
  )
  
  # Check error handling setting
  include_error_handling <- if (exists("input") && !is.null(input$includeErrorHandling_bar)) {
    input$includeErrorHandling_bar
  } else if (exists("input") && !is.null(input$includeErrorHandling_box)) {
    input$includeErrorHandling_box  
  } else {
    tryCatch(ENHANCED_CODE_CONFIG$include_error_handling, error = function(e) TRUE)
  }
  
  for (op in filtering_operations) {
    var_name <- escapeVarName(op$variable_name)
    safe_var_name <- escapeVariableNameForCode(op$variable_name)
    
    # Localized operation description - USING LOCALIZED VARIABLES
    code_lines <- c(code_lines,
                    paste("#", variableLabel, ":", op$variable_name),
                    paste("#", "-", originalModalitiesLabel, ":", op$data$original_modality_count),
                    paste("#", "-", keptModalitiesLabel, ":", op$data$kept_modality_count),
                    paste("#", filteringMethodLabel, ":", op$data$action)
    )
    
    # Generate executable filtering code based on method using TIDY STYLE
    if (op$data$action == "smart_filter" && !is.null(op$data$parameters$top_n)) {
      code_lines <- c(code_lines,
                      paste("#", smartFilterMethodDescription, ":", op$data$parameters$top_n)
      )
      
      if (include_error_handling) {
        code_lines <- c(code_lines,
                        "tryCatch({",
                        paste0("  data <- data %>%"),
                        paste0("    add_count(", var_name, ", name = 'freq') %>%"),
                        paste0("    filter(freq >= 2) %>%"),
                        paste0("    group_by(", var_name, ") %>%"),
                        paste0("    slice_max(freq, n = ", op$data$parameters$top_n, ") %>%"),
                        paste0("    ungroup()"),
                        paste0("  cat('✓ ", escapeSingleQuotesInLabel(filteringAppliedMessage), " ", escapeSingleQuotesInLabel(safe_var_name), "\\n')"),
                        "}, error = function(e) {",
                        paste0("  warning('", escapeSingleQuotesInLabel(filteringErrorMessage), " ", escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                        "})"
        )
      } else {
        code_lines <- c(code_lines,
                        paste0("data <- data %>%"),
                        paste0("  add_count(", var_name, ", name = 'freq') %>%"),
                        paste0("  filter(freq >= 2) %>%"),
                        paste0("  group_by(", var_name, ") %>%"),
                        paste0("  slice_max(freq, n = ", op$data$parameters$top_n, ") %>%"),
                        paste0("  ungroup()")
        )
      }
      
    } else if (op$data$action == "custom_filter") {
      if (op$data$filtering_method == "top_n" && !is.null(op$data$parameters$topN)) {
        code_lines <- c(code_lines,
                        paste("#", topNMethodDescription, ":", op$data$parameters$topN)
        )
        
        if (include_error_handling) {
          code_lines <- c(code_lines,
                          "tryCatch({",
                          paste0("  data <- data %>%"),
                          paste0("    add_count(", var_name, ", name = 'freq') %>%"),
                          paste0("    group_by(", var_name, ") %>%"),
                          paste0("    slice_max(freq, n = ", op$data$parameters$topN, ") %>%"),
                          paste0("    ungroup()"),
                          paste0("  cat('✓ ", escapeSingleQuotesInLabel(topNFilteringAppliedMessage), " ", escapeSingleQuotesInLabel(safe_var_name), "\\n')"),
                          "}, error = function(e) {",
                          paste0("  warning('", escapeSingleQuotesInLabel(filteringErrorMessage), " ", escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                          "})"
          )
        } else {
          code_lines <- c(code_lines,
                          paste0("data <- data %>%"),
                          paste0("  add_count(", var_name, ", name = 'freq') %>%"),
                          paste0("  group_by(", var_name, ") %>%"),
                          paste0("  slice_max(freq, n = ", op$data$parameters$topN, ") %>%"),
                          paste0("  ungroup()")
          )
        }
        
      } else if (op$data$filtering_method == "min_freq" && !is.null(op$data$parameters$minFreq)) {
        code_lines <- c(code_lines,
                        paste("#", minFreqMethodDescription, " >=", op$data$parameters$minFreq)
        )
        
        if (include_error_handling) {
          code_lines <- c(code_lines,
                          "tryCatch({",
                          paste0("  data <- data %>%"),
                          paste0("    add_count(", var_name, ", name = 'freq') %>%"),
                          paste0("    filter(freq >= ", op$data$parameters$minFreq, ") %>%"),
                          paste0("    select(-freq)"),
                          paste0("  cat('✓ ", escapeSingleQuotesInLabel(freqFilteringAppliedMessage), " ", escapeSingleQuotesInLabel(safe_var_name), "\\n')"),
                          "}, error = function(e) {",
                          paste0("  warning('", escapeSingleQuotesInLabel(filteringErrorMessage), " ", escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                          "})"
          )
        } else {
          code_lines <- c(code_lines,
                          paste0("data <- data %>%"),
                          paste0("  add_count(", var_name, ", name = 'freq') %>%"),
                          paste0("  filter(freq >= ", op$data$parameters$minFreq, ") %>%"),
                          paste0("  select(-freq)")
          )
        }
        
      } else if (op$data$filtering_method == "combined" && 
                 !is.null(op$data$parameters$topN) && 
                 !is.null(op$data$parameters$minFreq)) {
        code_lines <- c(code_lines,
                        paste("#", combinedMethodDescription, op$data$parameters$topN, 
                              ">=", op$data$parameters$minFreq)
        )
        
        if (include_error_handling) {
          code_lines <- c(code_lines,
                          "tryCatch({",
                          paste0("  data <- data %>%"),
                          paste0("    add_count(", var_name, ", name = 'freq') %>%"),
                          paste0("    filter(freq >= ", op$data$parameters$minFreq, ") %>%"),
                          paste0("    group_by(", var_name, ") %>%"),
                          paste0("    slice_max(freq, n = ", op$data$parameters$topN, ") %>%"),
                          paste0("    ungroup() %>%"),
                          paste0("    select(-freq)"),
                          paste0("  cat('✓ ", escapeSingleQuotesInLabel(combinedFilteringAppliedMessage), " ", escapeSingleQuotesInLabel(safe_var_name), "\\n')"),
                          "}, error = function(e) {",
                          paste0("  warning('", escapeSingleQuotesInLabel(filteringErrorMessage), " ", escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                          "})"
          )
        } else {
          code_lines <- c(code_lines,
                          paste0("data <- data %>%"),
                          paste0("  add_count(", var_name, ", name = 'freq') %>%"),
                          paste0("  filter(freq >= ", op$data$parameters$minFreq, ") %>%"),
                          paste0("  group_by(", var_name, ") %>%"),
                          paste0("  slice_max(freq, n = ", op$data$parameters$topN, ") %>%"),
                          paste0("  ungroup() %>%"),
                          paste0("  select(-freq)")
          )
        }
      }
      
    } else if (op$data$action == "auto_exclude") {
      code_lines <- c(code_lines,
                      paste("#", autoExcludeMethodDescription)
      )
      
      if (include_error_handling) {
        code_lines <- c(code_lines,
                        "tryCatch({",
                        paste0("  data <- data %>%"),
                        paste0("    add_count(", var_name, ", name = 'freq') %>%"),
                        paste0("    filter(freq >= 2) %>%"),
                        paste0("    select(-freq)"),
                        paste0("  cat('✓ ", escapeSingleQuotesInLabel(autoExclusionAppliedMessage), " ", escapeSingleQuotesInLabel(safe_var_name), "\\n')"),
                        "}, error = function(e) {",
                        paste0("  warning('", escapeSingleQuotesInLabel(filteringErrorMessage), " ", escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                        "})"
        )
      } else {
        code_lines <- c(code_lines,
                        paste0("data <- data %>%"),
                        paste0("  add_count(", var_name, ", name = 'freq') %>%"),
                        paste0("  filter(freq >= 2) %>%"),
                        paste0("  select(-freq)")
        )
      }
      
    } else if (!is.null(op$data$exact_kept_modalities)) {
      # Fallback: use exact modalities when available - TIDY STYLE + SAFE STRINGS
      # Escape single quotes in modality values
      safe_modalities <- sapply(op$data$exact_kept_modalities, function(mod) {
        paste0("'", gsub("'", "\\\\'", mod, fixed = TRUE), "'")
      })
      modalities_code <- paste0("c(", paste(safe_modalities, collapse = ", "), ")")
      
      code_lines <- c(code_lines,
                      paste("#", specificModalitiesMethodDescription)
      )
      
      if (include_error_handling) {
        code_lines <- c(code_lines,
                        "tryCatch({",
                        paste0("  data <- data %>%"),
                        paste0("    filter(", var_name, " %in% ", modalities_code, ")"),
                        paste0("  cat('✓ ", escapeSingleQuotesInLabel(specificFilteringAppliedMessage), " ", escapeSingleQuotesInLabel(safe_var_name), "\\n')"),
                        "}, error = function(e) {",
                        paste0("  warning('", escapeSingleQuotesInLabel(filteringErrorMessage), " ", escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                        "})"
        )
      } else {
        code_lines <- c(code_lines,
                        paste0("data <- data %>%"),
                        paste0("  filter(", var_name, " %in% ", modalities_code, ")")
        )
      }
    }
    
    code_lines <- c(code_lines, "")
  }
  
  return(code_lines)
}

# Generate executable NA handling operations with localization
generateExecutableNAHandlingOperations <- function(na_operations, labels) {
  if (length(na_operations) == 0) {
    return(c())
  }
  
  # Localized titles and labels
  naHandlingTitle <- safeGetUILabel("codeNAHandlingTitle", labels, "NA VALUES HANDLING")
  naHandlingNote <- safeGetUILabel("codeNAHandlingNote", labels, "Operations applied to handle undefined values (NA)")
  variableLabel <- safeGetUILabel("codeVariableLabel", labels, "Variable")
  naCountLabel <- safeGetUILabel("codeNACountLabel", labels, "NA values")
  handlingMethodLabel <- safeGetUILabel("codeHandlingMethodLabel", labels, "Handling method")
  
  # Method descriptions
  filterNADescription <- safeGetUILabel("codeFilterNADescription", labels, "Filtered rows with NA values")
  assignCodeDescription <- safeGetUILabel("codeAssignCodeDescription", labels, "Assigned custom code to NA values")
  keepNADescription <- safeGetUILabel("codeKeepNADescription", labels, "Kept NA as separate category")
  
  # Success messages
  filterNASuccess <- safeGetUILabel("codeFilterNASuccess", labels, "Rows with NA filtered")
  assignCodeSuccess <- safeGetUILabel("codeAssignCodeSuccess", labels, "Custom code assigned to NA")
  keepNASuccess <- safeGetUILabel("codeKeepNASuccess", labels, "NA kept as category")
  
  # Error messages
  naHandlingError <- safeGetUILabel("codeNAHandlingError", labels, "Error during NA handling for")
  
  code_lines <- c(
    paste("#", naHandlingTitle),
    paste("#", naHandlingNote),
    ""
  )
  
  # Check error handling setting
  include_error_handling <- if (exists("input") && !is.null(input$includeErrorHandling_bar)) {
    input$includeErrorHandling_bar
  } else if (exists("input") && !is.null(input$includeErrorHandling_box)) {
    input$includeErrorHandling_box
  } else {
    tryCatch(ENHANCED_CODE_CONFIG$include_error_handling, error = function(e) TRUE)
  }
  
  for (op in na_operations) {
    var_name <- escapeVarName(op$variable_name)
    safe_var_name <- escapeVariableNameForCode(op$variable_name)
    
    # Operation description
    code_lines <- c(code_lines,
                    paste("#", variableLabel, ":", op$variable_name),
                    paste("#", "-", naCountLabel, ":", op$data$na_count),
                    paste("#", handlingMethodLabel, ":", op$data$action)
    )
    
    # Generate executable code based on handling method
    if (op$data$action == "filter_na") {
      code_lines <- c(code_lines,
                      paste("#", filterNADescription)
      )
      
      if (include_error_handling) {
        code_lines <- c(code_lines,
                        "tryCatch({",
                        paste0("  data <- data %>% filter(!is.na(", var_name, "))"),
                        paste0("  cat('✓ ", escapeSingleQuotesInLabel(filterNASuccess), " ", 
                               escapeSingleQuotesInLabel(safe_var_name), "\\n')"),
                        "}, error = function(e) {",
                        paste0("  warning('", escapeSingleQuotesInLabel(naHandlingError), " ", 
                               escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                        "})"
        )
      } else {
        code_lines <- c(code_lines,
                        paste0("data <- data %>% filter(!is.na(", var_name, "))")
        )
      }
      
    } else if (op$data$action == "assign_code") {
      custom_label <- op$data$custom_label
      safe_label <- gsub("'", "\\\\'", custom_label, fixed = TRUE)
      
      code_lines <- c(code_lines,
                      paste("#", assignCodeDescription, ":", custom_label)
      )
      
      if (include_error_handling) {
        code_lines <- c(code_lines,
                        "tryCatch({",
                        paste0("  data <- data %>% mutate(", var_name, " = ifelse(is.na(", 
                               var_name, "), '", safe_label, "', as.character(", var_name, ")))"),
                        paste0("  cat('✓ ", escapeSingleQuotesInLabel(assignCodeSuccess), " ", 
                               escapeSingleQuotesInLabel(safe_var_name), ": ", safe_label, "\\n')"),
                        "}, error = function(e) {",
                        paste0("  warning('", escapeSingleQuotesInLabel(naHandlingError), " ", 
                               escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                        "})"
        )
      } else {
        code_lines <- c(code_lines,
                        paste0("data <- data %>% mutate(", var_name, " = ifelse(is.na(", 
                               var_name, "), '", safe_label, "', as.character(", var_name, ")))")
        )
      }
      
    } else if (op$data$action == "keep_na") {
      code_lines <- c(code_lines,
                      paste("#", keepNADescription)
      )
      
      if (include_error_handling) {
        code_lines <- c(code_lines,
                        "tryCatch({",
                        paste0("  data <- data %>% mutate(", var_name, " = addNA(factor(", var_name, ")))"),
                        paste0("  cat('✓ ", escapeSingleQuotesInLabel(keepNASuccess), " ", 
                               escapeSingleQuotesInLabel(safe_var_name), "\\n')"),
                        "}, error = function(e) {",
                        paste0("  warning('", escapeSingleQuotesInLabel(naHandlingError), " ", 
                               escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                        "})"
        )
      } else {
        code_lines <- c(code_lines,
                        paste0("data <- data %>% mutate(", var_name, " = addNA(factor(", var_name, ")))")
        )
      }
    }
    
    code_lines <- c(code_lines, "")
  }
  
  return(code_lines)
}

# Generate executable numeric conversions with localization and safety
generateExecutableNumericConversions <- function(conversion_operations, labels) {
  if (length(conversion_operations) == 0) {
    return(c())
  }
  
  # Localized labels
  numericTitle <- safeGetUILabel("codeNumericConversionsTitle", labels, "NUMERIC VARIABLE CONVERSIONS")
  numericConversionNote <- safeGetUILabel("codeNumericConversionNote", labels, "This conversion preserves the exact ordering used in the application")
  variableLabel <- safeGetUILabel("codeVariableLabel", labels, "Variable")
  originalValuesLabel <- safeGetUILabel("codeOriginalValuesLabel", labels, "Original unique values")
  conversionMethodLabel <- safeGetUILabel("codeConversionMethodLabel", labels, "Conversion method")
  thresholdUsedLabel <- safeGetUILabel("codeThresholdUsedLabel", labels, "Threshold used")
  factorOrderPreservationLabel <- safeGetUILabel("codeFactorOrderPreservationLabel", labels, "Factor with preserved numeric ordering")
  successMessageLabel <- safeGetUILabel("codeSuccessMessage", labels, "Successfully applied operation")
  errorMessageLabel <- safeGetUILabel("codeErrorMessage", labels, "Error applying operation")
  exactLevelsUnavailable <- safeGetUILabel("codeExactLevelsUnavailable", labels, "Warning: Exact levels not available, using numeric sort")
  fallbackLabel <- safeGetUILabel("codeFallbackLabel", labels, "Fallback")
  
  code_lines <- c(
    paste("#", numericTitle),
    paste("#", numericConversionNote),
    ""
  )
  
  # Check error handling setting
  include_error_handling <- if (exists("input") && !is.null(input$includeErrorHandling_bar)) {
    input$includeErrorHandling_bar
  } else if (exists("input") && !is.null(input$includeErrorHandling_box)) {
    input$includeErrorHandling_box  
  } else {
    tryCatch(ENHANCED_CODE_CONFIG$include_error_handling, error = function(e) TRUE)
  }
  
  for (op in conversion_operations) {
    var_name <- escapeVarName(op$variable_name)
    safe_var_name <- escapeVariableNameForCode(op$variable_name)
    
    code_lines <- c(code_lines,
                    paste("#", variableLabel, ":", op$variable_name),
                    paste("# -", originalValuesLabel, ":", op$data$unique_values_count),
                    paste("# -", conversionMethodLabel, ":", factorOrderPreservationLabel),
                    paste("# -", thresholdUsedLabel, ":", op$data$threshold_used)
    )
    
    if (!is.null(op$data$exact_levels) && length(op$data$exact_levels) > 0) {
      # Check if levels are numeric (for corrected numeric conversion)
      if (all(grepl("^[0-9.+-]+$", op$data$exact_levels))) {
        # Use unique() instead of sort(as.numeric()) to avoid vector issues
        if (include_error_handling) {
          code_lines <- c(code_lines,
                          "tryCatch({",
                          paste0("  data <- data %>%"),
                          paste0("    mutate(", var_name, " = factor(", var_name, ", levels = as.character(sort(unique(", var_name, ")))))"),
                          paste0("  cat('✓ ", escapeSingleQuotesInLabel(successMessageLabel), " ", escapeSingleQuotesInLabel(safe_var_name), "\\n')"),
                          "}, error = function(e) {",
                          paste0("  warning('", escapeSingleQuotesInLabel(errorMessageLabel), " ", escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                          paste0("  data <- data %>% mutate(", var_name, " = factor(", var_name, "))  # ", escapeSingleQuotesInLabel(fallbackLabel)),
                          "})"
          )
        } else {
          code_lines <- c(code_lines,
                          paste0("data <- data %>%"),
                          paste0("  mutate(", var_name, " = factor(", var_name, ", levels = as.character(sort(unique(", var_name, ")))))")
          )
        }
      } else {
        # Use explicit levels for non-numeric
        levels_code <- paste0("c(", paste0("'", gsub("'", "\\\\'", op$data$exact_levels, fixed = TRUE), "'", collapse = ", "), ")")
        
        if (include_error_handling) {
          code_lines <- c(code_lines,
                          "tryCatch({",
                          paste0("  data <- data %>%"),
                          paste0("    mutate(", var_name, " = factor(", var_name, ", levels = ", levels_code, "))"),
                          paste0("  cat('✓ ", escapeSingleQuotesInLabel(successMessageLabel), " ", escapeSingleQuotesInLabel(safe_var_name), "\\n')"),
                          "}, error = function(e) {",
                          paste0("  warning('", escapeSingleQuotesInLabel(errorMessageLabel), " ", escapeSingleQuotesInLabel(safe_var_name), ": ', e$message)"),
                          paste0("  data <- data %>% mutate(", var_name, " = factor(", var_name, "))  # ", escapeSingleQuotesInLabel(fallbackLabel)),
                          "})"
          )
        } else {
          code_lines <- c(code_lines,
                          paste0("data <- data %>%"),
                          paste0("  mutate(", var_name, " = factor(", var_name, ", levels = ", levels_code, "))")
          )
        }
      }
    } else {
# Fallback when exact levels not available
      code_lines <- c(code_lines,
                      paste0("# ", exactLevelsUnavailable),
                      paste0("data <- data %>%"),
                      paste0("  mutate(", var_name, " = factor(", var_name, ", levels = as.character(sort(unique(", var_name, ")))))")
      )
    }
    
    code_lines <- c(code_lines, "")
  }
  
  return(code_lines)
}

# Generate data verification code with localization
generateDataVerificationCode <- function(labels) {
  verificationTitle <- safeGetUILabel("codeDataVerificationLabel", labels, "Data verification after operations")
  verificationStartMessage <- safeGetUILabel("codeVerificationStartMessage", labels, "Data verification after user operations")
  verificationLevelsText <- safeGetUILabel("codeVerificationLevelsText", labels, "levels")
  verificationRowsText <- safeGetUILabel("codeVerificationRowsText", labels, "rows")
  verificationDatasetSizeText <- safeGetUILabel("codeVerificationDatasetSizeText", labels, "Final dataset size")
  
  return(c(
    paste("#", verificationTitle),
    paste0("cat('", verificationStartMessage, ":\\n')"),
    "for (var_name in names(data)) {",
    "  if (is.factor(data[[var_name]])) {",
    paste0("    cat(paste('- ', var_name, ' : ', nlevels(data[[var_name]]), ' ", verificationLevelsText, "\\n'))"),
    "  }",
    "}",
    paste0("cat(paste('- ", verificationDatasetSizeText, " : ', nrow(data), ' ", verificationRowsText, "\\n'))"),
    ""
  ))
}

# ============================================================================
# Main executable code generation function
# ============================================================================

# Enhanced header generation with complete localization and data loading
generateCodeHeader <- function(input, labels, values = NULL, needsGgridges = FALSE) {
  
  # Localized header elements
  codeHeaderTitle <- safeGetUILabel("codeHeaderTitle", labels, "iPlotDesigner - Generated R Code (Executable Operations)")
  codeGeneratedOnLabel <- safeGetUILabel("codeGeneratedOnLabel", labels, "Generated on")
  codeRVersionTitle <- safeGetUILabel("codeRVersionTitle", labels, "R version and packages versions")
  codeRequiredLibrariesTitle <- safeGetUILabel("codeRequiredLibrariesTitle", labels, "Required libraries")
  codeLoadDataTitle <- safeGetUILabel("codeLoadDataTitle", labels, "Load data")
  codeAdjustPathComment <- safeGetUILabel("codeAdjustPathComment", labels, "Adjust the path and reading function according to your file format")
  
  headerLines <- c(
    "# ============================================================================",
    paste("#", codeHeaderTitle),
    paste("#", codeGeneratedOnLabel, ":", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "# ============================================================================",
    ""
  )
  
  # Add R version info based on configuration (use input values if available)
  include_r_version <- if (!is.null(input$includePlatformInfo_bar)) {
    input$includePlatformInfo_bar
  } else if (!is.null(input$includePlatformInfo_box)) {
    input$includePlatformInfo_box
  } else {
    tryCatch(ENHANCED_CODE_CONFIG$include_r_version, error = function(e) FALSE)
  }
  
  if (include_r_version) {
    # Get R version and only show actually required packages
    r_version <- R.version.string
    platform <- R.version$platform
    
    headerLines <- c(headerLines,
                     paste("#", codeRVersionTitle, ":"),
                     paste("#", r_version),
                     paste("#", platform)
    )
    
    # Show only packages actually required by generated code
    required_packages <- c("ggplot2", "dplyr")
    
    # Add readxl if Excel file was used
    file_ext <- if (!is.null(values$file_extension)) {
      values$file_extension
    } else if (!is.null(values$original_filename)) {
      tools::file_ext(values$original_filename)
    } else {
      ""
    }
    
    if (tolower(file_ext) %in% c("xlsx", "xls")) {
      required_packages <- c(required_packages, "readxl")
    } else if (tolower(file_ext) == "ods") {
      required_packages <- c(required_packages, "readODS")
    }
    
    # Add ggridges if needed
    if(needsGgridges) {
      required_packages <- c(required_packages, "ggridges")
    }
    
    # Get versions only for required packages
    headerLines <- c(headerLines, "# Required packages:")
    for (pkg in required_packages) {
      tryCatch({
        pkg_version <- packageVersion(pkg)
        headerLines <- c(headerLines, paste("#", pkg, pkg_version))
      }, error = function(e) {
        headerLines <- c(headerLines, paste("#", pkg, "(version unavailable)"))
      })
    }
    
    headerLines <- c(headerLines, "")
  }
  
  headerLines <- c(headerLines,
                   paste("#", codeRequiredLibrariesTitle),
                   "library(ggplot2)",
                   "library(dplyr)"
  )
  
  # Get file extension
  file_ext <- if (!is.null(values$file_extension)) {
    values$file_extension
  } else if (!is.null(values$original_filename)) {
    tools::file_ext(values$original_filename)
  } else {
    ""
  }
  
  # Add readxl if Excel file was used, or readODS for an ODS file
  if (tolower(file_ext) %in% c("xlsx", "xls")) {
    headerLines <- c(headerLines, "library(readxl)")
  } else if (tolower(file_ext) == "ods") {
    headerLines <- c(headerLines, "library(readODS)")
  }
  
  # Add ggridges if needed
  if(needsGgridges) {
    headerLines <- c(headerLines, "library(ggridges)")
  }
  
  headerLines <- c(headerLines, "")
  
  # Generate font setup code if non-standard fonts are used
  font_setup_code <- generateFontSetupSection(input, labels)
  if (nchar(font_setup_code) > 0) {
    font_setup_lines <- strsplit(font_setup_code, "\n")[[1]]
    headerLines <- c(headerLines, font_setup_lines)
    debugLog("CODE_GENERATION: Font setup code included in header", "CODE_PROCESS")
  }
  
  # Add data loading section
  if (!is.null(values$original_filename)) {
    headerLines <- c(headerLines,
                     paste("#", codeLoadDataTitle),
                     paste("#", codeAdjustPathComment)
    )
    
    # Generate appropriate loading code based on file type
    if (file_ext %in% c("xlsx", "xls")) {
      headerLines <- c(headerLines,
                       paste0("data <- read_excel('", values$original_filename, "')")
      )
    } else if (file_ext == "tsv") {
      headerLines <- c(headerLines,
                       paste0("data <- read_tsv('", values$original_filename, "')")
      )
    } else {
      # Default to CSV
      headerLines <- c(headerLines,
                       paste0("data <- read_csv('", values$original_filename, "')")
      )
    }
    
    headerLines <- c(headerLines, "")
  }
  
  # Generate executable user operations section
  if (!is.null(values) && 
      exists("USER_OPERATIONS_CONFIG") && 
      USER_OPERATIONS_CONFIG$include_in_code_generation) {
    
    tryCatch({
      user_operations_code <- generateExecutableUserOperationsCode(values, labels)
      if (!is.null(user_operations_code) && nchar(user_operations_code) > 0) {
        # Split the operations code into lines
        operations_lines <- strsplit(user_operations_code, "\n")[[1]]
        
        # Ensure proper spacing: add empty line before and after operations
        headerLines <- c(headerLines, "", operations_lines, "")
        
        debugLog("EXECUTABLE_CODE: User operations included in header with proper spacing", "CODE_PROCESS")
      }
    }, error = function(e) {
      debugLog(paste("EXECUTABLE_CODE: Error including user operations:", e$message), "CODE_PROCESS")
    })
  }
  
  return(paste(headerLines, collapse = "\n"))
}

# ============================================================================
# Code generation functions
# ============================================================================

# Clean variable names for code generation (unchanged)
cleanVariableForCodeGeneration <- function(variable_input, labels) {
  if(is.null(variable_input) || variable_input == "") {
    return(NULL)
  }
  
  noVar2Label <- safeGetUILabel("noIndependentVar2Label", labels, "-- No independent variable 2 --")
  noVar3Label <- safeGetUILabel("noIndependentVar3Label", labels, "-- No independent variable 3 --")
  
  if(variable_input == noVar2Label || variable_input == noVar3Label) {
    return(NULL)
  }
  
  return(variable_input)
}

# Function to generate localized filtering code
generateFilteringCode <- function(var, selected, allModalities, labels) {
  # Skip if no filtering needed (all modalities selected)
  if(is.null(selected) || length(selected) == length(allModalities)) {
    return("")
  }
  
  filterDataComment <- paste0(safeGetUILabel("codeFilterDataComment", labels, "Filter data for"))
  
  # Use tidy style instead of base R
  code <- paste0(
    "# ", filterDataComment, " ", escapeVarName(var), "\n",
    "data <- data %>%\n",
    "  filter(", escapeVarName(var), " %in% c(",
    paste0("'", selected, "'", collapse = ", "),
    "))\n\n"
  )
  
  return(code)
}

# Function to generate localized factor ordering code - OPTIMIZED
generateFactorOrderCode <- function(var, order, selected, labels, values = NULL) {
  # Skip if order is alphabetical (default R behavior) 
  if(is.null(order) || identical(order, sort(order))) {
    return("")
  }
  
  # Check if this variable had numeric conversion and order was not modified
  if (!is.null(values) && !is.null(values$user_operations_history)) {
    numeric_ops <- getUserOperationsByType(values, "numeric_conversion")
    for (op in numeric_ops) {
      if (op$variable_name == var) {
        # Variable had numeric conversion - check if order matches original numeric order
        if (!is.null(op$data$exact_levels)) {
          # Check if current order is identical to the numeric conversion order
          if (identical(order, op$data$exact_levels)) {
            debugLog(paste("FACTOR_ORDER: Skipping", var, "- order matches numeric conversion"), "CODE_PROCESS")
return("")  # Skip - already handled in operations section
          } else {
            debugLog(paste("FACTOR_ORDER: Including", var, "- user modified order after conversion"), "CODE_PROCESS")
            # Continue with factor ordering - user modified the order manually
            break
          }
        }
      }
    }
  }
  
  # Only include selected modalities in factor levels
  relevantOrder <- if(!is.null(selected)) {
    order[order %in% selected]
  } else {
    order
  }
  
  # Skip if no relevant modalities
  if(length(relevantOrder) == 0) {
    return("")
  }
  
  setFactorLevelsComment <- paste0("# ", safeGetUILabel("codeSetFactorLevelsComment", labels, "Set factor levels for"))
  userModifiedOrderNote <- paste0("# ", safeGetUILabel("codeUserModifiedOrderNote", labels, "Order modified by user after conversion"))
  
  # Add note if this is a user-modified order after numeric conversion
  note_lines <- if (!is.null(values) && !is.null(values$user_operations_history)) {
    numeric_ops <- getUserOperationsByType(values, "numeric_conversion")
    is_modified_numeric <- any(sapply(numeric_ops, function(op) op$variable_name == var))
    if (is_modified_numeric) {
      c(userModifiedOrderNote, "")
    } else {
      c()
    }
  } else {
    c()
  }
  
  code_lines <- c(
    note_lines,
    paste0(setFactorLevelsComment, " ", escapeVarName(var)),
    paste0("data$", escapeVarName(var), " <- factor(data$", escapeVarName(var), ", levels = c("),
    paste0("  ", paste0("'", relevantOrder, "'", collapse = ", ")),
    "))", ""
  )
  
  return(paste(code_lines, collapse = "\n"))
}

# Function to generate color scale code
generateColorScaleCode <- function(var, all_colors, selected_modalities = NULL, fill = TRUE) {
  aesthetic <- if(fill) "fill" else "color"
  
  # Filter colors to only include selected/displayed modalities
  if (!is.null(selected_modalities) && !is.null(all_colors)) {
    # Keep only colors for modalities that are actually selected
    filtered_colors <- all_colors[names(all_colors) %in% selected_modalities]
    
    # Ensure we have colors for all selected modalities
    missing_modalities <- selected_modalities[!selected_modalities %in% names(filtered_colors)]
    if (length(missing_modalities) > 0) {
      # Add default colors for missing modalities
      for (i in seq_along(missing_modalities)) {
        modality <- missing_modalities[i]
        default_index <- (length(filtered_colors) + i - 1) %% length(defaultColors$var1) + 1
        filtered_colors[modality] <- defaultColors$var1[default_index]
      }
    }
    
    colors_to_use <- filtered_colors
  } else {
    # Fallback to all colors if no filtering info available
    colors_to_use <- all_colors
  }
  
  # Generate the scale code
  paste0(
    "  scale_", aesthetic, "_manual(values = c(",
    paste0("'", colors_to_use, "'", collapse = ", "),
    "))"
  )
}

# Enhanced orchestrator for generateThemeCode
# Collects all theme elements and combines them into a single theme() call
# with category comments inside
generateThemeCode <- function(input, labels) {
  
  tryCatch({
    all_elements <- list()  # Named list: comment => elements vector
    
    # 1. Base/General parameters
    base_result <- generateThemeBaseElements(input, labels)
    if(!is.null(base_result) && length(base_result$elements) > 0) {
      all_elements[[base_result$comment]] <- base_result$elements
    }
    
    # 2. Backgrounds
    bg_result <- generateThemeBackgroundsElements(input, labels)
    if(!is.null(bg_result) && length(bg_result$elements) > 0) {
      all_elements[[bg_result$comment]] <- bg_result$elements
    }
    
    # 3. Titles
    titles_result <- generateThemeTitlesElements(input, labels)
    if(!is.null(titles_result) && length(titles_result$elements) > 0) {
      all_elements[[titles_result$comment]] <- titles_result$elements
    }
    
    # 4. Axis text
    axis_result <- generateThemeAxisElements(input, labels)
    if(!is.null(axis_result) && length(axis_result$elements) > 0) {
      all_elements[[axis_result$comment]] <- axis_result$elements
    }
    
    # 5. Grids and borders
    grids_result <- generateThemeGridsElements(input, labels)
    if(!is.null(grids_result) && length(grids_result$elements) > 0) {
      all_elements[[grids_result$comment]] <- grids_result$elements
    }
    
    # Build combined code
    if(length(all_elements) == 0) {
      # No customizations: just theme_light()
      base_comment <- safeGetUILabel("codeCommentBaseTheme", labels, "Base theme")
      return(paste0("  # ", base_comment, "\n  theme_light()"))
    }
    
    # Start with theme_light()
    base_comment <- safeGetUILabel("codeCommentBaseTheme", labels, "Base theme")
    doc_comment <- safeGetUILabel("themeCodeDocumentationComment", labels, "Theme customization - See ggplot2 documentation:")
    
    code_lines <- c(
      paste0("  # ", doc_comment),
      "  # https://ggplot2.tidyverse.org/reference/theme.html",
      paste0("  # ", base_comment),
      "  theme_light() +",
      "  theme("
    )
    
    # Add elements grouped by category with comments
    all_theme_lines <- c()
    
    for(comment in names(all_elements)) {
      elements <- all_elements[[comment]]
      if(length(elements) > 0) {
        # Add category comment
        all_theme_lines <- c(all_theme_lines, paste0("# ", comment))
        # Add elements
        all_theme_lines <- c(all_theme_lines, elements)
      }
    }
    
    # Add proper indentation and commas
    for(i in seq_along(all_theme_lines)) {
      line <- all_theme_lines[i]
      
      # Add indentation
      if(grepl("^#", line)) {
        # Comment line: 4 spaces
        line <- paste0("    ", line)
      } else {
        # Element line: 4 spaces
        line <- paste0("    ", line)
      }
      
      # Add comma after element lines (not comments, not last line)
      if(!grepl("^\\s*#", line) && i < length(all_theme_lines)) {
        line <- paste0(line, ",")
      }
      
      code_lines <- c(code_lines, line)
    }
    
    code_lines <- c(code_lines, "  )")
    
    return(paste(code_lines, collapse = "\n"))
    
  }, error = function(e) {
    debugLog(paste("ERROR in generateThemeCode:", e$message), "CODE_ERROR")
    base_comment <- safeGetUILabel("codeCommentBaseTheme", labels, "Base theme")
    return(paste0("  # ", base_comment, "\n  theme_light()"))
  })
}

# Function to generate localized facet code
generateFacetCode <- function(var2, var3, labels, var2_used_for_fill = FALSE) {
  # Clean variables
  cleanVar2 <- cleanVariableForCodeGeneration(var2, labels)
  cleanVar3 <- cleanVariableForCodeGeneration(var3, labels)
  
  # If var2 is used for fill/color, don't use it for faceting
  if (var2_used_for_fill) {
    cleanVar2 <- NULL
  }
  
  # Generate facet code based on available variables
  if(!is.null(cleanVar3) && cleanVar3 != "") {
    if(!is.null(cleanVar2) && cleanVar2 != "") {
      # Both var2 and var3 available: use facet_grid
      paste0("  facet_grid(", escapeVarName(cleanVar2), " ~ ", escapeVarName(cleanVar3), ")")
    } else {
      # Only var3: use facet_wrap
      paste0("  facet_wrap(vars(", escapeVarName(cleanVar3), "))")
    }
  } else if(!is.null(cleanVar2) && cleanVar2 != "") {
    # Only var2: use facet_wrap
    paste0("  facet_wrap(vars(", escapeVarName(cleanVar2), "))")
  } else {
    NULL
  }
}

# Function to generate localized save code with intelligent naming
generateSaveCode <- function(baseFilename, input, labels, var_info = NULL) {
  displayPlotComment <- paste0("# ", safeGetUILabel("codeDisplayPlotComment", labels, "Display plot"))
  savePlotComment <- paste0("# ", safeGetUILabel("codeSavePlotComment", labels, "Save plot"))
  
  # Get export format and extension
  exportFormat <- if(!is.null(input$exportFormat)) input$exportFormat else "PNG"
  formatExtensions <- c("PNG" = ".png", "PDF" = ".pdf", "EPS" = ".eps", "SVG" = ".svg")
  fileExtension <- formatExtensions[exportFormat]
  if(is.null(fileExtension)) fileExtension <- ".png"
  
  # Use centralized intelligent filename generation
  finalFilename <- generateIntelligentFilename(baseFilename, input, fileExtension, labels)
  
  # Build ggsave parameters
  ggsaveParams <- paste0(
    "'", finalFilename, "', p, width = ", input$exportWidth,
    ", height = ", input$exportHeight, ", units = '", input$exportUnits, "'"
  )
  
  # Add device parameter for non-default formats
  if(exportFormat != "PNG") {
    deviceName <- switch(exportFormat,
                         "PDF" = "cairo_pdf",
                         "EPS" = "eps", 
                         "SVG" = "svg",
                         "png"
    )
    ggsaveParams <- paste0(ggsaveParams, ", device = '", deviceName, "'")
  }
  
  paste0(
    "\n", displayPlotComment, "\nprint(p)\n\n",
    savePlotComment, "\nggsave(", ggsaveParams, ")"
  )
}
