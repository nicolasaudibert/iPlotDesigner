# Label System Core
# Core label access functions with proper fallbacks

# Hard-coded emergency fallbacks (always available)
EMERGENCY_LABEL_FALLBACKS <- list(
  "noIndependentVar1Label" = "-- No independent variable 1 --",
  "noIndependentVar2Label" = "-- No independent variable 2 --",
  "noIndependentVar3Label" = "-- No independent variable 3 --",
  "independentVar1Label" = "Independent variable 1",
  "independentVar2Label" = "Independent variable 2",
  "independentVar3Label" = "Independent variable 3 (faceting)",
  "dependentVarLabel" = "Dependent variable",
  "invalidVariableTitle" = "Invalid Variable Selection",
  "invalidVariableMessage" = "Variable cannot be used for grouping",
  "numericVariableError" = "Numeric variables cannot be used for grouping.",
  "insufficientDataError" = "This variable has no categories with sufficient data for analysis.",
  "okLabel" = "OK",
  "errorTitle" = "Error",
  "variable" = "Variable"
)

# Base function getLabelOrPrompt with fallback logic
getLabelOrPrompt <- function(code, labelsAndPromptsReference) {
  # Detect potential cascade and prevent immediately
  callStack <- sys.calls()
  if(length(callStack) > 20) {
    debugLog(paste("*** CASCADE PREVENTION *** - Depth:", length(callStack), "for code:", code), "CASCADE_WARN")
    
    # Use emergency fallback immediately
    if(code %in% names(EMERGENCY_LABEL_FALLBACKS)) {
      return(EMERGENCY_LABEL_FALLBACKS[[code]])
    }
    
    return(paste("Deep call prevented:", code))
  }
  
  debugLog(paste("getLabelOrPrompt ENTRY: code=", code, "stack_depth=", length(callStack)), "CACHE_TRACE")
  
  # Try cache first if available
  if(exists("getCachedLabel", envir = .GlobalEnv)) {
    tryCatch({
      cached_result <- getCachedLabel(code, labelsAndPromptsReference, NULL)
      if(!is.null(cached_result) && !grepl("^Cascade prevented:", cached_result)) {
        debugLog(paste("getLabelOrPrompt CACHE SUCCESS: code=", code), "CACHE_TRACE")
        return(cached_result)
      }
    }, error = function(e) {
      debugLog(paste("getLabelOrPrompt CACHE ERROR:", e$message), "CACHE_ERROR")
    })
  }
  
  # Try direct lookup
  result <- tryCatch({
    if(is.null(code) || code == "" || is.null(labelsAndPromptsReference)) {
      stop("Invalid parameters")
    }
    
    if(!"entry" %in% names(labelsAndPromptsReference) || !"displayedLabel" %in% names(labelsAndPromptsReference)) {
      stop("Invalid labels structure")
    }
    
    selectedRow <- labelsAndPromptsReference[labelsAndPromptsReference$entry == code, ]
    if(nrow(selectedRow) > 0 && !is.na(selectedRow$displayedLabel[1])) {
      selectedRow$displayedLabel[1]
    } else {
      stop("Label not found")
    }
  }, error = function(e) {
    debugLog(paste("getLabelOrPrompt ERROR for code:", code, "error:", e$message), "CACHE_ERROR")
    
    # Use emergency fallback
    if(code %in% names(EMERGENCY_LABEL_FALLBACKS)) {
      return(EMERGENCY_LABEL_FALLBACKS[[code]])
    }
    
    return(paste("Error:", code))
  })
  
  debugLog(paste("getLabelOrPrompt SUCCESS: code=", code, "result=", result), "CACHE_TRACE")
  return(result)
}

# cleanVariableName with safe fallbacks
cleanVariableName <- function(varName, labels) {
  if(is.null(varName) || varName == "") {
    return(NULL)
  }
  
  # Safe fallback labels
  noVar2Label <- tryCatch({
    if(exists("getCachedLabel", envir = .GlobalEnv)) {
      getCachedLabel("noIndependentVar2Label")
    } else {
      EMERGENCY_LABEL_FALLBACKS[["noIndependentVar2Label"]]
    }
  }, error = function(e) {
    EMERGENCY_LABEL_FALLBACKS[["noIndependentVar2Label"]]
  })
  
  noVar3Label <- tryCatch({
    if(exists("getCachedLabel", envir = .GlobalEnv)) {
      getCachedLabel("noIndependentVar3Label")
    } else {
      EMERGENCY_LABEL_FALLBACKS[["noIndependentVar3Label"]]
    }
  }, error = function(e) {
    EMERGENCY_LABEL_FALLBACKS[["noIndependentVar3Label"]]
  })
  
  if(varName == noVar2Label || varName == noVar3Label) {
    return(NULL)
  }
  
  return(varName)
}

# checkDuplicateVariables with safe label access
checkDuplicateVariables <- function(var1, var2, var3, labels) {
  cleanVar1 <- if(is.null(var1) || var1 == "") NULL else var1
  
  # Use safe cleaning with fallbacks
  cleanVar2 <- tryCatch({
    cleanVariableName(var2, labels)
  }, error = function(e) {
    debugLog(paste("ERROR in checkDuplicateVariables cleaning var2:", e$message), "DATA_ERROR")
    NULL
  })
  
  cleanVar3 <- tryCatch({
    cleanVariableName(var3, labels)
  }, error = function(e) {
    debugLog(paste("ERROR in checkDuplicateVariables cleaning var3:", e$message), "DATA_ERROR")
    NULL
  })
  
  allVariables <- list(cleanVar1, cleanVar2, cleanVar3)
  variables <- unlist(allVariables[!sapply(allVariables, function(x) is.null(x) || x == "")])
  
  if(length(variables) <= 1) {
    return(list(
      hasDuplicates = FALSE,
      duplicateCount = 0,
      allIdentical = FALSE,
      shouldShowWarning = FALSE,
      shouldDisableHistDensity = FALSE
    ))
  }
  
  duplicateCount <- length(variables) - length(unique(variables))
  hasDuplicates <- duplicateCount > 0
  allIdentical <- length(variables) == 3 && length(unique(variables)) == 1
  
  return(list(
    hasDuplicates = hasDuplicates,
    duplicateCount = duplicateCount + 1,
    allIdentical = allIdentical,
    shouldShowWarning = hasDuplicates,
    shouldDisableHistDensity = allIdentical,
    variables = variables,
    uniqueVariables = unique(variables)
  ))
}
