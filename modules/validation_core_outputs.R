# Core validation outputs module
# Main validation reactive outputs with error protection and swap support

# Core plot data validation with natural reactive dependencies
output$plotDataValidation <- reactive({
  # Force re-evaluation after swap operations
  swap_trigger <- values[["swap_completed_trigger"]]
  
  # Direct reactive dependencies
  current_dep_var <- input$dependentVar
  current_ind_var1 <- input$independentVar1
  current_ind_var2 <- input$independentVar2
  current_ind_var3 <- input$independentVar3
  
  # Direct dependencies on variable properties for immediate propagation
  var1_selected <- values$var1_selected
  var2_selected <- values$var2_selected
  var3_selected <- values$var3_selected
  var1_order <- values$var1_order
  var2_order <- values$var2_order
  var3_order <- values$var3_order
  
  tryCatch({
    debugLog("plotDataValidation - Starting natural reactive validation", "VALIDATION_DEBUG")
    
    # Basic validation - early exit if missing required variables
    if(is.null(current_dep_var) || current_dep_var == "" ||
       is.null(current_ind_var1) || current_ind_var1 == "") {
      debugLog("plotDataValidation - Basic variables missing", "VALIDATION_DEBUG")
      return(FALSE)
    }
    
    # Clean variables using cached labels to avoid cascades
    cleanVars <- tryCatch({
      getCleanVariables(list(
        dependentVar = current_dep_var,
        independentVar1 = current_ind_var1,
        independentVar2 = current_ind_var2,
        independentVar3 = current_ind_var3
      ), labels)
    }, error = function(e) {
      debugLog(paste("plotDataValidation - Error cleaning vars:", e$message), "VALIDATION_ERROR")
      return(NULL)
    })
    
    if(is.null(cleanVars)) {
      return(FALSE)
    }
    
    # Individual variable validation with error protection
    var1Validation <- tryCatch({
      validateGroupingVariable(values$filteredData, cleanVars$indVar1)
    }, error = function(e) {
      debugLog(paste("plotDataValidation - Var1 validation error:", e$message), "VALIDATION_ERROR")
      list(isValid = FALSE, reason = "Validation error")
    })
    
    if(!var1Validation$isValid) {
      debugLog(paste("plotDataValidation - Var1 invalid:", var1Validation$reason), "VALIDATION_DEBUG")
      return(FALSE)
    }
    
    # Var2 validation if present
    if(!is.null(cleanVars$indVar2)) {
      var2Validation <- tryCatch({
        validateGroupingVariable(values$filteredData, cleanVars$indVar2)
      }, error = function(e) {
        debugLog(paste("plotDataValidation - Var2 validation error:", e$message), "VALIDATION_ERROR")
        list(isValid = FALSE, reason = "Validation error")
      })
      
      if(!var2Validation$isValid) {
        debugLog(paste("plotDataValidation - Var2 invalid:", var2Validation$reason), "VALIDATION_DEBUG")
        return(FALSE)
      }
    }
    
    # Var3 validation if present
    if(!is.null(cleanVars$indVar3)) {
      var3Validation <- tryCatch({
        validateGroupingVariable(values$filteredData, cleanVars$indVar3)
      }, error = function(e) {
        debugLog(paste("plotDataValidation - Var3 validation error:", e$message), "VALIDATION_ERROR")
        list(isValid = FALSE, reason = "Validation error")
      })
      
      if(!var3Validation$isValid) {
        debugLog(paste("plotDataValidation - Var3 invalid:", var3Validation$reason), "VALIDATION_DEBUG")
        return(FALSE)
      }
    }
    
    # Final combination validation with error protection
    combinationValidation <- tryCatch({
      validateSelectedCombination(
        values$filteredData,
        current_dep_var,
        cleanVars$indVar1,
        cleanVars$indVar2,
        cleanVars$indVar3,
        var1_selected,
        var2_selected,
        var3_selected
      )
    }, error = function(e) {
      debugLog(paste("plotDataValidation - Combination validation error:", e$message), "VALIDATION_ERROR")
      list(isValid = FALSE, dataCount = 0)
    })
    
    debugLog(paste("plotDataValidation - Final validation:", 
             combinationValidation$isValid, "with", combinationValidation$dataCount, "points"), "VALIDATION_DEBUG")
    
    return(combinationValidation$isValid)
    
  }, error = function(e) {
    debugLog(paste("plotDataValidation - Global error:", e$message), "VALIDATION_ERROR")
    return(FALSE)
  })
})
outputOptions(output, 'plotDataValidation', suspendWhenHidden = FALSE)

# Histogram validation with duplicate detection
output$histogramDataValidation <- reactive({
  # Force re-evaluation after swap operations
  swap_trigger <- values[["swap_completed_trigger"]]
  
  # Direct reactive dependencies
  current_dep_var <- input$dependentVar
  current_ind_var1 <- input$independentVar1
  current_ind_var2 <- input$independentVar2
  current_ind_var3 <- input$independentVar3
  var1_selected <- values$var1_selected
  var2_selected <- values$var2_selected
  var3_selected <- values$var3_selected
  
  tryCatch({
    debugLog("histogramDataValidation - Starting natural reactive validation", "VALIDATION_DEBUG")
    
    if(is.null(current_dep_var) || current_dep_var == "" ||
       is.null(current_ind_var1) || current_ind_var1 == "") {
      return(FALSE)
    }
    
    cleanVars <- tryCatch({
      getCleanVariables(list(
        dependentVar = current_dep_var,
        independentVar1 = current_ind_var1,
        independentVar2 = current_ind_var2,
        independentVar3 = current_ind_var3
      ), labels)
    }, error = function(e) {
      debugLog(paste("histogramDataValidation - Error cleaning vars:", e$message), "VALIDATION_ERROR")
      return(NULL)
    })
    
    if(is.null(cleanVars)) {
      return(FALSE)
    }
    
    # Check for duplicates (disable histogram if all identical)
    duplicateInfo <- tryCatch({
      checkDuplicateVariables(
        current_ind_var1,
        current_ind_var2,
        current_ind_var3,
        labels
      )
    }, error = function(e) {
      debugLog(paste("histogramDataValidation - Duplicate check error:", e$message), "VALIDATION_ERROR")
      list(shouldDisableHistDensity = TRUE)
    })
    
    if(duplicateInfo$shouldDisableHistDensity) {
      return(FALSE)
    }
    
    # Variable validation with protection
    var1Validation <- tryCatch({
      validateGroupingVariable(values$filteredData, cleanVars$indVar1)
    }, error = function(e) {
      debugLog(paste("histogramDataValidation - Var1 error:", e$message), "VALIDATION_ERROR")
      list(isValid = FALSE)
    })
    
    if(!var1Validation$isValid) {
      return(FALSE)
    }
    
    combinationValidation <- tryCatch({
      validateSelectedCombination(
        values$filteredData,
        current_dep_var,
        cleanVars$indVar1,
        cleanVars$indVar2,
        cleanVars$indVar3,
        var1_selected,
        var2_selected,
        var3_selected
      )
    }, error = function(e) {
      debugLog(paste("histogramDataValidation - Combination error:", e$message), "VALIDATION_ERROR")
      list(isValid = FALSE)
    })
    
    return(combinationValidation$isValid)
    
  }, error = function(e) {
    debugLog(paste("histogramDataValidation - Global error:", e$message), "VALIDATION_ERROR")
    return(FALSE)
  })
})
outputOptions(output, 'histogramDataValidation', suspendWhenHidden = FALSE)

# Density validation with duplicate detection
output$densityDataValidation <- reactive({
  # Force re-evaluation after swap operations
  swap_trigger <- values[["swap_completed_trigger"]]
  
  # Direct reactive dependencies
  current_dep_var <- input$dependentVar
  current_ind_var1 <- input$independentVar1
  current_ind_var2 <- input$independentVar2
  current_ind_var3 <- input$independentVar3
  var1_selected <- values$var1_selected
  var2_selected <- values$var2_selected
  var3_selected <- values$var3_selected
  
  tryCatch({
    debugLog("densityDataValidation - Starting natural reactive validation", "VALIDATION_DEBUG")
    
    if(is.null(current_dep_var) || current_dep_var == "" ||
       is.null(current_ind_var1) || current_ind_var1 == "") {
      return(FALSE)
    }
    
    cleanVars <- tryCatch({
      getCleanVariables(list(
        dependentVar = current_dep_var,
        independentVar1 = current_ind_var1,
        independentVar2 = current_ind_var2,
        independentVar3 = current_ind_var3
      ), labels)
    }, error = function(e) {
      debugLog(paste("densityDataValidation - Error cleaning vars:", e$message), "VALIDATION_ERROR")
      return(NULL)
    })
    
    if(is.null(cleanVars)) {
      return(FALSE)
    }
    
    duplicateInfo <- tryCatch({
      checkDuplicateVariables(
        current_ind_var1,
        current_ind_var2,
        current_ind_var3,
        labels
      )
    }, error = function(e) {
      debugLog(paste("densityDataValidation - Duplicate check error:", e$message), "VALIDATION_ERROR")
      list(shouldDisableHistDensity = TRUE)
    })
    
    if(duplicateInfo$shouldDisableHistDensity) {
      return(FALSE)
    }
    
    var1Validation <- tryCatch({
      validateGroupingVariable(values$filteredData, cleanVars$indVar1)
    }, error = function(e) {
      debugLog(paste("densityDataValidation - Var1 error:", e$message), "VALIDATION_ERROR")
      list(isValid = FALSE)
    })
    
    if(!var1Validation$isValid) {
      return(FALSE)
    }
    
    combinationValidation <- tryCatch({
      validateSelectedCombination(
        values$filteredData,
        current_dep_var,
        cleanVars$indVar1,
        cleanVars$indVar2,
        cleanVars$indVar3,
        var1_selected,
        var2_selected,
        var3_selected
      )
    }, error = function(e) {
      debugLog(paste("densityDataValidation - Combination error:", e$message), "VALIDATION_ERROR")
      list(isValid = FALSE)
    })
    
    return(combinationValidation$isValid)
    
  }, error = function(e) {
    debugLog(paste("densityDataValidation - Global error:", e$message), "VALIDATION_ERROR")
    return(FALSE)
  })
})
outputOptions(output, 'densityDataValidation', suspendWhenHidden = FALSE)

debugLog("VALIDATION_CORE_OUTPUTS: Core validation outputs loaded with natural reactive system", "VALIDATION_INIT")
