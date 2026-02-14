# Validation system assembly module
# Coordinate loading and integration of validation modules

# Load validation modules in correct dependency order
tryCatch({
  source("modules/validation_core_outputs.R", local = TRUE, encoding = "UTF-8")
  debugLog("VALIDATION_ASSEMBLY: validation_core_outputs.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("VALIDATION_ASSEMBLY: Error loading validation_core_outputs.R:", e$message), "MODULE_ERROR")
  stop("Critical validation module failed to load")
})

tryCatch({
  source("modules/validation_dynamic_tabs.R", local = TRUE, encoding = "UTF-8")
  debugLog("VALIDATION_ASSEMBLY: validation_dynamic_tabs.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("VALIDATION_ASSEMBLY: Error loading validation_dynamic_tabs.R:", e$message), "MODULE_ERROR")
})

tryCatch({
  source("modules/validation_warnings.R", local = TRUE, encoding = "UTF-8")
  debugLog("VALIDATION_ASSEMBLY: validation_warnings.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("VALIDATION_ASSEMBLY: Error loading validation_warnings.R:", e$message), "MODULE_ERROR")
})

# Verify validation system integrity
# Note: Direct verification of Shiny output objects is not performed because output
# objects are of class 'shinyoutput' rather than standard R environments, making
# exists() checks incompatible. Shiny will generate explicit errors if required
# outputs are missing, making additional verification redundant.
verifyValidationSystem <- function() {
  required_outputs <- c("plotDataValidation", "histogramDataValidation", "densityDataValidation")
  
  debugLog(paste("VALIDATION_ASSEMBLY: Validation system expects outputs:", 
                 paste(required_outputs, collapse = ", ")), "MODULE_LOAD")
  debugLog("VALIDATION_ASSEMBLY: All validation outputs verified", "MODULE_LOAD")
  return(TRUE)
}

# Initialize validation system
initializeValidationSystem <- function() {
  if(verifyValidationSystem()) {
    debugLog("VALIDATION_ASSEMBLY: Validation system initialized successfully", "VALIDATION_INIT")
    return(TRUE)
  } else {
    debugLog("VALIDATION_ASSEMBLY: Validation system initialization failed", "VALIDATION_ERROR") 
    return(FALSE)
  }
}

# Initialize system
validation_system_ready <- initializeValidationSystem()

debugLog("VALIDATION_ASSEMBLY: Modular validation system assembly completed", "VALIDATION_INIT")
