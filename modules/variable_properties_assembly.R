# Variable properties system assembly module
# Coordinate loading and integration of variable properties modules

# Load UI generation module
tryCatch({
  source("modules/variable_properties_ui.R", local = TRUE, encoding = "UTF-8")
  debugLog("VARIABLE_PROPERTIES_ASSEMBLY: variable_properties_ui.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("VARIABLE_PROPERTIES_ASSEMBLY: Error loading variable_properties_ui.R:", e$message), "MODULE_ERROR")
})

# Load reactive observers module
tryCatch({
  source("modules/variable_properties_observers.R", local = TRUE, encoding = "UTF-8")
  debugLog("VARIABLE_PROPERTIES_ASSEMBLY: variable_properties_observers.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("VARIABLE_PROPERTIES_ASSEMBLY: Error loading variable_properties_observers.R:", e$message), "MODULE_ERROR")
})

# Verify variable properties system integrity
# Note: Direct verification of Shiny output objects is not performed because output
# objects are of class 'shinyoutput' rather than standard R environments, making
# exists() checks incompatible. Shiny will generate explicit errors if required
# outputs are missing, making additional verification redundant.
verifyVariablePropertiesSystem <- function() {
  required_outputs <- c("var1Properties", "var2Properties", "var3Properties")
  
  debugLog(paste("VARIABLE_PROPERTIES_ASSEMBLY: Variable properties system expects outputs:", 
                 paste(required_outputs, collapse = ", ")), "MODULE_LOAD")
  
  # Verify helper functions are available
  required_functions <- c("getSelectedModalities", "getModalityColors", "createVariablePropertiesUI")
  
  for(func_name in required_functions) {
    if(!exists(func_name, envir = .GlobalEnv)) {
      debugLog(paste("VARIABLE_PROPERTIES_ASSEMBLY: Missing required function:", func_name), "MODULE_ERROR")
      return(FALSE)
    }
  }
  
  debugLog("VARIABLE_PROPERTIES_ASSEMBLY: All variable properties components verified", "MODULE_LOAD")
  return(TRUE)
}

# Initialize variable properties system
initializeVariablePropertiesSystem <- function() {
  if(verifyVariablePropertiesSystem()) {
    debugLog("VARIABLE_PROPERTIES_ASSEMBLY: Variable properties system initialized successfully", "VAR_INIT")
    return(TRUE)
  } else {
    debugLog("VARIABLE_PROPERTIES_ASSEMBLY: Variable properties system initialization failed", "VAR_PROPERTIES") 
    return(FALSE)
  }
}

# Initialize system
variable_properties_system_ready <- initializeVariablePropertiesSystem()

debugLog("VARIABLE_PROPERTIES_ASSEMBLY: Modular variable properties system assembly completed", "VAR_INIT")
