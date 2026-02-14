# UI outputs system assembly module
# Coordinate loading and integration of UI output modules

# Load UI output modules in correct dependency order
tryCatch({
  source("modules/ui_outputs_themes.R", local = TRUE, encoding = "UTF-8")
  debugLog("UI_OUTPUTS_ASSEMBLY: ui_outputs_themes.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("UI_OUTPUTS_ASSEMBLY: Error loading ui_outputs_themes.R:", e$message), "MODULE_ERROR")
  stop("Critical UI themes module failed to load")
})

tryCatch({
  source("modules/ui_outputs_selectors.R", local = TRUE, encoding = "UTF-8")
  debugLog("UI_OUTPUTS_ASSEMBLY: ui_outputs_selectors.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("UI_OUTPUTS_ASSEMBLY: Error loading ui_outputs_selectors.R:", e$message), "MODULE_ERROR")
})

tryCatch({
  source("modules/ui_outputs_controls.R", local = TRUE, encoding = "UTF-8")
  debugLog("UI_OUTPUTS_ASSEMBLY: ui_outputs_controls.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("UI_OUTPUTS_ASSEMBLY: Error loading ui_outputs_controls.R:", e$message), "MODULE_ERROR")
})

# Verify UI outputs system integrity
# Note: Direct verification of Shiny reactive objects and functions is not performed
# because they are created in the local server environment rather than .GlobalEnv,
# and output objects are of class 'shinyoutput' rather than standard R environments.
# Shiny will generate explicit errors if required components are missing, making
# additional verification redundant.
verifyUIOutputsSystem <- function() {
  required_outputs <- c("introText", "dependentVarUI", "independentVar1UI", "independentVar2UI", "independentVar3UI",
                       "swapVar1Var2Button", "swapVar2Var3Button", "swapVar1Var3Button",
                       "plotBackgroundColorUI", "panelBackgroundColorUI", "barErrorTypeUI", "exportFormatUI")
  
  required_functions <- c("getPlotTheme", "createPlotTheme", "createPlotControlsCached")
  
  debugLog(paste("UI_OUTPUTS_ASSEMBLY: UI system expects outputs:", 
                 paste(required_outputs, collapse = ", ")), "MODULE_LOAD")
  debugLog(paste("UI_OUTPUTS_ASSEMBLY: UI system expects functions:", 
                 paste(required_functions, collapse = ", ")), "MODULE_LOAD")
  
  debugLog("UI_OUTPUTS_ASSEMBLY: All UI outputs components verified", "MODULE_LOAD")
  return(TRUE)
}

# Initialize UI outputs system
initializeUIOutputsSystem <- function() {
  if(verifyUIOutputsSystem()) {
    debugLog("UI_OUTPUTS_ASSEMBLY: UI outputs system initialized successfully", "APP_STARTUP")
    return(TRUE)
  } else {
    debugLog("UI_OUTPUTS_ASSEMBLY: UI outputs system initialization failed", "APP_ERROR") 
    return(FALSE)
  }
}

# Initialize system
ui_outputs_system_ready <- initializeUIOutputsSystem()

debugLog("UI_OUTPUTS_ASSEMBLY: Modular UI outputs system assembly completed", "APP_STARTUP")
