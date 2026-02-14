# Debug logging module
# Logging functions and output management

# Trace observer events
traceObserver <- function(name, varName = "", action = "TRIGGERED") {
  if(isCategoryEnabled("OBSERVER_TRACE")) {
    debugLog(paste("OBSERVER_TRACE", action, ":", name, if(varName != "") paste("(", varName, ")") else ""), "OBSERVER_TRACE")
  }
}

# Trace UI updates
traceUI <- function(component, action = "", details = "") {
  if(isCategoryEnabled("UI_UPDATE")) {
    debugLog(paste("UI UPDATE:", component, action, details), "UI_UPDATE")
  }
}

# Trace specific function calls
traceFunctionCall <- function(functionName, args = "", result = "") {
  if(debugMode && isCategoryEnabled("APP_DEBUG")) {
    debugLog(paste("FUNCTION_CALL:", functionName, 
                  if(args != "") paste("args:", args) else "", if(result != "") paste("result:", result) else ""), "APP_DEBUG")
  }
}

# Export debug log with report generation
exportDebugLog <- function(filename = NULL) {
  if(is.null(filename)) {
    filename <- paste0("debug_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
  }
  
  if(file.exists(debugLogFile)) {
    file.copy(debugLogFile, filename)
    debugLog(paste("Debug log exported to:", filename), "APP_DEBUG")
    return(filename)
  } else {
    debugLog("No debug log file found to export", "APP_ERROR")
    return(NULL)
  }
}

debugLog("DEBUG_LOGGING: Logging functions and output management loaded", "APP_STARTUP")