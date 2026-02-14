# Debug system assembly module
# Coordinate loading and integration of debug modules

# Load debug modules in correct dependency order
tryCatch({
  source("modules/debug_core.R", local = TRUE, encoding = "UTF-8")
  debugLog("DEBUG_ASSEMBLY: debug_core.R loaded", "MODULE_LOAD")
}, error = function(e) {
  cat("CRITICAL: Error loading debug_core.R:", e$message, "\n")
  stop("Critical debug core module failed to load")
})

tryCatch({
  source("modules/debug_performance.R", local = TRUE, encoding = "UTF-8")
  debugLog("DEBUG_ASSEMBLY: debug_performance.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("DEBUG_ASSEMBLY: Error loading debug_performance.R:", e$message), "MODULE_ERROR")
})

tryCatch({
  source("modules/debug_logging.R", local = TRUE, encoding = "UTF-8")
  debugLog("DEBUG_ASSEMBLY: debug_logging.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("DEBUG_ASSEMBLY: Error loading debug_logging.R:", e$message), "MODULE_ERROR")
})

# Verify debug system integrity
verifyDebugSystem <- function() {
  required_functions <- c("debugLog", "initializeDebug", "isCategoryEnabled", 
                         "traceReactive", "measureTime", "traceObserverTiming", 
                         "exportDebugLog")
  
  for(func_name in required_functions) {
    if(!exists(func_name, envir = .GlobalEnv)) {
      cat("DEBUG_ASSEMBLY: Missing required function:", func_name, "\n")
      return(FALSE)
    }
  }
  
  debugLog("DEBUG_ASSEMBLY: All debug functions verified", "MODULE_LOAD")
  return(TRUE)
}

# Initialize debug system
initializeDebugSystem <- function() {
  if(verifyDebugSystem()) {
    debugLog("DEBUG_ASSEMBLY: Debug system initialized successfully", "APP_STARTUP")
    return(TRUE)
  } else {
    cat("DEBUG_ASSEMBLY: Debug system initialization failed\n")
    return(FALSE)
  }
}

# Cleanup debug system resources
cleanupDebugSystem <- function() {
  if(debugMode) {
    debugLog("DEBUG_ASSEMBLY: Cleaning up debug system", "APP_STARTUP")
    
    # Export final log
    finalLogFile <- exportDebugLog(paste0("final_debug_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
    if(!is.null(finalLogFile)) {
      debugLog(paste("Final debug log saved:", finalLogFile), "APP_STARTUP")
    }
  }
}

# Register cleanup on session end
if(exists("session") && !is.null(session)) {
  session$onSessionEnded(function() {
    cleanupDebugSystem()
  })
}

# Initialize system
debug_system_ready <- initializeDebugSystem()

debugLog("DEBUG_ASSEMBLY: Modular debug system assembly completed", "APP_STARTUP")