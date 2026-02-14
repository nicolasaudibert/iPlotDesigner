# Reactive protection system module
# Management of reactive flags and protection mechanisms

# Reactive control variables
lastValidationWarning <- reactiveVal("")
resetInProgress <- reactiveVal(FALSE)
reorganizationInProgress <- reactiveVal(FALSE)
validationInProgress <- reactiveVal(FALSE)
globalCooldown <- reactiveVal(FALSE)

# Synchronous flag reset without delays
resetAllFlags <- function(delay = 0.1) {
  debugLog("RESET_FLAGS_CALLED: Starting synchronous flag reset (delay ignored)", "PROTECTION_ACTIVE")
  
  # Immediate synchronous reset - ignore delay parameter for backward compatibility
  resetInProgress(FALSE)
  reorganizationInProgress(FALSE)
  validationInProgress(FALSE)
  globalCooldown(FALSE)
  debugLog("All protection flags reset synchronously", "PROTECTION_ACTIVE")
  resetReactiveCounters()
}

# Check if any protection is active
isProtected <- function() {
  protected <- resetInProgress() || reorganizationInProgress() || validationInProgress() || globalCooldown()
  if(protected) {
    debugLog(paste("PROTECTION ACTIVE: reset=", resetInProgress(), 
                  "reorg=", reorganizationInProgress(), "valid=", validationInProgress(), "cooldown=", globalCooldown()), "PROTECTION_ACTIVE")
                  
  }
  return(protected)
}
