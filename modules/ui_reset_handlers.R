# UI Reset Handlers - SYNCHRONOUS VERSION

# SYNCHRONOUS reset function for invalid variables
resetInvalidVariableUI <- function(varNum, session) {
  debugLog(paste("UI_RESET: Starting synchronous reset for var", varNum), "UI_RESET")
  
  # Immediate synchronous reset
  tryCatch({
    inputId <- paste0("independentVar", varNum)
    updateSelectInput(session, inputId, selected = "")
    debugLog(paste("UI_RESET: Reset", inputId, "synchronously"), "UI_RESET")
  }, error = function(e) {
    debugLog(paste("UI_RESET ERROR:", e$message), "UI_RESET")
  })
}

# SYNCHRONOUS cleanup for variable properties
cleanupVariableProperties <- function(varNum, values) {
  debugLog(paste("UI_RESET: Cleaning up properties for var", varNum), "UI_RESET")
  
  # Immediate synchronous cleanup
  values[[paste0("var", varNum, "_modalities")]] <- NULL
  values[[paste0("var", varNum, "_selected")]] <- NULL
  values[[paste0("var", varNum, "_colors")]] <- NULL
  values[[paste0("var", varNum, "_order")]] <- NULL
  
  debugLog(paste("UI_RESET: Properties cleaned for var", varNum), "UI_RESET")
}
