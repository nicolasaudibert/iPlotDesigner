# Enhanced validation main module
# Main integration point and public interface for enhanced user validation system
# Replaces enhanced_user_validation.R with modular architecture

# ============================================================================
# Module loading and dependencies
# ============================================================================

# Load dialog support functions first (required by dialog creation functions)
tryCatch({
  if (file.exists("modules/dialog_support.R")) {
    source("modules/dialog_support.R", local = TRUE, encoding = "UTF-8")
    debugLog("ENHANCED_VALIDATION_MAIN: dialog_support.R loaded successfully", "MODULE_LOAD")
  } else {
    stop("dialog_support.R not found")
  }
}, error = function(e) {
  debugLog(paste("ENHANCED_VALIDATION_MAIN: Error loading dialog_support.R:", e$message), "MODULE_ERROR")
  debugLog(paste("ENHANCED_VALIDATION_MAIN: Current working directory:", getwd()), "MODULE_ERROR")
  debugLog(paste("ENHANCED_VALIDATION_MAIN: Files in modules/:", paste(list.files("modules", pattern = "*.R"), collapse = ", ")), "MODULE_ERROR")
  stop("Required module dialog_support.R could not be loaded")
})

# Load dialog creation functions (depends on dialog_support.R)
tryCatch({
  if (file.exists("modules/user_dialogs.R")) {
    source("modules/user_dialogs.R", local = TRUE, encoding = "UTF-8")
    debugLog("ENHANCED_VALIDATION_MAIN: user_dialogs.R loaded successfully", "MODULE_LOAD")
  } else {
    stop("user_dialogs.R not found")
  }
}, error = function(e) {
  debugLog(paste("ENHANCED_VALIDATION_MAIN: Error loading user_dialogs.R:", e$message), "MODULE_ERROR")
  debugLog(paste("ENHANCED_VALIDATION_MAIN: Current working directory:", getwd()), "MODULE_ERROR") 
  debugLog(paste("ENHANCED_VALIDATION_MAIN: Files in modules/:", paste(list.files("modules", pattern = "*.R"), collapse = ", ")), "MODULE_ERROR")
  stop("Required module user_dialogs.R could not be loaded")
})

# Validate all required dependencies are available
dependency_check <- validateDialogDependencies()
if (!dependency_check$valid) {
  debugLog(paste("ENHANCED_VALIDATION_MAIN: Missing dependencies:", paste(dependency_check$missing, collapse = ", ")), "MODULE_ERROR")
  warning("Some enhanced validation dependencies are missing - functionality may be limited")
}

# ============================================================================
# Error handling and fallbacks
# ============================================================================

# Validate system integrity before processing
validateSystemIntegrity <- function() {
  
  integrity_check <- list(
    valid = TRUE,
    issues = c()
  )
  
  # Check Phase 1A functions are preserved
  phase1a_functions <- c(
    "storeUserOperation",
    "storeFilteringOperation",
    "getUserOperationsSummary"
  )
  
  for (func_name in phase1a_functions) {
    if (!exists(func_name, envir = .GlobalEnv)) {
      integrity_check$valid <- FALSE
      integrity_check$issues <- c(integrity_check$issues, paste("Missing Phase 1A function:", func_name))
    }
  }
  
  # Check configuration integrity
  if (!exists("USER_CHOICE_SETTINGS", envir = .GlobalEnv)) {
    integrity_check$valid <- FALSE
    integrity_check$issues <- c(integrity_check$issues, "Missing USER_CHOICE_SETTINGS configuration")
  }
  
  if (!exists("USER_OPERATIONS_CONFIG", envir = .GlobalEnv)) {
    integrity_check$valid <- FALSE
    integrity_check$issues <- c(integrity_check$issues, "Missing USER_OPERATIONS_CONFIG configuration")
  }
  
  if (!integrity_check$valid) {
    debugLog(paste("SYSTEM_INTEGRITY: Issues found -", paste(integrity_check$issues, collapse = "; ")), "APP_CRITICAL")
  }
  
  return(integrity_check)
}

# ============================================================================
# Backward compatibility interface
# ============================================================================

# Ensure existing code continues to work unchanged
# Main entry point that replaces original enhanced_user_validation.R exports

# Re-export all functions that were previously available
# This ensures no breaking changes for existing code

# Main dialog function (primary interface)
if (!exists("showIntelligentValidationDialog", envir = .GlobalEnv)) {
  debugLog("ENHANCED_VALIDATION_MAIN: Main dialog function not found - system may not be fully loaded", "VALIDATION_WARN")
}

# Support functions that may be called directly
if (!exists("safeGetUILabel", envir = .GlobalEnv)) {
  debugLog("ENHANCED_VALIDATION_MAIN: Support functions not found - system may not be fully loaded", "VALIDATION_WARN")
}

# ============================================================================
# Module completion and logging
# ============================================================================

# Final validation of successful module loading
final_check <- validateSystemIntegrity()
if (final_check$valid) {
  debugLog("ENHANCED_VALIDATION_MAIN: Module system loaded successfully with full Phase 1A preservation", "VALIDATION_PROCESS")
} else {
  debugLog(paste("ENHANCED_VALIDATION_MAIN: Module loaded with issues:", paste(final_check$issues, collapse = "; ")), "VALIDATION_WARN")
}

debugLog("ENHANCED_VALIDATION_MAIN: Enhanced validation main integration module loaded", "APP_STARTUP")
