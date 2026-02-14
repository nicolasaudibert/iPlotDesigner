# ============================================================================
# Hierarchical debug category system
# ============================================================================
# Structure: SYSTEM_COMPONENT_LEVEL
# - SYSTEM: Main system (APP, UI, DATA, etc.)
# - COMPONENT: Specific component (VALIDATION, MODULE, etc.)
# - LEVEL: Severity/type level (ERROR, WARN, INFO, DEBUG, TRACE)

# Global configuration
debugMode <- TRUE  # Set to FALSE to disable debug
profilingEnabled <- FALSE  # Set to FALSE to disable profiling UI/functionality
labelCacheMonitoring <- TRUE  # Detailed cache tracking détaillé du cache (requires profilingEnabled = TRUE)
debugLogFile <- "iPlotDesigner_debug.log"
debugStartTime <- NULL
defaultDebugProfile <- "production" # Set to either "production", "development", "verbose", "silent" or "custom", see function setDebugProfile() for details

# Fine-grained debug settings 
debugCategories <- list(
  # ============================================================================
  # Critical level - always enabled
  # ============================================================================
  
  # Critical system errors
  "APP_CRITICAL" = TRUE,           # Critical application errors
  "DATA_CRITICAL" = TRUE,          # Critical data corruption
  "VALIDATION_CRITICAL" = TRUE,    # Critical validation errors
  
  # Important system errors
  "APP_ERROR" = TRUE,              # General application errors
  "DATA_ERROR" = TRUE,             # Data errors
  "MODULE_ERROR" = TRUE,           # Module loading errors
  "UI_ERROR" = TRUE,               # User interface errors
  "VALIDATION_ERROR" = TRUE,       # Validation errors
  
  # System warnings
  "APP_WARN" = TRUE,               # Application warnings
  "PERFORMANCE_WARN" = TRUE,       # Performance warnings
  "VALIDATION_WARN" = TRUE,        # Validation warnings
  "CASCADE_WARN" = TRUE,           # Cascade warnings
  "LOOP_WARN" = TRUE,              # Loop warnings
  
  # ============================================================================
  # System level - controllable by domain
  # ============================================================================
  
  # Application & startup
  "APP_STARTUP" = FALSE,            # Application startup
  "APP_INIT" = FALSE,               # Application initialization
  "APP_CONFIG" = FALSE,             # Application configuration
  "APP_SHUTDOWN" = FALSE,           # Application shutdown
  
  # Modules
  "MODULE_LOAD" = TRUE,            # Module loading
  "MODULE_INIT" = TRUE,            # Module initialization
  "MODULE_EXPORT" = TRUE,          # Module export
  "MODULE_WARN" = TRUE,            # Module warnings
  
  # User interface
  "UI_INIT" = FALSE,                # UI initialization
  "UI_UPDATE" = FALSE,              # UI updates
  "UI_RESET" = FALSE,               # UI resets
  "UI_RENDER" = FALSE,              # UI rendering
  "UI_INTERACTION" = FALSE,         # User interactions
  
  # Data
  "DATA_LOAD" = FALSE,              # Data loading
  "DATA_PROCESS" = FALSE,           # Data processing
  "DATA_PRESERVE" = FALSE,          # Data preservation
  "DATA_FILTER" = FALSE,            # Data filtering
  "DATA_REFRESH" = FALSE,           # Data refresh
  
  # Validation
  "VALIDATION_INIT" = TRUE,        # Validation initialization
  "VALIDATION_PROCESS" = TRUE,     # Validation process
  "VALIDATION_DIALOG" = TRUE,      # Validation dialogs
  "VALIDATION_RESULT" = TRUE,      # Validation results
  
  # Variables (unified system)
  "VAR_INIT" = FALSE,               # Variable initialization
  "VAR_CHANGE" = FALSE,             # Variable changes
  "VAR_PROPERTIES" = FALSE,         # Variable properties
  "VAR_VALIDATION" = FALSE,         # Variable validation
  "VAR_SWAP" = FALSE,               # Variable swapping
  
  # Plot & rendering
  "PLOT_INIT" = FALSE,              # Plot initialization
  "PLOT_RENDER" = FALSE,            # Plot rendering
  "PLOT_PROTECT" = FALSE,           # Plot protection
  "PLOT_EXPORT" = FALSE,            # Plot export
  
  # Code generation
  "CODE_INIT" = FALSE,              # Code generation initialization
  "CODE_PROCESS" = FALSE,           # Code generation processing
  "CODE_CONFIG" = FALSE,            # Code generation configuration
  "CODE_EXPORT" = FALSE,            # Code export
  "CODE_ERROR" = TRUE,              # Code generation errors
  
  # Code generation - fonts
  "CODE_FONT_DETECT" = FALSE,       # Font detection in code generation
  "CODE_FONT_SETUP" = FALSE,        # Font setup code generation
  "CODE_FONT_ERROR" = TRUE,         # Font-related code generation errors
  
  # Font system (optional extrafont integration)
  "FONT_INIT" = FALSE,              # Font system initialization
  "FONT_IMPORT" = FALSE,            # Font import process
  "FONT_DETECT" = FALSE,            # Font detection and status check
  "FONT_FILTER" = FALSE,            # Font filtering (problematic fonts)
  "FONT_ERROR" = TRUE,              # Font system errors
  
  # Profiling & performance
  "PROFILING_INIT" = FALSE,         # Profiling initialization
  "PROFILING_START" = FALSE,        # Profiling start
  "PROFILING_STOP" = FALSE,         # Profiling stop
  "PROFILING_RESULT" = FALSE,       # Profiling results
  "PERFORMANCE_TIMING" = FALSE,     # Timing measurements
  "PERFORMANCE_OPTIMIZATION" = FALSE, # Optimizations
  "PROFILING_WARN" = TRUE,          # Profiling warnings
  
  # ============================================================================
  # Debug level - activable for development
  # ============================================================================
  
  # General debug
  "APP_DEBUG" = TRUE,             # General application debug
  "DATA_DEBUG" = FALSE,            # Data debug
  "UI_DEBUG" = FALSE,              # Interface debug
  "VALIDATION_DEBUG" = FALSE,      # Validation debug
  "VAR_DEBUG" = TRUE,             # Variable debug
  "PLOT_DEBUG" = FALSE,            # Plot debug
  "CODE_DEBUG" = FALSE,            # Code generation debug
  
  # ============================================================================
  # Trace level - activatable for intensive debugging
  # ============================================================================
  
  # Reactive observations
  "REACTIVE_TRACE" = FALSE,        # General reactive trace
  "OBSERVER_TRACE" = FALSE,        # Observer trace
  "OBSERVER_TIMING" = FALSE,       # Observer timing
  
  # User interactions
  "USER_TRACE" = FALSE,            # User interaction trace
  "BUTTON_TRACE" = FALSE,          # Button trace
  "DIALOG_TRACE" = FALSE,          # Dialog trace
  
  # Cache and labels
  "CACHE_TRACE" = FALSE,           # Cache trace
  "LABEL_TRACE" = FALSE,           # Label trace
  
  # ============================================================================
  # Specialized categories - activatable as needed
  # ============================================================================
  
  # Dialogs and modals
  "DIALOG_INIT" = FALSE,            # Dialog initialization
  "DIALOG_SHOW" = FALSE,            # Dialog display
  "DIALOG_FALLBACK" = FALSE,        # Dialog fallback
  "MODAL_INIT" = FALSE,             # Modal initialization
  "MODAL_CACHE" = FALSE,           # Modal cache
  
  # Cache system
  "CACHE_INIT" = FALSE,             # Cache initialization
  "CACHE_STORE" = FALSE,           # Cache storage
  "CACHE_ERROR" = FALSE,            # Cache errors
  
  # User operations
  "USER_OPERATION" = FALSE,         # User operations
  "USER_CHOICE" = FALSE,            # User choices
  "USER_CHOICE_RESULT" = FALSE,     # User choice results
  
  # System coordination
  "COORDINATION_INIT" = FALSE,      # Coordination initialization
  "PROTECTION_ACTIVE" = FALSE,      # Active protection
  "ISOLATION_ACTIVE" = FALSE        # Active isolation
)

# Check that essential categories are defined
essential_categories <- c("APP_CRITICAL", "DATA_CRITICAL", "VALIDATION_CRITICAL", 
                          "APP_ERROR", "DATA_ERROR", "MODULE_ERROR", "UI_ERROR", 
                          "VALIDATION_ERROR", "PERFORMANCE_WARN", "LOOP_WARN")

for(cat in essential_categories) {
  if(!cat %in% names(debugCategories)) {
    warning(paste("Essential debug category missing:", cat))
  }
}

# ============================================================================
# Helper functions for hierarchical filtering
# ============================================================================

# Check if a category or group is enabled
isCategoryEnabled <- function(category) {
  if(is.null(debugCategories[[category]])) {
    # For new undefined categories, enable important levels by default
    if(grepl("_(CRITICAL|ERROR|WARN)$", category)) {
      return(TRUE)
    }
    if(grepl("_(DEBUG|TRACE)$", category)) {
      return(FALSE)
    }
    return(TRUE)  # By default, enable other categories
  }
  return(debugCategories[[category]])
}

# Enable/disable by level (ERROR, WARN, DEBUG, TRACE)
setCategoryLevel <- function(level, enabled) {
  matching_categories <- names(debugCategories)[grepl(paste0("_", level, "$"), names(debugCategories))]
  for(cat in matching_categories) {
    debugCategories[[cat]] <<- enabled
  }
  cat("Updated", length(matching_categories), "categories for level:", level, "\n")
}

# Predefined debug profiles
setDebugProfile <- function(profile) {
  switch(profile,
         "production" = {
           # Only critical and important errors
           for(cat in names(debugCategories)) {
             debugCategories[[cat]] <<- FALSE
           }
           setCategoryLevel("CRITICAL", TRUE)
           setCategoryLevel("ERROR", TRUE)
           setCategoryLevel("WARN", TRUE)
         },
         "development" = {
           # Everything except trace
           for(cat in names(debugCategories)) {
             debugCategories[[cat]] <<- TRUE
           }
           setCategoryLevel("TRACE", FALSE)
         },
         "verbose" = {
           # Everything enabled
           for(cat in names(debugCategories)) {
             debugCategories[[cat]] <<- TRUE
           }
         },
         "silent" = {
           # Only critical
           for(cat in names(debugCategories)) {
             debugCategories[[cat]] <<- FALSE
           }
           setCategoryLevel("CRITICAL", TRUE)
         },
         "custom" = {
           # Use the predefined values in debugCategories - no changes applied
           cat("Using custom category values from debugCategories configuration\n")
         },
         {
           # Fallback: unknown profile defaults to "custom" behavior
           cat("Unknown profile '", profile, "' - using custom category values\n", sep = "")
         }
  )
  cat("Debug profile set to:", profile, "\n")
}

# ============================================================================
# Main debug system functions
# ============================================================================

# Check if profiling is enabled
isProfilingEnabled <- function() {
  return(profilingEnabled)
}

# Initialize debug session
initializeDebug <- function() {
  if(debugMode) {
    debugStartTime <<- Sys.time()
    cat("=== DEBUG SESSION STARTED ===\n", file = debugLogFile, append = FALSE)
    cat("Timestamp:", format(debugStartTime), "\n", file = debugLogFile, append = TRUE)
    cat("Debug mode:", debugMode, "\n", file = debugLogFile, append = TRUE)
    cat("Profiling enabled:", profilingEnabled, "\n", file = debugLogFile, append = TRUE)
    cat("Active categories:", paste(names(debugCategories)[sapply(debugCategories, isTRUE)], collapse = ", "), "\n", file = debugLogFile, append = TRUE)
    cat("============================\n\n", file = debugLogFile, append = TRUE)
  }
}

# Main debug log function with category filtering
debugLog <- function(message, category = "APP_DEBUG", writeToFile = TRUE) {
  if(debugMode && isCategoryEnabled(category)) {
    timestamp <- format(Sys.time(), "%H:%M:%S.%3f")
    elapsed <- if(!is.null(debugStartTime)) {
      round(as.numeric(difftime(Sys.time(), debugStartTime, units = "secs")), 3)
    } else 0
    
    logMessage <- paste0("[", timestamp, "] [+", elapsed, "s] [", category, "] ", message)
    
    # Always print to console for critical events
    if(grepl("_CRITICAL|_ERROR|_WARN", category)) {
      cat("*** ", logMessage, " ***\n")
    } else {
      cat(logMessage, "\n")
    }
    
    # Optionally write to file
    if(writeToFile) {
      cat(logMessage, "\n", file = debugLogFile, append = TRUE)
    }
  }
}

# ============================================================================
# Default configuration
# ============================================================================

if(debugMode) {
  # Default profile for development
  if(!exists("DEBUG_PROFILE")) {
    DEBUG_PROFILE <- defaultDebugProfile
  }
  setDebugProfile(DEBUG_PROFILE)
  
  debugLog("DEBUG_CORE: Harmonized debug system initialized", "APP_STARTUP")
}
