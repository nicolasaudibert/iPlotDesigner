# Debug profiling server functions - server functionality only
# Server reactive outputs and profiling logic, loaded very late
# This file is loaded after all core modules are initialized

# Ensure digest package is available
if(!requireNamespace("digest", quietly = TRUE)) {
  install.packages("digest")
}
require(digest)

# Global profiling state using simple environment (not reactiveValues)
.profiling_env <- new.env()
.profiling_env$isActive <- FALSE
.profiling_env$startTime <- NULL
.profiling_env$sessionId <- NULL
.profiling_env$profileFile <- NULL

# Generate unique profile filename
generateProfileFilename <- function() {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  sessionId <- substr(digest::digest(Sys.time()), 1, 8)
  return(paste0("iPlotDesigner_profile_", timestamp, "_", sessionId, ".Rprof"))
}

# Start profiling function
startProfiling <- function() {
  if(!.profiling_env$isActive) {
    .profiling_env$sessionId <- substr(digest::digest(Sys.time()), 1, 8)
    .profiling_env$profileFile <- generateProfileFilename()
    .profiling_env$startTime <- Sys.time()
    
    # Enable label cache tracking if configured
    if(exists("labelCacheMonitoring", envir = .GlobalEnv) && 
       get("labelCacheMonitoring", envir = .GlobalEnv) == TRUE &&
       exists("enableLabelCacheTracking", envir = .GlobalEnv)) {
      tryCatch({
        enableLabelCacheTracking(TRUE)
        debugLog("PROFILING: Label cache tracking enabled", "PROFILING_START")
      }, error = function(e) {
        debugLog(paste("PROFILING: Could not enable label cache tracking:", e$message), "PROFILING_WARN")
      })
    }
    
    # Start Rprof with detailed settings
    Rprof(filename = .profiling_env$profileFile, 
          interval = 0.01,  # 10ms intervals for detailed capture
          memory.profiling = TRUE,
          gc.profiling = TRUE,
          line.profiling = TRUE)
    
    .profiling_env$isActive <- TRUE
    
    debugLog(paste("PROFILING: Started session", .profiling_env$sessionId, "->", .profiling_env$profileFile), "PROFILING_RESULT")
    
    return(TRUE)
  }
  return(FALSE)
}

# Stop profiling function
stopProfiling <- function() {
  if(.profiling_env$isActive) {
    Rprof(NULL)  # Stop profiling
    
    # Capture label cache metrics if available
    if(exists("getLabelCacheStats", envir = .GlobalEnv)) {
      tryCatch({
        stats <- getLabelCacheStats()
        if(stats$total_accesses > 0) {
          debugLog(paste("PROFILING_METRICS: Label Cache - Hit Rate:", 
                         stats$hit_rate_percent, "%,",
                         "Size:", stats$cache_size, "labels,",
                         "Total accesses:", stats$total_accesses), 
                   "PROFILING_RESULT")
          
          # Export detailed statistics if tracking was enabled
          if(stats$tracking_enabled && exists("exportLabelCacheStats", envir = .GlobalEnv)) {
            export_result <- exportLabelCacheStats()
            if(export_result$success) {
              debugLog(paste("PROFILING_METRICS: Exported label cache details to", 
                           export_result$stats_file), "PROFILING_RESULT")
            }
          }
        }
        
        # Disable tracking after profiling
        if(exists("enableLabelCacheTracking", envir = .GlobalEnv)) {
          enableLabelCacheTracking(FALSE)
        }
      }, error = function(e) {
        # Silent fail - cache stats not critical
      })
    }
    
    duration <- as.numeric(difftime(Sys.time(), .profiling_env$startTime, units = "secs"))
    
    debugLog(paste("PROFILING: Stopped session", .profiling_env$sessionId, 
                   "after", round(duration, 2), "seconds"), "PROFILING_RESULT")
    
    # Generate summary immediately
    tryCatch({
      profile_summary <- summaryRprof(.profiling_env$profileFile, memory = "both")
      
      # Log key findings
      if(!is.null(profile_summary$by.self)) {
        top_functions <- head(profile_summary$by.self[order(-profile_summary$by.self$self.time), ], 5)
        debugLog("PROFILING: Top 5 functions by self time:", "PROFILING_RESULT")
        for(i in 1:min(5, nrow(top_functions))) {
          debugLog(paste("  ", i, ":", rownames(top_functions)[i], 
                        "->", round(top_functions$self.time[i], 2), "s"), "PROFILING_RESULT")
        }
      }
      
      # Memory info if available
      if(!is.null(profile_summary$by.total)) {
        total_time <- sum(profile_summary$by.total$total.time, na.rm = TRUE)
        debugLog(paste("PROFILING: Total measured time:", round(total_time, 2), "seconds"), "PROFILING_RESULT")
      }
      
    }, error = function(e) {
      debugLog(paste("PROFILING: Error generating summary:", e$message), "PROFILING_RESULT")
    })
    
    .profiling_env$isActive <- FALSE
    return(.profiling_env$profileFile)
  }
  return(NULL)
}

# Get profiling status (safe access)
getProfilingStatus <- function() {
  return(list(
    isActive = .profiling_env$isActive,
    startTime = .profiling_env$startTime,
    sessionId = .profiling_env$sessionId,
    profileFile = .profiling_env$profileFile
  ))
}

# Function to initialize profiling outputs in server context
# Critical: This is called *very late* after all core modules are loaded
initializeProfilingOutputs <- function(input, output, session) {
  
  # Check if profiling is enabled
  if(!exists("isProfilingEnabled") || !isProfilingEnabled()) {
    debugLog("PROFILING_SERVER: Profiling disabled, skipping server initialization", "PROFILING_START")
    return(NULL)
  }
  
  debugLog("PROFILING_SERVER: Starting delayed profiling server initialization", "PROFILING_INIT")
  
  # Timer for periodic UI refresh (completely isolated from app reactivity)
  timer <- reactiveTimer(1000)  # Update every second when profiling is active
  
  # Profiling status UI with ZERO dependency on app reactive system
  output$profilingStatus <- renderUI({
    # Only trigger when timer fires (isolated from everything else)
    timer()
    
    # Complete isolation - no access to any app reactive values
    isolate({
      status <- getProfilingStatus()  # Pure function call
      
      if(status$isActive) {
        duration <- as.numeric(difftime(Sys.time(), status$startTime, units = "secs"))
        div(
          style = "background-color: #d4edda; border: 1px solid #c3e6cb; padding: 10px; border-radius: 5px;",
          icon("record-vinyl", style = "color: #155724;"),
          strong(" PROFILING ACTIVE"),
          br(),
          paste("Session:", status$sessionId),
          br(),
          paste("Duration:", round(duration, 1), "seconds"),
          br(),
          paste("File:", status$profileFile)
        )
      } else {
        div(
          style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 10px; border-radius: 5px;",
          icon("stop-circle", style = "color: #6c757d;"),
          strong(" PROFILING INACTIVE"),
          br(),
          "Click 'Start Profiling' before testing slow operations"
        )
      }
    })
  })
  
  # Button observers (completely isolated - no reactive contamination possible)
  observeEvent(input$startProfiling, {
    result <- startProfiling()  # Pure function call
    if(result) {
      showNotification("Profiling started", type = "message", duration = 3)
    } else {
      showNotification("Profiling already active", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$stopProfiling, {
    profileFile <- stopProfiling()  # Pure function call
    if(!is.null(profileFile)) {
      showNotification(paste("Profiling stopped. File:", profileFile), type = "message", duration = 5)
    } else {
      showNotification("No active profiling session", type = "warning", duration = 3)
    }
  }, ignoreInit = TRUE)
  
  # Download handler (pure function, no reactive elements)
  output$downloadProfile <- downloadHandler(
    filename = function() {
      status <- getProfilingStatus()  # Pure function call
      if(!is.null(status$profileFile) && file.exists(status$profileFile)) {
        return(status$profileFile)
      } else {
        return("iPlotDesigner_profile.Rprof")
      }
    },
    content = function(file) {
      status <- getProfilingStatus()  # Pure function call
      if(!is.null(status$profileFile) && file.exists(status$profileFile)) {
        file.copy(status$profileFile, file)
      } else {
        # Create empty file if no profile available
        writeLines("No profile data available", file)
      }
    }
  )
  
  debugLog("PROFILING_SERVER: Delayed profiling server initialization completed", "PROFILING_INIT")
}

debugLog("DEBUG_PROFILING_SERVER: Delayed server functions defined", "PROFILING_START")
