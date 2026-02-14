# Debug performance monitoring module
# Performance monitoring, timing functions, and general reactive loop detection

# Global counters for reactive loop detection
reactiveCallCounts <- list()
lastReactiveCallTime <- list()
loopDetectionThreshold <- 10  # Number of calls to trigger loop detection
loopTimeWindow <- 2  # Time window in seconds

# VI3 specific functions - commented for future use
# Uncomment and adapt these functions if variable-specific debugging is needed

# VI3 specific timing tracking - COMMENTED
# vi3EventTimestamps <- list()
# vi3ProcessingStages <- c()

# Track VI3 processing timeline - COMMENTED
# trackVI3Event <- function(eventName, details = "") {
#   if(debugMode) {
#     timestamp <- as.numeric(Sys.time())
#     elapsed <- if(!is.null(debugStartTime)) {
#       round(timestamp - as.numeric(debugStartTime), 3)
#     } else 0
#     
#     debugLog(paste("VI3_TIMELINE:", eventName, details), "PERFORMANCE_WARN")
#     
#     # Store in timeline
#     vi3EventTimestamps[[paste0("event_", length(vi3EventTimestamps) + 1)]] <<- list(
#       event = eventName,
#       timestamp = timestamp,
#       elapsed = elapsed,
#       details = details
#     )
#   }
# }

# Generate VI3 processing report - COMMENTED
# generateVI3Report <- function() {
#   if(debugMode && length(vi3EventTimestamps) > 0) {
#     cat("\n=== VI3 PROCESSING TIMELINE REPORT ===\n")
#     cat("Total VI3 events tracked:", length(vi3EventTimestamps), "\n")
#     
#     # Calculate gaps between events
#     timestamps <- sapply(vi3EventTimestamps, function(x) x$elapsed)
#     if(length(timestamps) > 1) {
#       gaps <- diff(timestamps)
#       maxGap <- max(gaps)
#       maxGapIndex <- which.max(gaps)
#       
#       cat("Largest time gap:", round(maxGap, 3), "seconds between events", maxGapIndex, "and", maxGapIndex + 1, "\n")
#       
#       if(maxGap > 5) {
#         cat("*** CRITICAL: Gap >5 seconds detected - potential bottleneck ***\n")
#         cat("Event before gap:", vi3ProcessingStages[maxGapIndex], "\n")
#         cat("Event after gap:", vi3ProcessingStages[maxGapIndex + 1], "\n")
#       }
#     }
#     
#     cat("\nFull timeline:\n")
#     for(i in seq_along(vi3ProcessingStages)) {
#       cat(i, ":", vi3ProcessingStages[i], "\n")
#     }
#     cat("=====================================\n\n")
#   }
# }

# Active general performance functions

# Trace reactive updates with loop detection
traceReactive <- function(name, action = "START") {
  if(debugMode && isCategoryEnabled("REACTIVE_TRACE")) {
    callId <- paste(name, action)
    currentTime <- as.numeric(Sys.time())
    
    # Initialize counters if needed
    if(is.null(reactiveCallCounts[[callId]])) {
      reactiveCallCounts[[callId]] <<- 0
      lastReactiveCallTime[[callId]] <<- currentTime
    }
    
    # Check if we're in a potential loop
    timeDiff <- currentTime - lastReactiveCallTime[[callId]]
    if(timeDiff < loopTimeWindow) {
      reactiveCallCounts[[callId]] <<- reactiveCallCounts[[callId]] + 1
      
      if(reactiveCallCounts[[callId]] > loopDetectionThreshold) {
        debugLog(paste("*** POTENTIAL REACTIVE LOOP DETECTED ***", callId, 
                      "- Called", reactiveCallCounts[[callId]], "times in", round(timeDiff, 2), "seconds"), "LOOP_WARN")
        
        # Special handling for variable loops (generalized from VI3)
        if(grepl("independentVar|var[123]", callId)) {
          debugLog(paste("*** VARIABLE REACTIVE LOOP CONFIRMED ***", callId), "PERFORMANCE_WARN")
        }
      }
    } else {
      # Reset counter if enough time has passed
      reactiveCallCounts[[callId]] <<- 1
    }
    
    lastReactiveCallTime[[callId]] <<- currentTime
    
    debugLog(paste("REACTIVE_TRACE", action, ":", name, 
                  "[count:", reactiveCallCounts[[callId]], "]"), "REACTIVE_TRACE")
  }
}

# Measure execution time with performance monitoring
measureTime <- function(expr, description = "") {
  if(debugMode) {
    startTime <- Sys.time()
    result <- expr
    endTime <- Sys.time()
    duration <- as.numeric(difftime(endTime, startTime, units = "secs"))
    
    if(isCategoryEnabled("PERFORMANCE_TIMING")) {
      debugLog(paste("TIMING:", description, "took", round(duration, 3), "seconds"), "PERFORMANCE_TIMING")
    }
    
    # Warning for slow operations
    if(duration > 1) {
      debugLog(paste("*** SLOW OPERATION WARNING ***", description, 
                    "took", round(duration, 3), "seconds"), "PERFORMANCE_WARN")
    }
    
    # Additional warning for very slow operations (previously VI3-specific)
    if(duration > 5) {
      debugLog(paste("*** VERY SLOW OPERATION ***", description, 
                    "took", round(duration, 3), "seconds"), "PERFORMANCE_WARN")
    }
    
    return(result)
  } else {
    return(expr)
  }
}

# Trace observer execution with timing
traceObserverTiming <- function(name, action, startTime = NULL) {
  if(debugMode && isCategoryEnabled("OBSERVER_TIMING")) {
    if(is.null(startTime)) {
      # Starting - return current time
      currentTime <- Sys.time()
      debugLog(paste("OBSERVER_TIMING START:", name, action), "OBSERVER_TIMING")
      return(currentTime)
    } else {
      # Ending - calculate duration
      endTime <- Sys.time()
      duration <- as.numeric(difftime(endTime, startTime, units = "secs"))
      debugLog(paste("OBSERVER_TIMING END:", name, action, 
                    "- Duration:", round(duration, 3), "seconds"), "OBSERVER_TIMING")
      
      # Warn about slow observers
      if(duration > 1) {
        debugLog(paste("*** SLOW OBSERVER WARNING ***", name, action, 
                      "took", round(duration, 3), "seconds"), "PERFORMANCE_WARN")
      }
      
      # Warning for very slow observers (generalized from VI3-specific)
      if(duration > 5) {
        debugLog(paste("*** VERY SLOW OBSERVER ***", name, action, 
                      "took", round(duration, 3), "seconds"), "PERFORMANCE_WARN")
      }
    }
  }
}

# Reset all reactive call counters
resetReactiveCounters <- function() {
  reactiveCallCounts <<- list()
  lastReactiveCallTime <<- list()
  debugLog("Reactive call counters reset", "REACTIVE_TRACE")
}

debugLog("DEBUG_PERFORMANCE: Performance monitoring and general reactive loop detection loaded", "APP_STARTUP")
