# Debug profiling UI functions - UI creation only
# Only UI creation functions, no reactive elements, no server outputs
# This file is loaded early and contains *zero* reactive code

# Create profiling UI controls (pure UI function, no reactive elements)
createProfilingUI <- function(labels) {
  # Check if profiling is enabled
  if(!exists("isProfilingEnabled") || !isProfilingEnabled()) {
    debugLog("PROFILING_UI: Profiling disabled, returning NULL", "PROFILING_INIT")
    return(NULL)
  }
  
  debugLog("PROFILING_UI: Creating profiling UI controls", "PROFILING_INIT")
  
  profilingTitle <- safeGetUILabel("profilingTitle", labels, "Performance Profiling")
  startProfilingLabel <- safeGetUILabel("startProfilingLabel", labels, "Start Profiling")
  stopProfilingLabel <- safeGetUILabel("stopProfilingLabel", labels, "Stop Profiling")
  downloadProfileLabel <- safeGetUILabel("downloadProfileLabel", labels, "Download Profile")
  profilingInstructions <- safeGetUILabel("profilingInstructions", labels, 
    "Use profiling to analyze performance bottlenecks. Start before triggering slow operations.")
  
  wellPanel(
    h4(profilingTitle),
    p(profilingInstructions),
    
    fluidRow(
      column(4, 
        actionButton("startProfiling", startProfilingLabel, 
                     icon = icon("play"), 
                     style = "background-color: #28a745; color: white; width: 100%;")
      ),
      column(4,
        actionButton("stopProfiling", stopProfilingLabel,
                     icon = icon("stop"),
                     style = "background-color: #dc3545; color: white; width: 100%;")
      ),
      column(4,
        downloadButton("downloadProfile", downloadProfileLabel,
                       style = "background-color: #007bff; color: white; width: 100%;")
      )
    ),
    
    br(),
    
    # Status display (placeholder - will be populated by server)
    fluidRow(
      column(12,
        uiOutput("profilingStatus")
      )
    )
  )
}

debugLog("DEBUG_PROFILING_UI_ONLY: UI creation functions loaded (no reactive elements)", "PROFILING_INIT")
