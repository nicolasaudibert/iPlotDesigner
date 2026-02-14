# UI components and layout with tabbed version and safe profiling
# Load the modular UI assembly system with tabbed properties
# Minimal modification to existing code

# Load profiling UI functions only (no reactive elements)
source("modules/debug_profiling_ui_only.R", encoding = "UTF-8")

# Load the modular UI assembly system
source("modules/ui_main_assembly.R", encoding = "UTF-8")

# Load tabbed variable properties UI functions (no output definitions here)
source("modules/variable_properties_ui_functions.R", encoding = "UTF-8")

# Function to create advanced code generation options UI with unique IDs
createAdvancedCodeOptionsUI <- function(labels, plotType = "general") {
  
  # Create unique IDs for this plot type
  options_id <- paste0("advancedCodeOptions_", plotType)
  content_id <- paste0("advancedCodeContent_", plotType)
  chevron_id <- paste0("advancedCodeChevron_", plotType)
  js_function_name <- paste0("toggleAdvancedCodeOptions_", plotType)
  
  # Get labels with safe fallbacks
  advancedOptionsTitle <- safeGetUILabel("codeAdvancedOptionsTitle", labels, "Advanced Code Generation Options")
  metadataIncludeLabel <- safeGetUILabel("codeOperationsMetadataInclude", labels, "Include metadata in generated code")
  timestampsIncludeLabel <- safeGetUILabel("codeOperationsTimestampsInclude", labels, "Include operation timestamps")
  platformIncludeLabel <- safeGetUILabel("codeOperationsPlatformInclude", labels, "Include R version and platform info")
  verificationStepsLabel <- safeGetUILabel("codeVerificationStepsInclude", labels, "Include data verification steps  ")
  # Hidden options - kept for potential future use
  # errorHandlingLabel <- safeGetUILabel("codeErrorHandlingInclude", labels, "Include error handling in operations")
  # performanceOptimLabel <- safeGetUILabel("codePerformanceOptimInclude", labels, "Use performance optimization")
    
  # Use div with border instead of wellPanel for more compact display
  div(
    style = "margin-top: 10px; padding: 8px; border: 1px solid #ddd; border-radius: 4px; background-color: #f8f9fa;",
    
    # Collapsible header
    div(
      style = "cursor: pointer; margin-bottom: 10px;",
      onclick = paste0(js_function_name, "()"),
      h6(
        icon("cog", style = "margin-right: 8px; color: #6c757d;"),
        advancedOptionsTitle,
        icon("chevron-down", id = chevron_id, style = "float: right; color: #6c757d; transition: transform 0.3s;")
      )
    ),
    
    # Collapsible content using fluidRow for proper Shiny structure
    div(
      id = content_id,
      style = "display: none;",
      
      fluidRow(
        column(4,
               checkboxInput(
                 paste0("includeOperationsMetadata_", plotType), 
                 metadataIncludeLabel, 
                 value = tryCatch(ENHANCED_CODE_CONFIG$include_session_info, error = function(e) TRUE)
               )
        ),
        column(4,
               checkboxInput(
                 paste0("includeOperationsTimestamps_", plotType), 
                 timestampsIncludeLabel, 
                 value = tryCatch(ENHANCED_CODE_CONFIG$include_operation_timestamps, error = function(e) TRUE)
               )
        ),
        column(4,
               checkboxInput(
                 paste0("includePlatformInfo_", plotType), 
                 platformIncludeLabel, 
                 value = tryCatch(ENHANCED_CODE_CONFIG$include_r_version, error = function(e) FALSE)
               )
        )
      ),
      
      fluidRow(
        # Hidden option - error handling
        # column(4,
        #        checkboxInput(
        #          paste0("includeErrorHandling_", plotType), 
        #          errorHandlingLabel, 
        #          value = tryCatch(ENHANCED_CODE_CONFIG$include_error_handling, error = function(e) TRUE)
        #        )
        # ),
        column(4,
               checkboxInput(
                 paste0("includeVerificationSteps_", plotType), 
                 verificationStepsLabel, 
                 value = tryCatch(ENHANCED_CODE_CONFIG$include_verification_steps, error = function(e) TRUE)
               )
        )
      ),
      
      fluidRow(
        column(12,
               div(
                 style = "margin-top: 10px; padding: 8px; background-color: #e3f2fd; border-radius: 3px;",
                 tags$small(
                   icon("info-circle", style = "margin-right: 5px; color: #1976d2;"),
                   safeGetUILabel("codeAdvancedOptionsHelp", labels, "These options control how user operations are documented in the generated R code. Changes apply to newly generated code.")
                 )
               )
        )
      )
    ),
    
    # JavaScript for toggle functionality with unique function name
    tags$script(HTML(paste0("
      function ", js_function_name, "() {
        var content = document.getElementById('", content_id, "');
        var chevron = document.getElementById('", chevron_id, "');
        
        if (content.style.display === 'none') {
          content.style.display = 'block';
          chevron.style.transform = 'rotate(180deg)';
        } else {
          content.style.display = 'none';
          chevron.style.transform = 'rotate(0deg)';
        }
      }
    ")))
  )
}

# Enhanced createPlotControls function with advanced options
createPlotControls <- function(plotType, labels) {
  # Use safe label access to prevent cascades
  showCodeLabel <- safeGetUILabel("showCodeLabel", labels, "Show R code")
  copyCodeLabel <- safeGetUILabel("copyCodeLabel", labels, "Copy code")
  downloadCodeLabel <- safeGetUILabel("downloadCodeLabel", labels, "Download script")
  downloadPlotLabel <- safeGetUILabel("downloadPlotLabel", labels, "Download plot")
  
  tagList(
    fluidRow(
      column(3, checkboxInput(paste0(plotType, "ShowCode"), showCodeLabel, FALSE)),
      column(3, conditionalPanel(
        condition = paste0("input.", plotType, "ShowCode"),
        actionButton(paste0(plotType, "CopyCode"), copyCodeLabel, icon = icon("copy"))
      )),
      column(3, conditionalPanel(
        condition = paste0("input.", plotType, "ShowCode"),
        downloadButton(paste0(plotType, "DownloadCode"), downloadCodeLabel)
      )),
      column(3, downloadButton(paste0("download", tools::toTitleCase(plotType), "Plot"), downloadPlotLabel))
    ),
    
    # Advanced code options - show only when code is visible
    conditionalPanel(
      condition = paste0("input.", plotType, "ShowCode"),
      createAdvancedCodeOptionsUI(labels, plotType)
    ),
    
    conditionalPanel(
      condition = paste0("input.", plotType, "ShowCode"),
      br(),
      aceEditor(paste0(plotType, "PlotCode"), mode = "r", theme = "monokai", readOnly = TRUE, height = "400px")
    )
  )
}

debugLog("UI_COMPONENTS_TABBED: Tabbed UI system initialized with safe profiling integration", "UI_INIT")
