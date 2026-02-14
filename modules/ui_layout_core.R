# UI Layout Core Module
# CSS, JavaScript, and core layout structure

# Function to create head tags with CSS and JavaScript
createUIHeadTags <- function() {
  tags$head(
    # Define favicon
    tags$link(rel = "icon", type = "image/png", href = "iPlotDesigner.png?v=1"),
    tags$style(HTML("
      .well { background-color: #f5f5f5; }
      .nav-tabs > li > a { font-weight: bold; }
      .color-box { 
        width: 30px; 
        height: 30px; 
        border: 1px solid #ccc; 
        display: inline-block; 
        vertical-align: middle; 
        margin-right: 5px;
      }
      .modality-row {
        padding: 5px;
        margin: 2px;
        background-color: #f9f9f9;
        border-radius: 3px;
      }
      .modality-controls {
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .disabled-tab-content {
        text-align: center; 
        padding: 50px; 
        color: #666;
      }
      .disabled-tab {
        opacity: 0.6;
        pointer-events: none;
      }
      .insufficient-data-content {
        text-align: center;
        padding: 30px;
        color: #856404;
        background-color: #fff3cd;
        border: 1px solid #ffeaa7;
        border-radius: 5px;
        margin: 10px;
      }
      
      /* =================================================================
         HTML5 COLOR INPUTS - Optimized styles
         ================================================================= */
      
      /* Base styles for input[type='color'] */
      input[type='color'] {
        border: 2px solid #ddd !important;
        border-radius: 4px !important;
        cursor: pointer !important;
        transition: border-color 0.2s ease !important;
        outline: none !important;
      }
      
      /* Hover effect */
      input[type='color']:hover {
        border-color: #007bff !important;
        box-shadow: 0 0 0 2px rgba(0, 123, 255, 0.25) !important;
      }
      
      /* Focus effect */
      input[type='color']:focus {
        border-color: #007bff !important;
        box-shadow: 0 0 0 3px rgba(0, 123, 255, 0.25) !important;
      }
      
      /* Disabled state */
      input[type='color']:disabled {
        opacity: 0.6 !important;
        cursor: not-allowed !important;
        border-color: #ccc !important;
      }
      
      /* Special styling for modality rows */
      .modality-row input[type='color'] {
        margin-top: 2px;
      }
      
      /* Responsive adjustments */
      @media (max-width: 768px) {
        input[type='color'] {
          width: 40px !important;
          height: 30px !important;
        }
      }
    ")),
    
    # Minimal JavaScript - essential handlers only
    tags$script(HTML("
      $(document).ready(function() {
        console.log('iPlotDesigner UI loaded with HTML5 color inputs');
        
        // Handler for force input update messages (kept for compatibility)
        Shiny.addCustomMessageHandler('force_input_update', function(data) {
          console.log('Force update received for: ' + data.inputId + ' to value: ' + data.value);
          
          var element = $('#' + data.inputId);
          if (element.length > 0) {
            // Force the value change
            element.val(data.value).trigger('change');
            
            // Additional force for select2 elements
            if (element.hasClass('selectized') || element.data('selectize')) {
              var selectize = element[0].selectize;
              if (selectize) {
                selectize.setValue(data.value, true);
              }
            }
            
            // Force Shiny to recognize the change
            element.trigger('shiny:inputchanged');
          }
        });
        
        // Debug helper to monitor select input changes
        $(document).on('change', 'select[id^=\"independentVar\"]', function() {
          console.log('Select change detected: ' + this.id + ' = ' + this.value);
        });
        
        // HTML5 color input enhancement
        $(document).on('change', 'input[type=\"color\"]', function() {
          console.log('HTML5 color changed: ' + this.id + ' = ' + this.value);
        });
      });
    "))
  )
}

# Function to create main layout structure
createMainLayoutStructure <- function(labels) {
  windowTitle <- safeGetUILabel("windowTitle", labels, "iPlotDesigner - Interactive distribution comparison")
  
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = defaultTheme),
    createUIHeadTags(),
    titlePanel("", windowTitle = windowTitle),
    htmlOutput("introText")
  )
}