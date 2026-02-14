# Advanced Theme Options UI Module - Main Entry Point
# Modular architecture based on theme_light() with tabbed interface

# Function to create advanced theme options UI (collapsed by default)
createAdvancedThemeOptionsUI <- function(labels) {
  
  # Unique IDs for this component
  options_id <- "advancedThemeOptions"
  content_id <- "advancedThemeContent"
  chevron_id <- "advancedThemeChevron"
  js_function_name <- "toggleAdvancedThemeOptions"
  
  # Get main labels
  advancedThemeTitle <- safeGetUILabel("themeAdvancedOptionsTitle", labels, "Advanced Theme Options")
  resetToDefaultLabel <- safeGetUILabel("themeResetToDefaultLabel", labels, "Reset to defaults")
  themeOptionsHelpText <- safeGetUILabel("themeAdvancedOptionsHelp", labels, 
    "These options allow fine-grained control of plot appearance. Changes apply immediately to displayed plots and generated code.")
  
  # Get section titles for tabs (using existing label keys)
  backgroundTabTitle <- safeGetUILabel("themeBackgroundSection", labels, "Background Elements")
  titlesTabTitle <- safeGetUILabel("themeTitlesSection", labels, "Titles Customization")
  axisTextTabTitle <- safeGetUILabel("themeAxisTextSection", labels, "Axis Text Customization")
  gridBorderTabTitle <- safeGetUILabel("themeGridBorderSection", labels, "Grids and Borders")
  
  # Use div with border instead of wellPanel for compact display
  div(
    style = "margin-top: 10px; padding: 8px; border: 1px solid #ddd; border-radius: 4px; background-color: #f8f9fa;",
    
    # Collapsible header
    div(
      style = "cursor: pointer; margin-bottom: 10px;",
      onclick = paste0(js_function_name, "()"),
      h6(
        icon("palette", style = "margin-right: 8px; color: #6c757d;"),
        advancedThemeTitle,
        icon("chevron-down", id = chevron_id, style = "float: right; color: #6c757d; transition: transform 0.3s;")
      )
    ),
    
    # Collapsible content with tabs
    div(
      id = content_id,
      style = "display: none;",
      
      # Tabbed interface for theme sections
      tabsetPanel(
        id = "themeCustomizationTabs",
        type = "tabs",
        
        # Tab 1: Background Elements
        tabPanel(
          title = backgroundTabTitle,
          value = "background_tab",
          div(
            style = "padding-top: 15px;",
            createThemeBackgroundUI(labels)
          )
        ),
        
        # Tab 2: Titles Customization
        tabPanel(
          title = titlesTabTitle,
          value = "titles_tab",
          div(
            style = "padding-top: 15px;",
            createThemeTitlesUI(labels)
          )
        ),
        
        # Tab 3: Axis Text Customization
        tabPanel(
          title = axisTextTabTitle,
          value = "axis_text_tab",
          div(
            style = "padding-top: 15px;",
            createThemeAxisTextUI(labels)
          )
        ),
        
        # Tab 4: Grids and Borders
        tabPanel(
          title = gridBorderTabTitle,
          value = "grid_border_tab",
          div(
            style = "padding-top: 15px;",
            createThemeGridBorderUI(labels)
          )
        )
      ),
      
      # Advanced customization note (outside tabs, always visible)
      fluidRow(
        column(12,
          div(
            style = "margin-top: 20px; padding: 10px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 3px;",
            tags$small(
              icon("info-circle", style = "margin-right: 5px; color: #856404;"),
              safeGetUILabel("themeAdvancedCustomizationNote", labels, 
                "Note: This interface covers the most common theme options. For advanced customization (margins, spacing, element positions, etc.), you can modify the theme() section in the generated R code.")
            )
          )
        )
      ),
      
      # Help text and reset button (outside tabs, always visible)
      fluidRow(
        column(9,
          div(
            style = "margin-top: 15px; padding: 8px; background-color: #e3f2fd; border-radius: 3px;",
            tags$small(
              icon("info-circle", style = "margin-right: 5px; color: #1976d2;"),
              themeOptionsHelpText
            )
          )
        ),
        column(3,
          div(
            style = "margin-top: 15px;",
            actionButton("themeResetDefaults", resetToDefaultLabel,
                        icon = icon("undo"),
                        style = "width: 100%;")
          )
        )
      )
    ),
    
    # JavaScript for toggle functionality
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

debugLog("UI_THEME_ADVANCED_OPTIONS_MAIN: Main theme options UI with tabs loaded", "APP_STARTUP")
