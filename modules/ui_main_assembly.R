# UI Main Assembly Module - TABBED VERSION
# Assembles all UI modules into the complete interface with tabbed properties

# Load all UI modules
source("modules/ui_layout_core.R", encoding = "UTF-8")
source("modules/ui_file_input.R", encoding = "UTF-8")
source("modules/ui_variable_selection_tabbed.R", encoding = "UTF-8")
source("modules/ui_plot_tabs.R", encoding = "UTF-8")
source("modules/ui_general_settings.R", encoding = "UTF-8")

# Load modular theme UI sub-modules
source("modules/ui_theme_background.R", encoding = "UTF-8")
source("modules/ui_theme_titles.R", encoding = "UTF-8")
source("modules/ui_theme_axis_text.R", encoding = "UTF-8")
source("modules/ui_theme_grid_border.R", encoding = "UTF-8")
source("modules/ui_theme_main.R", encoding = "UTF-8")  # Load last as it calls the sub-modules

# Main UI assembly function
ui_main <- function(labels) {
  # Create the main layout structure
  layout <- createMainLayoutStructure(labels)
  
  # Add the file input section
  layout$children[[3]] <- createFileInputSection(labels)
  
  # Add the main content within conditional panel
  layout$children[[4]] <- createFileConditionalPanel(
    tagList(
      # Variable selection section WITH TABBED PROPERTIES
      createVariableSelectionSection(labels),
      
      # Plot tabs section
      createPlotTabsSection(labels),
      
      # General settings section
      createGeneralSettingsSection(labels)
    )
  )
  
  return(layout)
}

debugLog("UI_MAIN_ASSEMBLY_TABBED: Modular theme system loaded successfully", "UI_INIT")
