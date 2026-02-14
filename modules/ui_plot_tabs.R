# UI Plot Tabs Module - CLEAN MODULAR VERSION
# Main assembly for all plot type tabs

# Load plot tab modules
source("modules/ui_plot_common.R", encoding = "UTF-8")
source("modules/ui_plot_basic_tabs.R", encoding = "UTF-8")
source("modules/ui_plot_advanced_tabs.R", encoding = "UTF-8")

# Function to create all plot tabs section
createPlotTabsSection <- function(labels) {
  # Cache common labels used across multiple tabs
  commonLabels <- cacheCommonPlotLabels(labels)
  
  conditionalPanel(
    condition = "output.validVariablesSelected",
    
    # WARNING MESSAGE FOR DISABLED PLOTS - RENDERED DYNAMICALLY
    uiOutput("duplicateVariableWarning"),
    
    tabsetPanel(
      id = "plotTabs",
      
      # Basic plot tabs
      createBarChartTab(labels, commonLabels),
      createBoxPlotTab(labels, commonLabels),
      createViolinPlotTab(labels, commonLabels),
      
      # Advanced plot tabs (including dynamic content)
      createHistogramTab(labels),
      createDensityTab(labels),
      createRidgeTab(labels, commonLabels)
    )
  )
}

debugLog("UI_PLOT_TABS_CLEAN: Clean modular plot tabs system loaded", "UI_INIT")
