# iPlotDesigner - Interactive distribution comparison
# Main application file with HTML5 color inputs and performance optimization

# Debug mode configuration
options(shiny.reactlog = F)
options(shiny.error = F)
options(shiny.trace = F)

# Load required libraries
require(shiny)
require(shinyjs)
require(bslib)
require(bsicons)
require(tidyverse)
require(ggridges)
require(DT)
require(Cairo)
require(readxl)
require(readODS)
require(shinyAce)
require(sortable)
require(clipr)
require(rlang)
require(data.table)  # Required for performance optimization
# Library jsonlite is also required but loaded later

# Source debug utilities first
source("modules/debug_utils.R", encoding = "UTF-8")

# Initialize debug session
initializeDebug()
debugLog("Application starting", "APP_STARTUP")

# Source modular components
source("modules/global_settings.R", encoding = "UTF-8")
debugLog("Loaded global_settings.R", "APP_STARTUP")

source("modules/early_utilities.R", encoding = "UTF-8")
debugLog("Loaded early_utilities.R", "APP_STARTUP")

source("modules/performance_utilities.R", encoding = "UTF-8")
debugLog("Loaded performance_utilities.R", "APP_STARTUP")

source("modules/label_system_core.R", encoding = 'UTF-8')
debugLog("Loaded label_system_core.R", "APP_STARTUP")

# Label cache system
source("modules/label_cache_optimization.R", encoding = "UTF-8")
debugLog("Loaded label_cache_optimization.R", "APP_STARTUP")

source('modules/validation_functions.R', encoding = 'UTF-8')
debugLog("Loaded validation_functions.R", "APP_STARTUP")

# Load localization before cache init
displayedLabelsAndPrompts <- read_tsv(
  file = labelsFile, 
  show_col_types = F
)
debugLog("Localization loaded", "APP_STARTUP")

# Initialize cache with actual labels
initializeLabelCache(displayedLabelsAndPrompts)
debugLog("Label cache initialized", "APP_STARTUP")

# Load modular UI system
source("modules/ui_components.R", encoding = "UTF-8")
debugLog("Loaded modular ui_components.R", "APP_STARTUP")

# Load other modules
source("modules/data_handling.R", encoding = "UTF-8")
debugLog("Loaded data_handling.R", "APP_STARTUP")

source("modules/plot_functions.R", encoding = "UTF-8")
debugLog("Loaded plot_functions.R", "APP_STARTUP")

source("modules/code_generation.R", encoding = "UTF-8")
debugLog("Loaded code_generation.R", "APP_STARTUP")

source("modules/variable_management.R", encoding = "UTF-8")
debugLog("Loaded variable_management.R", "APP_STARTUP")

# Main Shiny app
debugLog("Starting Shiny app with HTML5 color inputs and data.table optimization", "APP_STARTUP")
shinyApp(
  ui = ui_main(displayedLabelsAndPrompts),
  server = server_main(displayedLabelsAndPrompts)
)
