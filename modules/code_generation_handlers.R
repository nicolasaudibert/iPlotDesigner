# Code generation event handlers - modular version
# This file loads all modularized code handler components

# Load all code handler modules
source("modules/code_handlers_common.R", local = TRUE, encoding = "UTF-8")
source("modules/code_handlers_display.R", local = TRUE, encoding = "UTF-8")
source("modules/code_handlers_copy.R", local = TRUE, encoding = "UTF-8")
source("modules/code_handlers_download.R", local = TRUE, encoding = "UTF-8")
source("modules/code_handlers_autoupdate.R", local = TRUE, encoding = "UTF-8")

debugLog("CODE_GENERATION_HANDLERS_MODULAR: All code handler modules loaded successfully", "CODE_INIT")
