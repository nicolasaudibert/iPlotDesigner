# Variable observers module - Fixed loading with central triggers
# This file loads all the modularized observer components

# Load reactive protection system
source("modules/reactive_protection_system.R", local = TRUE, encoding = "UTF-8")

# Load modal dialog handlers (isolated version)
source("modules/modal_dialog_handlers.R", local = TRUE, encoding = "UTF-8")

# Load UI reset handlers
source("modules/ui_reset_handlers.R", local = TRUE, encoding = "UTF-8")

# Load variable validation core
source("modules/variable_validation_core.R", local = TRUE, encoding = "UTF-8")

# Load main variable observers (now includes uniform VI3 observer)
source("modules/variable_observers_main.R", local = TRUE, encoding = "UTF-8")

# Load variable swap handlers
source("modules/variable_swap_handlers.R", local = TRUE, encoding = "UTF-8")

# Initialize label cache
initializeLabelCache(labels)

# Load variable properties (now uses centrally defined triggers)
source("modules/variable_properties.R", local = TRUE, encoding = "UTF-8")

# Load the reactive coordination system (now uses centrally defined triggers)
source("modules/reactive_coordination_system.R", local = TRUE, encoding = "UTF-8")
