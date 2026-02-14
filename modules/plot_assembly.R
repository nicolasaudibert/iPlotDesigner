# Plot Assembly Module
# Main assembly and module loader for plot functions

# Load all plot modules
source("modules/plot_core_helpers.R", encoding = "UTF-8")
source("modules/plot_bar_box.R", encoding = "UTF-8")
source("modules/plot_violin_ridge.R", encoding = "UTF-8")
source("modules/plot_histogram_density.R", encoding = "UTF-8")

# Export all plot functions for compatibility
# This ensures existing code continues to work without changes

debugLog("PLOT_ASSEMBLY: All plot modules loaded successfully", "PLOT_INIT")
