# R code generation system with fine-grained modular architecture
# Main module that loads all code generation components

# Load core functions first
source("modules/code_generation_core.R", encoding = "UTF-8")

# Load basic plot functions
source("modules/code_generation_plots_basic.R", encoding = "UTF-8")

# Load individual advanced plot modules
source("modules/code_generation_violin.R", encoding = "UTF-8")
source("modules/code_generation_histogram.R", encoding = "UTF-8")
source("modules/code_generation_density.R", encoding = "UTF-8")
source("modules/code_generation_ridge.R", encoding = "UTF-8")

# Module loading verification
debugLog("CODE_GENERATION: Fine-grained modular architecture loaded successfully", "MODULE_INIT")
debugLog("CODE_GENERATION: Core functions loaded from code_generation_core.R", "MODULE_INIT")
debugLog("CODE_GENERATION: Basic plots loaded from code_generation_plots_basic.R", "MODULE_INIT")
debugLog("CODE_GENERATION: Advanced plots loaded from individual modules", "MODULE_INIT")
debugLog("CODE_GENERATION: Available functions - generateBarPlotCode, generateBoxPlotCode, generateViolinPlotCode, generateHistogramCode, generateDensityPlotCode, generateRidgePlotCode", "MODULE_INIT")

debugLog("CODE_GENERATION: User operations code generation system ready", "CODE_INIT")
