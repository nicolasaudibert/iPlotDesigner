# Variable management system assembly module
# Coordinate loading and integration of variable management modules

# Load variable management modules in correct dependency order
tryCatch({
  source("modules/variable_color_management.R", local = TRUE, encoding = "UTF-8")
  debugLog("VARIABLE_MANAGEMENT_ASSEMBLY: variable_color_management.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("VARIABLE_MANAGEMENT_ASSEMBLY: Error loading variable_color_management.R:", e$message), "MODULE_ERROR")
  stop("Critical variable color management module failed to load")
})

tryCatch({
  source("modules/variable_modality_handling.R", local = TRUE, encoding = "UTF-8")
  debugLog("VARIABLE_MANAGEMENT_ASSEMBLY: variable_modality_handling.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("VARIABLE_MANAGEMENT_ASSEMBLY: Error loading variable_modality_handling.R:", e$message), "MODULE_ERROR")
})

tryCatch({
  source("modules/variable_properties_helpers.R", local = TRUE, encoding = "UTF-8")
  debugLog("VARIABLE_MANAGEMENT_ASSEMBLY: variable_properties_helpers.R loaded", "MODULE_LOAD")
}, error = function(e) {
  debugLog(paste("VARIABLE_MANAGEMENT_ASSEMBLY: Error loading variable_properties_helpers.R:", e$message), "MODULE_ERROR")
  stop("Critical variable properties helpers module failed to load")
})

# Function to swap variable properties - kept exactly as in original
swapVariableProperties <- function(var1Num, var2Num, values, input, session) {
  # Store properties of var1
  var1_modalities <- values[[paste0("var", var1Num, "_modalities")]]
  var1_selected <- values[[paste0("var", var1Num, "_selected")]]
  var1_colors <- values[[paste0("var", var1Num, "_colors")]]
  var1_order <- values[[paste0("var", var1Num, "_order")]]
  
  # Store properties of var2
  var2_modalities <- values[[paste0("var", var2Num, "_modalities")]]
  var2_selected <- values[[paste0("var", var2Num, "_selected")]]
  var2_colors <- values[[paste0("var", var2Num, "_colors")]]
  var2_order <- values[[paste0("var", var2Num, "_order")]]
  
  # Swap the properties
  values[[paste0("var", var1Num, "_modalities")]] <- var2_modalities
  values[[paste0("var", var1Num, "_selected")]] <- var2_selected
  values[[paste0("var", var1Num, "_colors")]] <- var2_colors
  values[[paste0("var", var1Num, "_order")]] <- var2_order
  
  values[[paste0("var", var2Num, "_modalities")]] <- var1_modalities
  values[[paste0("var", var2Num, "_selected")]] <- var1_selected
  values[[paste0("var", var2Num, "_colors")]] <- var1_colors
  values[[paste0("var", var2Num, "_order")]] <- var1_order
  
  # Trigger UI update
  values$propertiesSwapped <- Sys.time()
  
  debugLog(paste("Swapped properties between variable", var1Num, "and variable", var2Num), "VAR_PROPERTIES")
}

# Verify variable management system integrity - check only original functions
verifyVariableManagementSystem <- function() {
  required_functions <- c("createVariablePropertiesUI", "getSelectedModalities", "getModalityColors", "swapVariableProperties")
  
  for(func_name in required_functions) {
    if(!exists(func_name, envir = .GlobalEnv)) {
      debugLog(paste("VARIABLE_MANAGEMENT_ASSEMBLY: Missing required function:", func_name), "MODULE_ERROR")
      return(FALSE)
    }
  }
  
  debugLog("VARIABLE_MANAGEMENT_ASSEMBLY: All original variable management functions verified", "MODULE_LOAD")
  return(TRUE)
}

# Initialize variable management system
initializeVariableManagementSystem <- function() {
  if(verifyVariableManagementSystem()) {
    debugLog("VARIABLE_MANAGEMENT_ASSEMBLY: Variable management system initialized successfully", "APP_STARTUP")
    return(TRUE)
  } else {
    debugLog("VARIABLE_MANAGEMENT_ASSEMBLY: Variable management system initialization failed", "APP_ERROR") 
    return(FALSE)
  }
}

# Initialize system
variable_management_system_ready <- initializeVariableManagementSystem()

debugLog("VARIABLE_MANAGEMENT_ASSEMBLY: Modular variable management system assembly completed", "APP_STARTUP")
