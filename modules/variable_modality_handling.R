# Variable modality handling module
# Modality selection, ordering, and validation functions

# Get selected modalities with stable IDs
getSelectedModalities <- function(varNum, modalities, input) {
  if(is.null(modalities) || length(modalities) == 0) {
    return(NULL)
  }
  
  modalityList <- unique(modalities)
  selected <- c()
  
  for(modality in modalityList) {
    # Use stable ID based on name
    modalityId <- gsub("[^[:alnum:]]", "_", modality)
    checkId <- paste0("var", varNum, "_check_", modalityId)
    
    if(!is.null(input[[checkId]]) && input[[checkId]]) {
      selected <- c(selected, modality)
    }
  }
  
  # Return all modalities if none are selected (safety)
  return(if(length(selected) > 0) selected else modalityList)
}

debugLog("VARIABLE_MODALITY_HANDLING: Modality selection and ordering functions loaded", "APP_STARTUP")
