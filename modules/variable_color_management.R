# Variable color management module
# Color validation, assignment, and fallback handling

# Get modality colors with stable IDs and fallback protection
getModalityColors <- function(varNum, modalities, input, existingColors = NULL) {
  # Safety check for empty or invalid inputs
  if(is.null(modalities) || length(modalities) == 0) {
    debugLog(paste("getModalityColors - No modalities provided for var", varNum), "APP_DEBUG")
    return(NULL)
  }
  
  modalityList <- unique(modalities)
  # Remove NA values
  modalityList <- modalityList[!is.na(modalityList)]
  
  if(length(modalityList) == 0) {
    debugLog(paste("getModalityColors - No valid modalities after cleaning for var", varNum), "APP_DEBUG")
    return(NULL)
  }
  
  colors <- setNames(character(length(modalityList)), modalityList)
  
  for(i in seq_along(modalityList)) {
    modality <- modalityList[i]
    # Use stable ID
    modalityId <- gsub("[^[:alnum:]]", "_", modality)
    colorId <- paste0("var", varNum, "_color_", modalityId)
    
    # Safety check for input existence
    if(!is.null(input[[colorId]]) && !is.na(input[[colorId]]) && input[[colorId]] != "") {
      colors[modality] <- input[[colorId]]
    } else if(!is.null(existingColors) && modality %in% names(existingColors) && 
              !is.na(existingColors[modality]) && existingColors[modality] != "") {
      colors[modality] <- existingColors[modality]
    } else {
      # Fallback to default colors
      defaultColor <- defaultColors[[paste0("var", varNum)]][((i-1) %% length(defaultColors[[paste0("var", varNum)]])) + 1]
      colors[modality] <- defaultColor
    }
  }
  
  # Double check: remove any empty or NA colors
  colors[colors == "" | is.na(colors)] <- "#1f77b4"  # Fallback blue color
  
  return(colors)
}

debugLog("VARIABLE_COLOR_MANAGEMENT: Color management functions loaded with validation and fallbacks", "APP_STARTUP")
