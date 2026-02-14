# UI outputs selectors module
# Generate variable selector UI elements

# HTML header output (no reactive dependencies)
output$introText <- renderUI({
  HTML(paste(readLines(htmlHeaderFile, encoding = "UTF-8"), collapse = "\n"))
})

# Dependent variable selector UI
output$dependentVarUI <- renderUI({
  # Direct reactive dependency on filedata
  current_filedata <- values$filedata
  
  if(is.null(current_filedata)) {
    return(NULL)
  }
  
  numericVars <- names(current_filedata)[sapply(current_filedata, is.numeric)]
  label <- getCachedLabel("dependentVarLabel", labels, "Dependent variable (quantitative)")
  
  selectInput("dependentVar", label, choices = c("", numericVars))
})

# Independent variable 1 selector UI
output$independentVar1UI <- renderUI({
  # Direct reactive dependency on filedata
  current_filedata <- values$filedata
  
  if(is.null(current_filedata)) {
    return(NULL)
  }
  
  allVars <- names(current_filedata)
  label <- getCachedLabel("independentVar1Label", labels, "Independent variable 1 (primary grouping)")
  
  selectInput("independentVar1", label, choices = c("", allVars))
})

# Independent variable 2 selector UI
output$independentVar2UI <- renderUI({
  # Direct reactive dependency on filedata
  current_filedata <- values$filedata
  
  if(is.null(current_filedata)) {
    return(NULL)
  }
  
  allVars <- names(current_filedata)
  
  # Use cached label to avoid cascades
  label <- getCachedLabel("independentVar2Label", labels, "Independent variable 2 (secondary grouping/facet)")
  noVar2Label <- getCachedLabel("noIndependentVar2Label", labels, "-- No independent variable 2 --")
  
  allChoices <- c("", noVar2Label, allVars)
  names(allChoices) <- c("", noVar2Label, allVars)
  
  selectInput("independentVar2", label, choices = allChoices)
})

# Independent variable 3 selector UI
output$independentVar3UI <- renderUI({
  # Direct reactive dependency on filedata
  current_filedata <- values$filedata
  
  if(is.null(current_filedata)) {
    return(NULL)
  }
  
  allVars <- names(current_filedata)
  
  # Use cached labels to avoid cascades  
  label <- getCachedLabel("independentVar3Label", labels, "Independent variable 3 (faceting)")
  noVar3Label <- getCachedLabel("noIndependentVar3Label", labels, "-- No independent variable 3 --")
  
  allChoices <- c("", noVar3Label, allVars)
  names(allChoices) <- c("", noVar3Label, allVars)
  
  selectInput("independentVar3", label, choices = allChoices)
})

# Error type selector for bar plots
output$barErrorTypeUI <- renderUI({
  label <- getCachedLabel("errorTypeLabel", labels, "Error bars")
  errorTypeSELabel <- getCachedLabel("errorTypeSELabel", labels, "Standard error")
  errorTypeSDLabel <- getCachedLabel("errorTypeSDLabel", labels, "Standard deviation")
  
  selectInput("barErrorType", label,
             choices = setNames(c("se", "sd"), c(errorTypeSELabel, errorTypeSDLabel)))
})

# Export format selector
output$exportFormatUI <- renderUI({
  label <- getCachedLabel("exportFormatLabel", labels, "Export format")
  selectInput("exportFormat", label, choices = availableOutputFormats)
})

debugLog("UI_OUTPUTS_SELECTORS: Variable selector generation loaded", "APP_STARTUP")
