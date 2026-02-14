# Modal dialog handlers module
# Management of error dialogs and filtered modalities dialogs using cached labels

# ============================================================================
# Modal dialog functions
# ============================================================================

# Isolated showFilteredModalitiesDialog with cached labels only
showFilteredModalitiesDialog <- function(varName, validation, labels, session) {
  startTime <- traceObserverTiming("showFilteredModalitiesDialog", "START")
  
  traceUI("FilteredModalitiesDialog", "SHOW", paste("Variable:", varName))
  
  if(!validation$hasFilteredModalities || length(validation$filteredModalities) == 0) {
    traceUI("FilteredModalitiesDialog", "SKIP", "No filtered modalities")
    traceObserverTiming("showFilteredModalitiesDialog", "SKIP", startTime)
    return()
  }
  
  # Use only cached labels to prevent cascades
  modalTitle <- getCachedLabel("modalitiesFilteredDialogTitle", labels, "Modalities Automatically Filtered")
  variableLabel <- getCachedLabel("variable", labels, "Variable")
  filteredLabel <- getCachedLabel("filteredModalitiesLabel", labels, "Filtered modalities")
  reasonLabel <- getCachedLabel("modalitiesExcludedReason", labels, "These modalities have been automatically excluded because they contain fewer than 2 observations")
  okLabel <- getCachedLabel("okLabel", labels, "OK")
  cancelLabel <- getCachedLabel("cancelLabel", labels, "Cancel")
  
  debugLog("MODAL_CACHED: All labels retrieved from cache", "MODAL_CACHE")
  
  filteredCount <- length(validation$filteredModalities)
  modalitiesShown <- min(filteredCount, maxModalitiesInDialog)
  needsDownload <- filteredCount > maxModalitiesInDialog
  
  modalityList <- if(needsDownload) {
    c(validation$filteredModalities[1:maxModalitiesInDialog], "...")
  } else {
    validation$filteredModalities
  }
  
  tryCatch({
    showModal(modalDialog(
      title = div(
        icon("exclamation-triangle", style = "color: #856404; margin-right: 10px;"),
        modalTitle
      ),
      
      div(
        p(strong(variableLabel, ": "), varName),
        p(strong(filteredLabel, ": "), filteredCount),
        
        div(
          style = "max-height: 200px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          tags$ul(
            style = "margin: 0; padding-left: 20px;",
            lapply(modalityList, function(mod) {
              tags$li(
                mod, 
                style = if(mod == "...") "font-style: italic; color: #666;" else ""
              )
            })
          )
        ),
        
        br(),
        p(
          style = "font-size: 0.9em; color: #666;",
          reasonLabel
        )
      ),
      
      easyClose = FALSE,
      footer = tagList(
        actionButton("filteredModalitiesOk", okLabel, class = "btn-primary"),
        actionButton("filteredModalitiesCancel", cancelLabel, class = "btn-default")
      ),
      
      # Add keyboard navigation
      addKeyboardNavigation("filteredModalitiesOk", "filteredModalitiesCancel")
    ))
    
    debugLog("MODAL_CACHED: Filtered dialog created successfully with cached labels", "MODAL_CACHE")
    traceUI("FilteredModalitiesDialog", "SHOWN", "Dialog displayed successfully")
    traceObserverTiming("showFilteredModalitiesDialog", "SUCCESS", startTime)
    
  }, error = function(e) {
    debugLog(paste("CRITICAL ERROR showing filtered dialog:", e$message), "MODAL_INIT")
    debugLog(paste("MODAL ERROR STACK:", toString(sys.calls())), "MODAL_INIT")
    
    # Fallback modal with hardcoded strings
    showModal(modalDialog(
      title = "Filtered Modalities",
      p("Some modalities were filtered (insufficient data)"),
      p("Variable: ", varName),
      p("Count: ", filteredCount),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    traceObserverTiming("showFilteredModalitiesDialog", "ERROR", startTime)
    numericError <- getCachedLabel("numericVariableError", labels, "Numeric variables cannot be used for grouping. Please select a categorical variable.")
    insufficientError <- getCachedLabel("insufficientDataError", labels, "This variable has no categories with sufficient data for analysis.")
    okButtonLabel <- getCachedLabel("okLabel", labels, "OK")
    cancelButtonLabel <- getCachedLabel("cancelLabel", labels, "Cancel")
    
    debugLog("MODAL_CACHED: All error modal labels retrieved from cache", "MODAL_CACHE")
    
    # Determine error detail without additional label lookups
    errorDetail <- if(!is.null(validationResult$reason)) {
      if(validationResult$reason == "Numeric variables cannot be used for grouping") {
        numericError
      } else if(validationResult$reason == "No modalities with at least 2 data points") {
        insufficientError
      } else {
        validationResult$reason
      }
    } else {
      invalidMessage
    }
    
    showModal(modalDialog(
      title = invalidTitle,
      div(
        h4(invalidMessage),
        p(strong("Variable: "), varName),
        p(errorDetail)
      ),
      easyClose = FALSE,
      footer = tagList(
        actionButton("invalidVariableOk", okButtonLabel, class = "btn-primary"),
        actionButton("invalidVariableCancel", cancelButtonLabel, class = "btn-default")
      ),
      
      # Add keyboard navigation
      addKeyboardNavigation("invalidVariableOk", "invalidVariableCancel")
    ))
    
    debugLog("MODAL_CACHED: Invalid variable modal created successfully with cached labels", "MODAL_CACHE")
    
  }, error = function(e) {
    debugLog(paste("ERROR showing modal for var", varNum, ":", e$message), "MODAL_INIT")
    debugLog(paste("MODAL CREATION ERROR STACK:", toString(sys.calls())), "MODAL_INIT")
    
    # Ultimate fallback with hardcoded strings
    showModal(modalDialog(
      title = "Invalid Variable",
      p("Variable: ", varName),
      p("Reason: ", if(!is.null(validationResult$reason)) validationResult$reason else "Unknown error"),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
}

# Observer to handle OK button click
observeEvent(input$invalidVariableOk, {
  removeModal()
})

# Observer to handle Cancel button click
observeEvent(input$invalidVariableCancel, {
  removeModal()
})
