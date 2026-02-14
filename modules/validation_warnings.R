# Validation warnings module  
# Handle duplicate variable detection and warning system

# Track last shown configuration to prevent duplicate warnings
lastShownConfig <- ""

# Reactive observer for duplicate variable detection
observeEvent(c(input$independentVar1, input$independentVar2, input$independentVar3), {
  current_ind_var1 <- input$independentVar1
  current_ind_var2 <- input$independentVar2
  current_ind_var3 <- input$independentVar3
  
  if(is.null(current_ind_var1)) {
    return()
  }
  
  currentSignature <- tryCatch({
    getVariableConfigSignature(
      current_ind_var1,
      current_ind_var2,
      current_ind_var3,
      labels
    )
  }, error = function(e) {
    debugLog(paste("Error getting variable signature:", e$message), "VALIDATION_ERROR")
    return("")
  })
  
  if(currentSignature != lastShownConfig && currentSignature != "") {
    lastShownConfig <<- currentSignature
    
    duplicateInfo <- tryCatch({
      checkDuplicateVariables(
        current_ind_var1,
        current_ind_var2,
        current_ind_var3,
        labels
      )
    }, error = function(e) {
      debugLog(paste("Error checking duplicates:", e$message), "VALIDATION_ERROR")
      return(list(shouldShowWarning = FALSE))
    })
    
    if(duplicateInfo$shouldShowWarning) {
      # Use cached labels to avoid cascades
      title <- getCachedLabel("duplicateVariableWarningTitle", labels, "Duplicate Variable Selection")
      message <- getCachedLabel("duplicateVariableWarningMessage", labels, "The same independent variable has been selected {count} times. Please verify this is the intended configuration.")
      okLabel <- getCachedLabel("okLabel", labels, "OK")
      cancelLabel <- getCachedLabel("cancelLabel", labels, "Cancel")
      
      if(duplicateInfo$allIdentical) {
        disabledMessage <- getCachedLabel("duplicateVariableDisabledMessage", labels, "Histogram and density plots are disabled when all independent variables are identical.")
        
        showModal(modalDialog(
          title = title,
          div(
            p(gsub("\\{count\\}", duplicateInfo$duplicateCount, message)),
            br(),
            p(style = "color: orange; font-weight: bold;", disabledMessage)
          ),
          easyClose = FALSE,
          footer = tagList(
            tags$button(
              okLabel,
              id = "duplicateWarningOk",
              class = "btn btn-primary",
              onclick = "$('#shiny-modal').modal('hide');"
            ),
            tags$button(
              cancelLabel,
              id = "duplicateWarningCancel",
              class = "btn btn-default",
              onclick = "$('#shiny-modal').modal('hide');"
            )
          ),
          
          # Add keyboard navigation
          addKeyboardNavigation("duplicateWarningOk", "duplicateWarningCancel")
        ))
      } else {
        showModal(modalDialog(
          title = title,
          p(gsub("\\{count\\}", duplicateInfo$duplicateCount, message)),
          easyClose = FALSE,
          footer = tagList(
            tags$button(
              okLabel,
              id = "duplicateWarningOk2",
              class = "btn btn-primary",
              onclick = "$('#shiny-modal').modal('hide');"
            ),
            tags$button(
              cancelLabel,
              id = "duplicateWarningCancel2",
              class = "btn btn-default",
              onclick = "$('#shiny-modal').modal('hide');"
            )
          ),
          
          # Add keyboard navigation
          addKeyboardNavigation("duplicateWarningOk2", "duplicateWarningCancel2")
        ))
      }
    }
  }
}, ignoreInit = TRUE)

# Dynamic UI warning for duplicate variables
output$duplicateVariableWarning <- renderUI({
  current_ind_var1 <- input$independentVar1
  current_ind_var2 <- input$independentVar2
  current_ind_var3 <- input$independentVar3
  
  if(is.null(current_ind_var1)) {
    return(NULL)
  }
  
  duplicateInfo <- tryCatch({
    checkDuplicateVariables(
      current_ind_var1,
      current_ind_var2,
      current_ind_var3,
      labels
    )
  }, error = function(e) {
    debugLog(paste("Error in duplicateVariableWarning:", e$message), "VALIDATION_ERROR")
    return(list(allIdentical = FALSE))
  })
  
  if(duplicateInfo$allIdentical) {
    # Use localized labels like in the modal
    noteLabel <- getCachedLabel("noteLabel", labels, "Note:")
    disabledMessage <- getCachedLabel("duplicateVariableDisabledMessage", labels, 
                                     "Histogram and density plots are disabled when all independent variables are identical.")
    
    div(
      style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; margin: 10px 0; border-radius: 5px;",
      icon("exclamation-triangle", style = "color: #856404; margin-right: 5px;"),
      strong(noteLabel, " "),
      disabledMessage
    )
  }
})

debugLog("VALIDATION_WARNINGS: Duplicate variable warning system loaded", "VALIDATION_INIT")
