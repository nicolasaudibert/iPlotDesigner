# UI outputs controls module
# Generate control elements and swap buttons

# Variable swap buttons with direct reactive dependencies
output$swapVar1Var2Button <- renderUI({
  # Direct reactive dependencies
  current_var1 <- input$independentVar1
  current_var2 <- input$independentVar2
  
  # Use cached label to avoid cascades
  noVar2Label <- getCachedLabel("noIndependentVar2Label", labels, "-- No independent variable 2 --")
  
  if(!is.null(current_var1) && current_var1 != "" &&
     !is.null(current_var2) && current_var2 != "" &&
     current_var2 != noVar2Label) {
    
    swapLabel <- getCachedLabel("swapVar1Var2Label", labels, "Swap Var 1 ↔ Var 2")
    actionButton("swapVar1Var2", swapLabel, icon = icon("exchange-alt"))
  }
})

output$swapVar2Var3Button <- renderUI({
  # Direct reactive dependencies
  current_var2 <- input$independentVar2
  current_var3 <- input$independentVar3
  
  # Use cached labels to avoid cascades
  noVar2Label <- getCachedLabel("noIndependentVar2Label", labels, "-- No independent variable 2 --")
  noVar3Label <- getCachedLabel("noIndependentVar3Label", labels, "-- No independent variable 3 --")
  
  if(!is.null(current_var2) && current_var2 != "" &&
     current_var2 != noVar2Label &&
     !is.null(current_var3) && current_var3 != "" &&
     current_var3 != noVar3Label) {
    
    swapLabel <- getCachedLabel("swapVar2Var3Label", labels, "Swap Var 2 ↔ Var 3")
    actionButton("swapVar2Var3", swapLabel, icon = icon("exchange-alt"))
  }
})

output$swapVar1Var3Button <- renderUI({
  # Direct reactive dependencies
  current_var1 <- input$independentVar1
  current_var3 <- input$independentVar3
  
  # Use cached label to avoid cascades
  noVar3Label <- getCachedLabel("noIndependentVar3Label", labels, "-- No independent variable 3 --")
  
  if(!is.null(current_var1) && current_var1 != "" &&
     !is.null(current_var3) && current_var3 != "" &&
     current_var3 != noVar3Label) {
    
    swapLabel <- getCachedLabel("swapVar1Var3Label", labels, "Swap Var 1 ↔ Var 3")
    actionButton("swapVar1Var3", swapLabel, icon = icon("exchange-alt"))
  }
})

# Plot control UIs with cached labels
output$barPlotControls <- renderUI({
  tryCatch({
    createPlotControls("bar", labels)
  }, error = function(e) {
    debugLog(paste("Error in barPlotControls:", e$message), "UI_ERROR")
    # Fallback to simple controls
    div(p("Plot controls temporarily unavailable"))
  })
})

output$boxPlotControls <- renderUI({
  tryCatch({
    createPlotControls("box", labels)
  }, error = function(e) {
    debugLog(paste("Error in boxPlotControls:", e$message), "UI_ERROR")
    div(p("Plot controls temporarily unavailable"))
  })
})

output$violinPlotControls <- renderUI({
  tryCatch({
    createPlotControls("violin", labels)
  }, error = function(e) {
    debugLog(paste("Error in violinPlotControls:", e$message), "UI_ERROR")
    div(p("Plot controls temporarily unavailable"))
  })
})

output$histPlotControls <- renderUI({
  tryCatch({
    createPlotControls("hist", labels)
  }, error = function(e) {
    debugLog(paste("Error in histPlotControls:", e$message), "UI_ERROR")
    div(p("Plot controls temporarily unavailable"))
  })
})

output$densityPlotControls <- renderUI({
  tryCatch({
    createPlotControls("density", labels)
  }, error = function(e) {
    debugLog(paste("Error in densityPlotControls:", e$message), "UI_ERROR")
    div(p("Plot controls temporarily unavailable"))
  })
})

output$ridgePlotControls <- renderUI({
  tryCatch({
    createPlotControls("ridge", labels)
  }, error = function(e) {
    debugLog(paste("Error in ridgePlotControls:", e$message), "UI_ERROR")
    div(p("Plot controls temporarily unavailable"))
  })
})

debugLog("UI_OUTPUTS_CONTROLS: Control element generation loaded", "APP_STARTUP")
