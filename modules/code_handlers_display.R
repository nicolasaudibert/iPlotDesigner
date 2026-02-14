# Code Handlers Display Module
# Show/hide code handlers for all plot types

# Show code handlers using updateAceEditor
observeEvent(input$barShowCode, {
  if(input$barShowCode) {
    code <- generateBarPlotCode(input, values, labels)
    updateAceEditor(session, "barPlotCode", value = code)
  }
})

observeEvent(input$boxShowCode, {
  if(input$boxShowCode) {
    code <- generateBoxPlotCode(input, values, labels)
    updateAceEditor(session, "boxPlotCode", value = code)
  }
})

observeEvent(input$violinShowCode, {
  if(input$violinShowCode) {
    code <- generateViolinPlotCode(input, values, labels)
    updateAceEditor(session, "violinPlotCode", value = code)
  }
})

observeEvent(input$histShowCode, {
  if(input$histShowCode) {
    code <- generateHistogramCode(input, values, labels)
    updateAceEditor(session, "histPlotCode", value = code)
  }
})

observeEvent(input$densityShowCode, {
  if(input$densityShowCode) {
    code <- generateDensityPlotCode(input, values, labels)
    updateAceEditor(session, "densityPlotCode", value = code)
  }
})

observeEvent(input$ridgeShowCode, {
  if(input$ridgeShowCode) {
    code <- generateRidgePlotCode(input, values, labels)
    updateAceEditor(session, "ridgePlotCode", value = code)
  }
})

debugLog("CODE_HANDLERS_DISPLAY: Code display handlers loaded", "CODE_PROCESS")
