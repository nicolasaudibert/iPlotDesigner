# Code Handlers Copy Module
# Copy to clipboard handlers for all plot types

# Copy code to clipboard handlers with localized labels
observeEvent(input$barCopyCode, {
  req(input$barShowCode)
  createCopyCodeHandler("bar", generateBarPlotCode)(input, values, labels)
})

observeEvent(input$boxCopyCode, {
  req(input$boxShowCode)
  createCopyCodeHandler("box", generateBoxPlotCode)(input, values, labels)
})

observeEvent(input$violinCopyCode, {
  req(input$violinShowCode)
  createCopyCodeHandler("violin", generateViolinPlotCode)(input, values, labels)
})

observeEvent(input$histCopyCode, {
  req(input$histShowCode)
  createCopyCodeHandler("hist", generateHistogramCode)(input, values, labels)
})

observeEvent(input$densityCopyCode, {
  req(input$densityShowCode)
  createCopyCodeHandler("density", generateDensityPlotCode)(input, values, labels)
})

observeEvent(input$ridgeCopyCode, {
  req(input$ridgeShowCode)
  createCopyCodeHandler("ridge", generateRidgePlotCode)(input, values, labels)
})

debugLog("CODE_HANDLERS_COPY: Code copy handlers loaded", "CODE_EXPORT")
