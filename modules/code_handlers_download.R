# Code Handlers Download Module
# Download script handlers for all plot types

# Download code handlers with localized code generation
output$barDownloadCode <- createDownloadHandler("bar", generateBarPlotCode)

output$boxDownloadCode <- createDownloadHandler("boxplot", generateBoxPlotCode)

output$violinDownloadCode <- createDownloadHandler("violin", generateViolinPlotCode)

output$histDownloadCode <- createDownloadHandler("histogram", generateHistogramCode)

output$densityDownloadCode <- createDownloadHandler("density", generateDensityPlotCode)

output$ridgeDownloadCode <- createDownloadHandler("ridge", generateRidgePlotCode)

debugLog("CODE_HANDLERS_DOWNLOAD: Code download handlers loaded", "CODE_EXPORT")
