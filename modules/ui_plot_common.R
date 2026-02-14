# UI Plot Common Module
# Common functions and content for plot tabs

# Function to create insufficient data content
createInsufficientDataContent <- function(labels) {
  # Cache insufficient data labels
  insufficientDataTitle <- safeGetUILabel("insufficientDataTitle", labels, "Insufficient Data")
  insufficientDataMessage <- safeGetUILabel("insufficientDataMessage", labels, "The selected combination results in insufficient data for plotting.")
  insufficientDataSuggestion1 <- safeGetUILabel("insufficientDataSuggestion1", labels, "Selecting different variables")
  insufficientDataSuggestion2 <- safeGetUILabel("insufficientDataSuggestion2", labels, "Including more modalities")
  insufficientDataSuggestion3 <- safeGetUILabel("insufficientDataSuggestion3", labels, "Using a different data subset")
  
  div(
    class = "insufficient-data-content",
    h4(icon("exclamation-triangle"), " ", insufficientDataTitle),
    p(insufficientDataMessage),
    p(strong("Please try:")),
    tags$ul(
      style = "text-align: left; display: inline-block;",
      tags$li(insufficientDataSuggestion1),
      tags$li(insufficientDataSuggestion2),
      tags$li(insufficientDataSuggestion3)
    )
  )
}

# Function to create plot conditional panels
createPlotConditionalPanels <- function(plotOutput, plotControls, labels) {
  tagList(
    conditionalPanel(
      condition = "output.plotDataValidation",
      plotOutput("600px")
    ),
    conditionalPanel(
      condition = "!output.plotDataValidation",
      createInsufficientDataContent(labels)
    ),
    br(),
    plotControls
  )
}

# Function to cache common labels used across plot tabs
cacheCommonPlotLabels <- function(labels) {
  list(
    alphaLabel = safeGetUILabel("alphaLabel", labels, "Transparency"),
    showPointsLabel = safeGetUILabel("showPointsLabel", labels, "Show data points"),
    pointSizeLabel = safeGetUILabel("pointSizeLabel", labels, "Point size"),
    showMeanLabel = safeGetUILabel("showMeanLabel", labels, "Show mean")
  )
}

debugLog("UI_PLOT_COMMON: Common plot UI functions loaded", "UI_INIT")
