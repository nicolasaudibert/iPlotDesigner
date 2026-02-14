# UI Plot Advanced Tabs Module
# Histogram, Density, and Ridge plot tabs (including dynamic content)

# Function to create histogram tab (dynamic content)
createHistogramTab <- function(labels) {
  histogramTabLabel <- safeGetUILabel("histogramTabLabel", labels, "Histogram")
  
  tabPanel(
    histogramTabLabel,
    value = "histogram",
    br(),
    uiOutput("histogramTabContent")
  )
}

# Function to create density tab (dynamic content)
createDensityTab <- function(labels) {
  densityTabLabel <- safeGetUILabel("densityTabLabel", labels, "Density plot")
  
  tabPanel(
    densityTabLabel,
    value = "density",
    br(),
    uiOutput("densityTabContent")
  )
}

# Function to create ridge plot tab with FIXES APPLIED
createRidgeTab <- function(labels, commonLabels) {
  # Cache specific labels
  ridgeTabLabel <- safeGetUILabel("ridgeTabLabel", labels, "Ridge plot")
  ridgeScaleLabel <- safeGetUILabel("ridgeScaleLabel", labels, "Ridge scale")
  ridgeTypeLabel <- safeGetUILabel("ridgeTypeLabel", labels, "Ridge type")
  showQuantilesLabel <- safeGetUILabel("showQuantilesLabel", labels, "Show quantiles")
  
  # RIDGE FIX 3c: Cache localized ridge type options
  ridgeTypeDensity <- safeGetUILabel("ridgeTypeDensity", labels, "Density")
  ridgeTypeHistogram <- safeGetUILabel("ridgeTypeHistogram", labels, "Histogram")
  
  tabPanel(
    ridgeTabLabel,
    value = "ridge",
    br(),
    wellPanel(
      fluidRow(
        column(3, sliderInput("ridgeScale", ridgeScaleLabel, 
                             min = 0.5, max = 3, value = 1.5, step = 0.1)),
        column(3, sliderInput("ridgeAlpha", commonLabels$alphaLabel, 
                             min = 0, max = 1, value = 0.7, step = 0.1)),
        column(3, selectInput("ridgeType", ridgeTypeLabel,
                             # RIDGE FIX 3c: Localized ridge type options
                             choices = setNames(
                               c("density", "histogram"),
                               c(ridgeTypeDensity, ridgeTypeHistogram)
                             ))),
        # RIDGE FIX 3b: Conditional quantiles only for density type
        column(3, conditionalPanel(
          condition = "input.ridgeType == 'density'",
          checkboxInput("ridgeShowQuantiles", showQuantilesLabel, FALSE)
        ))
      )
    ),
    createPlotConditionalPanels(
      function(height) plotOutput("ridgePlot", height = height),
      uiOutput("ridgePlotControls"),
      labels
    )
  )
}

debugLog("UI_PLOT_ADVANCED_TABS: Advanced plot tabs (histogram, density, ridge) loaded with ridge fixes", "UI_INIT")
