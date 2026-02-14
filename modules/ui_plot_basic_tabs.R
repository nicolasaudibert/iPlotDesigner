# UI Plot Basic Tabs Module
# Bar, Box, and Violin plot tabs

# Function to create bar chart tab
createBarChartTab <- function(labels, commonLabels) {
  # Cache specific labels
  barChartTabLabel <- safeGetUILabel("barChartTabLabel", labels, "Bar chart")
  barTypeLabel <- safeGetUILabel("barTypeLabel", labels, "Bar height represents")
  barTypeMeanLabel <- safeGetUILabel("barTypeMeanLabel", labels, "Mean")
  barTypeCountLabel <- safeGetUILabel("barTypeCountLabel", labels, "Count")
  errorBarWidthLabel <- safeGetUILabel("errorBarWidthLabel", labels, "Error bar width")
  showDataLabelsLabel <- safeGetUILabel("showDataLabelsLabel", labels, "Show data labels")
  
  tabPanel(
    barChartTabLabel,
    value = "barChart",
    br(),
    wellPanel(
      fluidRow(
        column(3, radioButtons("barType", barTypeLabel,
                              choices = setNames(c("mean", "count"), c(barTypeMeanLabel, barTypeCountLabel)),
                              selected = "mean")),
        column(3, conditionalPanel(
          condition = "input.barType == 'mean'",
          uiOutput("barErrorTypeUI")
        )),
        column(3, conditionalPanel(
          condition = "input.barType == 'mean'",
          sliderInput("barErrorWidth", errorBarWidthLabel, 
                     min = 0, max = 1, value = defaultErrorBarWidth, step = 0.05)
        )),
        column(3, sliderInput("barAlpha", commonLabels$alphaLabel, 
                             min = 0, max = 1, value = defaultAlpha, step = 0.1))
      ),
      fluidRow(
        column(3, checkboxInput("barShowDataLabels", showDataLabelsLabel, FALSE))
      )
    ),
    createPlotConditionalPanels(
      function(height) plotOutput("barPlot", height = height),
      uiOutput("barPlotControls"),
      labels
    )
  )
}

# Function to create box plot tab
createBoxPlotTab <- function(labels, commonLabels) {
  # Cache specific labels
  boxplotTabLabel <- safeGetUILabel("boxplotTabLabel", labels, "Box plot")
  
  tabPanel(
    boxplotTabLabel,
    value = "boxplot",
    br(),
    wellPanel(
      fluidRow(
        column(3, sliderInput("boxAlpha", commonLabels$alphaLabel, 
                             min = 0, max = 1, value = defaultAlpha, step = 0.1)),
        column(3, checkboxInput("boxShowPoints", commonLabels$showPointsLabel, FALSE)),
        column(3, conditionalPanel(
          condition = "input.boxShowPoints",
          sliderInput("boxPointSize", commonLabels$pointSizeLabel, 
                     min = 0.5, max = 5, value = defaultPointSize, step = 0.5)
        )),
        column(3, checkboxInput("boxShowMean", commonLabels$showMeanLabel, FALSE))
      )
    ),
    createPlotConditionalPanels(
      function(height) plotOutput("boxPlot", height = height),
      uiOutput("boxPlotControls"),
      labels
    )
  )
}

# Function to create violin plot tab
createViolinPlotTab <- function(labels, commonLabels) {
  # Cache specific labels
  violinTabLabel <- safeGetUILabel("violinTabLabel", labels, "Violin plot")
  showBoxplotLabel <- safeGetUILabel("showBoxplotLabel", labels, "Show boxplot")
  trimViolinLabel <- safeGetUILabel("trimViolinLabel", labels, "Trim violin tails")
  
  tabPanel(
    violinTabLabel,
    value = "violin",
    br(),
    wellPanel(
      fluidRow(
        column(3, sliderInput("violinAlpha", commonLabels$alphaLabel, 
                             min = 0, max = 1, value = defaultAlpha, step = 0.1)),
        column(3, checkboxInput("violinShowBox", showBoxplotLabel, TRUE)),
        column(3, checkboxInput("violinShowPoints", commonLabels$showPointsLabel, FALSE)),
        column(3, checkboxInput("violinTrim", trimViolinLabel, TRUE))
      )
    ),
    createPlotConditionalPanels(
      function(height) plotOutput("violinPlot", height = height),
      uiOutput("violinPlotControls"),
      labels
    )
  )
}

debugLog("UI_PLOT_BASIC_TABS: Basic plot tabs (bar, box, violin) loaded", "UI_INIT")
