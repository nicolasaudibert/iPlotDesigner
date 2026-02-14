# Theme Background Elements UI Sub-module
# Handles legend and strip backgrounds

createThemeBackgroundUI <- function(labels) {
  
  # Get labels
  legendPositionLabel <- safeGetUILabel("themeLegendPositionLabel", labels, "Legend position")
  legendPositionRightLabel <- safeGetUILabel("themeLegendPositionRight", labels, "Right")
  legendPositionLeftLabel <- safeGetUILabel("themeLegendPositionLeft", labels, "Left")
  legendPositionTopLabel <- safeGetUILabel("themeLegendPositionTop", labels, "Top")
  legendPositionBottomLabel <- safeGetUILabel("themeLegendPositionBottom", labels, "Bottom")
  legendPositionNoneLabel <- safeGetUILabel("themeLegendPositionNone", labels, "None")
  legendPositionHelpText <- safeGetUILabel("themeLegendPositionHelp", labels, "Position of the legend relative to the plot area.")
  
  legendBackgroundLabel <- safeGetUILabel("themeLegendBackgroundLabel", labels, "Legend background matches panel")
  legendBackgroundColorLabel <- safeGetUILabel("themeLegendBackgroundColorLabel", labels, "Legend background color")
  legendKeyLabel <- safeGetUILabel("themeLegendKeyLabel", labels, "Legend key matches panel")
  stripBackgroundLabel <- safeGetUILabel("themeStripBackgroundLabel", labels, "Strip background matches default")
  
  tagList(
    # Legend position
    fluidRow(
      column(4,
        selectInput("themeLegendPosition", legendPositionLabel,
                   choices = setNames(
                     c("right", "left", "top", "bottom", "none"),
                     c(legendPositionRightLabel, legendPositionLeftLabel, 
                       legendPositionTopLabel, legendPositionBottomLabel, 
                       legendPositionNoneLabel)
                   ),
                   selected = "right")
      ),
      column(8,
        div(
          style = "margin-top: 25px;",
          tags$small(
            icon("info-circle", style = "margin-right: 5px; color: #1976d2;"),
            legendPositionHelpText
          )
        )
      )
    ),
    
    # Legend background
    fluidRow(
      column(4,
        checkboxInput("themeLegendBackground", legendBackgroundLabel, value = TRUE)
      ),
      column(4,
        conditionalPanel(
          condition = "!input.themeLegendBackground",
          uiOutput("themeLegendBackgroundFillColorUI")
        )
      )
    ),
    
    # Legend key
    fluidRow(
      column(4,
        checkboxInput("themeLegendKey", legendKeyLabel, value = TRUE)
      ),
      column(4,
        conditionalPanel(
          condition = "!input.themeLegendKey",
          uiOutput("themeLegendKeyColorUI")
        )
      )
    ),
    
    # Strip background
    fluidRow(
      column(4,
        checkboxInput("themeStripBackground", stripBackgroundLabel, value = TRUE)
      ),
      column(4,
        conditionalPanel(
          condition = "!input.themeStripBackground",
          uiOutput("themeStripBackgroundColorUI")
        )
      )
    )
  )
}

debugLog("UI_THEME_BACKGROUND: Background elements UI sub-module loaded", "APP_STARTUP")
