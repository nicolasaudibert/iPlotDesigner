# Theme Axis Text UI Sub-module
# Handles axis text color, size, angle, and font

createThemeAxisTextUI <- function(labels) {
  
  # Get labels
  useGlobalAxisSizeLabel <- safeGetUILabel("themeUseGlobalAxisSizeLabel", labels, "Use global text size for axes")
  useGlobalAxisFontLabel <- safeGetUILabel("themeUseGlobalAxisFontLabel", labels, "Use global font for axes")
  
  xAxisSectionLabel <- safeGetUILabel("themeXAxisSection", labels, "X-Axis")
  yAxisSectionLabel <- safeGetUILabel("themeYAxisSection", labels, "Y-Axis")
  
  xAxisTextAngleLabel <- safeGetUILabel("themeXAxisTextAngleLabel", labels, "Angle")
  yAxisTextAngleLabel <- safeGetUILabel("themeYAxisTextAngleLabel", labels, "Angle")
  
  sizeLabel <- safeGetUILabel("themeSizeLabel", labels, "Size")
  fontLabel <- safeGetUILabel("themeFontLabel", labels, "Font")
  faceLabel <- safeGetUILabel("themeFaceLabel", labels, "Style")
  
  # Font choices (dynamic - will be updated by observer)
  fontChoices <- getAvailableFonts(include_system_fonts = FALSE)
  
  # Font face choices
  faceChoices <- setNames(
    c("plain", "bold", "italic", "bold.italic"),
    c(safeGetUILabel("themeFacePlain", labels, "Normal"),
      safeGetUILabel("themeFaceBold", labels, "Bold"),
      safeGetUILabel("themeFaceItalic", labels, "Italic"),
      safeGetUILabel("themeFaceBoldItalic", labels, "Bold Italic"))
  )
  
  tagList(
    # Global axis options
    fluidRow(
      column(4,
        checkboxInput("themeUseGlobalAxisSize", useGlobalAxisSizeLabel, value = TRUE)
      ),
      column(4,
        checkboxInput("themeUseGlobalAxisFont", useGlobalAxisFontLabel, value = TRUE)
      )
    ),
    
    # X-axis specific options (color, angle always visible - size/font conditional)
    fluidRow(
      column(12,
        h6(xAxisSectionLabel, 
           style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #666;")
      )
    ),
    
    fluidRow(
      column(3,
        uiOutput("themeXAxisTextColorUI")
      ),
      column(3,
        sliderInput("themeXAxisTextAngle", xAxisTextAngleLabel,
                   min = 0, max = 90, value = 0, step = 15)
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalAxisSize",
          sliderInput("themeXAxisTextSize", sizeLabel,
                     min = 6, max = 16, value = 8.8, step = 0.5)
        )
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalAxisFont",
          selectInput("themeXAxisTextFamily", fontLabel,
                     choices = fontChoices,
                     selected = "sans")
        )
      )
    ),
    
    fluidRow(
      column(3),
      column(3),
      column(3),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalAxisFont",
          selectInput("themeXAxisTextFace", faceLabel,
                     choices = faceChoices,
                     selected = "plain")
        )
      )
    ),
    
    # Y-axis specific options (color, angle always visible - size/font conditional)
    fluidRow(
      column(12,
        h6(yAxisSectionLabel, 
           style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #666;")
      )
    ),
    
    fluidRow(
      column(3,
        uiOutput("themeYAxisTextColorUI")
      ),
      column(3,
        sliderInput("themeYAxisTextAngle", yAxisTextAngleLabel,
                   min = 0, max = 90, value = 0, step = 15)
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalAxisSize",
          sliderInput("themeYAxisTextSize", sizeLabel,
                     min = 6, max = 16, value = 8.8, step = 0.5)
        )
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalAxisFont",
          selectInput("themeYAxisTextFamily", fontLabel,
                     choices = fontChoices,
                     selected = "sans")
        )
      )
    ),
    
    fluidRow(
      column(3),
      column(3),
      column(3),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalAxisFont",
          selectInput("themeYAxisTextFace", faceLabel,
                     choices = faceChoices,
                     selected = "plain")
        )
      )
    )
  )
}

debugLog("UI_THEME_AXIS_TEXT: Axis text customization UI sub-module loaded", "APP_STARTUP")
