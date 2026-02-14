# Theme Titles UI Sub-module
# Handles axis titles, strip text, and legend title/text

createThemeTitlesUI <- function(labels) {
  
  # Get labels
  useGlobalTitlesSizeLabel <- safeGetUILabel("themeUseGlobalTitlesSizeLabel", labels, "Use global text size for titles")
  useGlobalTitlesFontLabel <- safeGetUILabel("themeUseGlobalTitlesFontLabel", labels, "Use global font for titles")
  
  individualTitlesLabel <- safeGetUILabel("themeIndividualTitlesSection", labels, "Individual Title Controls")
  axisTitleXLabel <- safeGetUILabel("themeAxisTitleXLabel", labels, "X-axis title")
  axisTitleYLabel <- safeGetUILabel("themeAxisTitleYLabel", labels, "Y-axis title")
  stripTextLabel <- safeGetUILabel("themeStripTextLabel", labels, "Strip text (facets)")
  stripSeparateXYLabel <- safeGetUILabel("themeStripSeparateXY", labels, "Separate X/Y strips")
  stripTextXLabel <- safeGetUILabel("themeStripTextXLabel", labels, "Strip X")
  stripTextYLabel <- safeGetUILabel("themeStripTextYLabel", labels, "Strip Y")
  legendTitleLabel <- safeGetUILabel("themeLegendTitleLabel", labels, "Legend title")
  legendTextLabel <- safeGetUILabel("themeLegendTextLabel", labels, "Legend text")
  
  # Common control labels
  sizeLabel <- safeGetUILabel("themeSizeLabel", labels, "Size")
  fontLabel <- safeGetUILabel("themeFontLabel", labels, "Font")
  faceLabel <- safeGetUILabel("themeFaceLabel", labels, "Style")
  
  # Font face choices
  faceChoices <- setNames(
    c("plain", "bold", "italic", "bold.italic"),
    c(safeGetUILabel("themeFacePlain", labels, "Normal"),
      safeGetUILabel("themeFaceBold", labels, "Bold"),
      safeGetUILabel("themeFaceItalic", labels, "Italic"),
      safeGetUILabel("themeFaceBoldItalic", labels, "Bold Italic"))
  )
  
  # Font choices (dynamic - will be updated by observer)
  fontChoices <- getAvailableFonts(include_system_fonts = FALSE)
  
  tagList(
    # Global title options
    fluidRow(
      column(4,
        checkboxInput("themeUseGlobalTitlesSize", useGlobalTitlesSizeLabel, value = TRUE)
      ),
      column(4,
        checkboxInput("themeUseGlobalTitlesFont", useGlobalTitlesFontLabel, value = TRUE)
      )
    ),
    
    # Individual title controls
    fluidRow(
      column(12,
        h6(individualTitlesLabel, 
           style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #666;")
      )
    ),
    
    # Axis title X
    fluidRow(
      column(12,
        h6(axisTitleXLabel, style = "font-size: 0.9em; color: #555; margin-bottom: 5px;")
      )
    ),
    
    fluidRow(
      column(3,
        uiOutput("themeAxisTitleXColorUI")
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesSize",
          sliderInput("themeAxisTitleXSize", sizeLabel,
                     min = 8, max = 20, value = 11, step = 1)
        )
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesFont",
          selectInput("themeAxisTitleXFont", fontLabel,
                     choices = fontChoices, selected = "sans")
        )
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesFont",
          selectInput("themeAxisTitleXFace", faceLabel,
                     choices = faceChoices, selected = "plain")
        )
      )
    ),
    
    # Axis title Y
    fluidRow(
      column(12,
        h6(axisTitleYLabel, style = "font-size: 0.9em; color: #555; margin-top: 10px; margin-bottom: 5px;")
      )
    ),
    
    fluidRow(
      column(3,
        uiOutput("themeAxisTitleYColorUI")
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesSize",
          sliderInput("themeAxisTitleYSize", sizeLabel,
                     min = 8, max = 20, value = 11, step = 1)
        )
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesFont",
          selectInput("themeAxisTitleYFont", fontLabel,
                     choices = fontChoices, selected = "sans")
        )
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesFont",
          selectInput("themeAxisTitleYFace", faceLabel,
                     choices = faceChoices, selected = "plain")
        )
      )
    ),
    
    # Strip text options
    fluidRow(
      column(12,
        checkboxInput("themeStripSeparateXY", stripSeparateXYLabel, value = FALSE)
      )
    ),
    
    # Unified strip settings
    conditionalPanel(
      condition = "!input.themeStripSeparateXY",
      fluidRow(
        column(12,
          h6(stripTextLabel, style = "font-size: 0.9em; color: #555; margin-bottom: 5px;")
        )
      ),
      fluidRow(
        column(4,
          uiOutput("themeStripTextColorUI")
        ),
        column(4,
          conditionalPanel(
            condition = "!input.themeUseGlobalTitlesSize",
            sliderInput("themeStripTextSize", sizeLabel,
                       min = 6, max = 16, value = 8.8, step = 0.5)
          )
        ),
        column(4,
          conditionalPanel(
            condition = "!input.themeUseGlobalTitlesFont",
            selectInput("themeStripTextFont", fontLabel,
                       choices = fontChoices, selected = "sans")
          )
        )
      ),
      fluidRow(
        column(4),
        column(4),
        column(4,
          conditionalPanel(
            condition = "!input.themeUseGlobalTitlesFont",
            selectInput("themeStripTextFace", faceLabel,
                       choices = faceChoices, selected = "plain")
          )
        )
      )
    ),
    
    # Separate X/Y strip settings
    conditionalPanel(
      condition = "input.themeStripSeparateXY",
      
      # Strip text X
      fluidRow(
        column(12,
          h6(stripTextXLabel, style = "font-size: 0.9em; color: #555; margin-bottom: 5px;")
        )
      ),
      fluidRow(
        column(3,
          uiOutput("themeStripTextXColorUI")
        ),
        column(3,
          conditionalPanel(
            condition = "!input.themeUseGlobalTitlesSize",
            sliderInput("themeStripTextXSize", sizeLabel,
                       min = 6, max = 16, value = 8.8, step = 0.5)
          )
        ),
        column(3,
          conditionalPanel(
            condition = "!input.themeUseGlobalTitlesFont",
            selectInput("themeStripTextXFont", fontLabel,
                       choices = fontChoices, selected = "sans")
          )
        ),
        column(3,
          conditionalPanel(
            condition = "!input.themeUseGlobalTitlesFont",
            selectInput("themeStripTextXFace", faceLabel,
                       choices = faceChoices, selected = "plain")
          )
        )
      ),
      
      # Strip text Y
      fluidRow(
        column(12,
          h6(stripTextYLabel, style = "font-size: 0.9em; color: #555; margin-top: 10px; margin-bottom: 5px;")
        )
      ),
      fluidRow(
        column(3,
          uiOutput("themeStripTextYColorUI")
        ),
        column(3,
          conditionalPanel(
            condition = "!input.themeUseGlobalTitlesSize",
            sliderInput("themeStripTextYSize", sizeLabel,
                       min = 6, max = 16, value = 8.8, step = 0.5)
          )
        ),
        column(3,
          conditionalPanel(
            condition = "!input.themeUseGlobalTitlesFont",
            selectInput("themeStripTextYFont", fontLabel,
                       choices = fontChoices, selected = "sans")
          )
        ),
        column(3,
          conditionalPanel(
            condition = "!input.themeUseGlobalTitlesFont",
            selectInput("themeStripTextYFace", faceLabel,
                       choices = faceChoices, selected = "plain")
          )
        )
      )
    ),
    
    # Legend title
    fluidRow(
      column(12,
        h6(legendTitleLabel, style = "font-size: 0.9em; color: #555; margin-top: 10px; margin-bottom: 5px;")
      )
    ),
    
    fluidRow(
      column(3,
        uiOutput("themeLegendTitleColorUI")
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesSize",
          sliderInput("themeLegendTitleSize", sizeLabel,
                     min = 8, max = 16, value = 11, step = 0.5)
        )
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesFont",
          selectInput("themeLegendTitleFont", fontLabel,
                     choices = fontChoices, selected = "sans")
        )
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesFont",
          selectInput("themeLegendTitleFace", faceLabel,
                     choices = faceChoices, selected = "plain")
        )
      )
    ),
    
    # Legend text
    fluidRow(
      column(12,
        h6(legendTextLabel, style = "font-size: 0.9em; color: #555; margin-top: 10px; margin-bottom: 5px;")
      )
    ),
    
    fluidRow(
      column(3,
        uiOutput("themeLegendTextColorUI")
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesSize",
          sliderInput("themeLegendTextSize", sizeLabel,
                     min = 6, max = 14, value = 8.8, step = 0.5)
        )
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesFont",
          selectInput("themeLegendTextFont", fontLabel,
                     choices = fontChoices, selected = "sans")
        )
      ),
      column(3,
        conditionalPanel(
          condition = "!input.themeUseGlobalTitlesFont",
          selectInput("themeLegendTextFace", faceLabel,
                     choices = faceChoices, selected = "plain")
        )
      )
    )
  )
}

debugLog("UI_THEME_TITLES: Titles customization UI sub-module loaded", "APP_STARTUP")
