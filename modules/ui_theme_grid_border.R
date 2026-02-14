# Theme Grids and Borders UI Sub-module
# Handles grid lines and borders display, colors, and sizes

createThemeGridBorderUI <- function(labels) {
  
  # Get labels
  showMajorGridLabel <- safeGetUILabel("themeShowMajorGrid", labels, "Show major grid lines")
  showMinorGridLabel <- safeGetUILabel("themeShowMinorGrid", labels, "Show minor grid lines")
  showLegendKeyLabel <- safeGetUILabel("themeShowLegendKey", labels, "Show legend key background")
  showPanelBorderLabel <- safeGetUILabel("themeShowPanelBorder", labels, "Show panel border")
  showStripBorderLabel <- safeGetUILabel("themeShowStripBorder", labels, "Show strip border")
  
  majorGridSectionLabel <- safeGetUILabel("themeMajorGridSection", labels, "Major Grid Lines")
  minorGridSectionLabel <- safeGetUILabel("themeMinorGridSection", labels, "Minor Grid Lines")
  panelBorderSectionLabel <- safeGetUILabel("themePanelBorderSection", labels, "Panel Border")
  stripBorderSectionLabel <- safeGetUILabel("themeStripBorderSection", labels, "Strip Border (Facets)")
  legendKeySectionLabel <- safeGetUILabel("themeLegendKeySection", labels, "Legend Key Border")
  
  gridMajorColorLabel <- safeGetUILabel("themeGridMajorColorLabel", labels, "Major grid color")
  gridMinorColorLabel <- safeGetUILabel("themeGridMinorColorLabel", labels, "Minor grid color")
  gridMajorSizeLabel <- safeGetUILabel("themeGridMajorSizeLabel", labels, "Major grid line width")
  gridMinorSizeLabel <- safeGetUILabel("themeGridMinorSizeLabel", labels, "Minor grid line width")
  panelBorderColorLabel <- safeGetUILabel("themePanelBorderColorLabel", labels, "Panel border color")
  panelBorderSizeLabel <- safeGetUILabel("themePanelBorderSizeLabel", labels, "Panel border width")
  stripBorderColorLabel <- safeGetUILabel("themeStripBorderColorLabel", labels, "Strip border color")
  stripBorderSizeLabel <- safeGetUILabel("themeStripBorderSizeLabel", labels, "Strip border width")
  legendKeyBorderColorLabel <- safeGetUILabel("themeLegendKeyBorderColorLabel", labels, "Legend key border color")
  
  tagList(
    # Show/hide toggles
    fluidRow(
      column(4,
        checkboxInput("themeShowMajorGrid", showMajorGridLabel, value = TRUE)
      ),
      column(4,
        checkboxInput("themeShowMinorGrid", showMinorGridLabel, value = TRUE)
      ),
      column(4,
        checkboxInput("themeShowPanelBorder", showPanelBorderLabel, value = TRUE)
      )
    ),
    
    fluidRow(
      column(4,
        checkboxInput("themeShowStripBorder", showStripBorderLabel, value = FALSE)
      ),
      column(4,
        checkboxInput("themeShowLegendKey", showLegendKeyLabel, value = TRUE)
      )
    ),
    
    # Major grid controls (conditional)
    conditionalPanel(
      condition = "input.themeShowMajorGrid",
      fluidRow(
        column(12,
          h6(majorGridSectionLabel, 
             style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #666;")
        )
      ),
      fluidRow(
        column(6,
          uiOutput("themeGridMajorColorUI")
        ),
        column(6,
          sliderInput("themeGridMajorSize", gridMajorSizeLabel,
                     min = 0.1, max = 2, value = 0.5, step = 0.1)
        )
      )
    ),
    
    # Minor grid controls (conditional)
    conditionalPanel(
      condition = "input.themeShowMinorGrid",
      fluidRow(
        column(12,
          h6(minorGridSectionLabel, 
             style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #666;")
        )
      ),
      fluidRow(
        column(6,
          uiOutput("themeGridMinorColorUI")
        ),
        column(6,
          sliderInput("themeGridMinorSize", gridMinorSizeLabel,
                     min = 0.1, max = 1, value = 0.25, step = 0.05)
        )
      )
    ),
    
    # Panel border controls (conditional)
    conditionalPanel(
      condition = "input.themeShowPanelBorder",
      fluidRow(
        column(12,
          h6(panelBorderSectionLabel, 
             style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #666;")
        )
      ),
      fluidRow(
        column(6,
          uiOutput("themePanelBorderColorUI")
        ),
        column(6,
          sliderInput("themePanelBorderSize", panelBorderSizeLabel,
                     min = 0.1, max = 2, value = 1, step = 0.1)
        )
      )
    ),
    
    # Strip border controls (conditional)
    conditionalPanel(
      condition = "input.themeShowStripBorder",
      fluidRow(
        column(12,
          h6(stripBorderSectionLabel, 
             style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #666;")
        )
      ),
      fluidRow(
        column(6,
          uiOutput("themeStripBorderColorUI")
        ),
        column(6,
          sliderInput("themeStripBorderSize", stripBorderSizeLabel,
                     min = 0.1, max = 2, value = 0.5, step = 0.1)
        )
      )
    ),
    
    # Legend key border controls (conditional)
    conditionalPanel(
      condition = "input.themeShowLegendKey",
      fluidRow(
        column(12,
          h6(legendKeySectionLabel, 
             style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #666;")
        )
      ),
      fluidRow(
        column(6,
          uiOutput("themeLegendKeyBorderColorUI")
        )
      )
    )
  )
}

debugLog("UI_THEME_GRID_BORDER: Grids and borders UI sub-module loaded", "APP_STARTUP")
