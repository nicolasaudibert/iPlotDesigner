# UI General Settings Module
# General plot settings panel

# Enhanced createGeneralSettingsSection with advanced options
createGeneralSettingsSection <- function(labels) {
  # Cache settings labels
  generalSettingsTitle <- safeGetUILabel("generalSettingsTitle", labels, "General plot settings")
  textSizeLabel <- safeGetUILabel("textSizeLabel", labels, "Text size")
  fontFamilyLabel <- safeGetUILabel("fontFamilyLabel", labels, "Font family")
  includeSystemFontsLabel <- safeGetUILabel("includeSystemFontsLabel", labels, "Include system fonts")
  includeSystemFontsHelp <- safeGetUILabel("includeSystemFontsHelp", labels, 
    "Requires extrafont package. See documentation for setup instructions.")
  transparentBackgroundLabel <- safeGetUILabel("transparentBackgroundLabel", labels, "Transparent background")
  transparentPanelLabel <- safeGetUILabel("transparentPanelLabel", labels, "Transparent panel")
  exportWidthLabel <- safeGetUILabel("exportWidthLabel", labels, "Width")
  exportHeightLabel <- safeGetUILabel("exportHeightLabel", labels, "Height")
  exportUnitsLabel <- safeGetUILabel("exportUnitsLabel", labels, "Units")
  
  # Enhanced validation labels
  enableUserChoiceLabel <- tryCatch({
    getCachedLabel("enableUserChoiceLabel", labels, "Enable advanced user choice dialogs")
  }, error = function(e) {
    "Enable advanced user choice dialogs"
  })
  
  variableValidationSettingsTitle <- tryCatch({
    getCachedLabel("variableValidationSettingsTitle", labels, "Variable Validation Settings")
  }, error = function(e) {
    "Variable Validation Settings"
  })
  
  wellPanel(
    h4(generalSettingsTitle),
    
    # Main plot settings
    fluidRow(
      column(2, sliderInput("textSize", textSizeLabel, 
                            min = 8, max = 24, value = defaultTextSize)),
      column(2, selectInput("defaultFont", fontFamilyLabel,
                            choices = getAvailableFonts(include_system_fonts = FALSE))),
      column(2, selectInput("defaultFontFace", safeGetUILabel("themeFaceLabel", labels, "Style"),
                            choices = setNames(
                              c("plain", "bold", "italic", "bold.italic"),
                              c(safeGetUILabel("themeFacePlain", labels, "Normal"),
                                safeGetUILabel("themeFaceBold", labels, "Bold"),
                                safeGetUILabel("themeFaceItalic", labels, "Italic"),
                                safeGetUILabel("themeFaceBoldItalic", labels, "Bold Italic"))
                            ),
                            selected = "plain")),
      column(2, checkboxInput("includeSystemFonts", includeSystemFontsLabel, FALSE)),
      column(4, helpText(includeSystemFontsHelp))
    ),
    
    # Background settings
    fluidRow(
      column(2, uiOutput("plotBackgroundColorUI")),
      column(2, checkboxInput("plotBackgroundTransparent", transparentBackgroundLabel, FALSE)),
      column(2, uiOutput("panelBackgroundColorUI")),
      column(2, checkboxInput("panelBackgroundTransparent", transparentPanelLabel, FALSE))
    ),
    
    # Export settings
    fluidRow(
      column(3, uiOutput("exportFormatUI")),
      column(3, numericInput("exportWidth", exportWidthLabel, 
                             value = defaultExportedPlotWidth)),
      column(3, numericInput("exportHeight", exportHeightLabel, 
                             value = defaultExportedPlotHeight)),
      column(3, selectInput("exportUnits", exportUnitsLabel,
                            choices = availableUnitsForImageExport))
    ),
    
    # Advanced theme options (collapsed by default)
    createAdvancedThemeOptionsUI(labels),
    
    # Profiling system
    createProfilingUI(labels)
  )
}
