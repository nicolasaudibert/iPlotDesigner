# Theme Observers Module
# Observers for theme-related actions (reset button, etc.)

# Observer for reset to defaults button
observeEvent(input$themeResetDefaults, {
  debugLog("Resetting advanced theme options to theme_light() defaults", "UI_RESET")
  
  # Background options
  updateCheckboxInput(session, "themeLegendBackground", value = TRUE)
  updateCheckboxInput(session, "themeLegendKey", value = TRUE)
  updateCheckboxInput(session, "themeStripBackground", value = TRUE)
  updateSelectInput(session, "themeLegendPosition", selected = "right")
  
  # Titles options
  updateCheckboxInput(session, "themeUseGlobalTitlesSize", value = TRUE)
  updateCheckboxInput(session, "themeUseGlobalTitlesFont", value = TRUE)
  updateSliderInput(session, "themeTitlesSize", value = 11)
  updateSelectInput(session, "themeTitlesFont", selected = "sans")
  
  # Axis titles
  updateSliderInput(session, "themeAxisTitleXSize", value = 11)
  updateSelectInput(session, "themeAxisTitleXFont", selected = "sans")
  updateSliderInput(session, "themeAxisTitleYSize", value = 11)
  updateSelectInput(session, "themeAxisTitleYFont", selected = "sans")
  
  # Strip text
  updateCheckboxInput(session, "themeStripSeparateXY", value = FALSE)
  updateSliderInput(session, "themeStripTextSize", value = 8.8)
  updateSelectInput(session, "themeStripTextFont", selected = "sans")
  updateSliderInput(session, "themeStripTextXSize", value = 8.8)
  updateSelectInput(session, "themeStripTextXFont", selected = "sans")
  updateSliderInput(session, "themeStripTextYSize", value = 8.8)
  updateSelectInput(session, "themeStripTextYFont", selected = "sans")
  
  # Legend title and text
  updateSliderInput(session, "themeLegendTitleSize", value = 11)
  updateSelectInput(session, "themeLegendTitleFont", selected = "sans")
  updateSliderInput(session, "themeLegendTextSize", value = 8.8)
  updateSelectInput(session, "themeLegendTextFont", selected = "sans")
  
  # Axis text options
  updateCheckboxInput(session, "themeUseGlobalAxisSize", value = TRUE)
  updateCheckboxInput(session, "themeUseGlobalAxisFont", value = TRUE)
  
  # X-axis text
  updateSliderInput(session, "themeXAxisTextSize", value = 8.8)
  updateSliderInput(session, "themeXAxisTextAngle", value = 0)
  updateSelectInput(session, "themeXAxisTextFamily", selected = "sans")
  
  # Y-axis text
  updateSliderInput(session, "themeYAxisTextSize", value = 8.8)
  updateSliderInput(session, "themeYAxisTextAngle", value = 0)
  updateSelectInput(session, "themeYAxisTextFamily", selected = "sans")
  
  # Grid and border toggles
  updateCheckboxInput(session, "themeShowMajorGrid", value = TRUE)
  updateCheckboxInput(session, "themeShowMinorGrid", value = TRUE)
  updateCheckboxInput(session, "themeShowLegendKey", value = TRUE)
  updateCheckboxInput(session, "themeShowPanelBorder", value = TRUE)
  updateCheckboxInput(session, "themeShowStripBorder", value = FALSE)
  
  # Grid and border sizes
  updateSliderInput(session, "themeGridMajorSize", value = 0.5)
  updateSliderInput(session, "themeGridMinorSize", value = 0.25)
  updateSliderInput(session, "themePanelBorderSize", value = 1)
  updateSliderInput(session, "themeStripBorderSize", value = 0.5)
  
  # Reset color pickers to defaults
  updateTextInput(session, "themeLegendBackgroundColor", value = "#FFFFFF")
  updateTextInput(session, "themeLegendKeyColor", value = "#FFFFFF")
  updateTextInput(session, "themeStripBackgroundColor", value = "#B3B3B3")
  updateTextInput(session, "themeGridMajorColor", value = "#DEDEDE")
  updateTextInput(session, "themeGridMinorColor", value = "#DEDEDE")
  updateTextInput(session, "themePanelBorderColor", value = "#B3B3B3")
  updateTextInput(session, "themeStripBorderColor", value = "#B3B3B3")
  updateTextInput(session, "themeLegendKeyBorderColor", value = "#B3B3B3")
  
  # Reset title and axis text colors
  updateTextInput(session, "themeAxisTitleXColor", value = "#000000")
  updateTextInput(session, "themeAxisTitleYColor", value = "#000000")
  updateTextInput(session, "themeStripTextColor", value = "#FFFFFF")
  updateTextInput(session, "themeStripTextXColor", value = "#FFFFFF")
  updateTextInput(session, "themeStripTextYColor", value = "#FFFFFF")
  updateTextInput(session, "themeLegendTitleColor", value = "#000000")
  updateTextInput(session, "themeLegendTextColor", value = "#000000")
  updateTextInput(session, "themeXAxisTextColor", value = "#000000")
  updateTextInput(session, "themeYAxisTextColor", value = "#000000")
  
  debugLog("Theme options reset completed", "UI_RESET")
})

debugLog("THEME_OBSERVERS: Theme observers module loaded", "APP_STARTUP")
