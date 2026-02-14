# Theme Dynamic UI Module
# RenderUI for all theme color pickers (backgrounds, grids, borders, titles, axes)

# Create HTML5 color picker helper function
createColorPickerUI <- function(id, label, default_color) {
  current_value <- if(!is.null(input[[id]]) && input[[id]] != "") {
    input[[id]]
  } else {
    default_color
  }
  
  div(
    style = "margin-bottom: 10px;",
    tags$label(label, `for` = id, style = "display: block; margin-bottom: 5px;"),
    tags$input(
      type = "color",
      id = id,
      value = current_value,
      style = paste0(
        "width: 60px; ",
        "height: 35px; ",
        "padding: 0; ",
        "border: 2px solid #ddd; ",
        "border-radius: 4px; ",
        "cursor: pointer; ",
        "background: none; ",
        "-webkit-appearance: none; ",
        "-moz-appearance: none; ",
        "appearance: none; ",
        "transition: border-color 0.2s ease;"
      ),
      onchange = sprintf("Shiny.setInputValue('%s', this.value);", id)
    )
  )
}

# Background color pickers

output$themeLegendBackgroundColorUI <- renderUI({
  # Direct reactive dependency
  legend_bg <- input$themeLegendBackground
  
  if(!is.null(legend_bg) && !legend_bg) {
    label <- getCachedLabel("themeLegendBackgroundColorLabel", labels, "Legend background color")
    createColorPickerUI("themeLegendBackgroundColor", label, "#FFFFFF")
  }
})

output$themeLegendKeyColorUI <- renderUI({
  legend_key <- input$themeLegendKey
  
  if(!is.null(legend_key) && !legend_key) {
    label <- getCachedLabel("themeLegendKeyColorLabel", labels, "Legend key color")
    createColorPickerUI("themeLegendKeyColor", label, "#FFFFFF")
  }
})

output$themeStripBackgroundColorUI <- renderUI({
  strip_bg <- input$themeStripBackground
  
  if(!is.null(strip_bg) && !strip_bg) {
    label <- getCachedLabel("themeStripBackgroundColorLabel", labels, "Strip background color")
    createColorPickerUI("themeStripBackgroundColor", label, "#B3B3B3")
  }
})

# Grid color pickers

output$themeGridMajorColorUI <- renderUI({
  show_major <- input$themeShowMajorGrid
  
  if(!is.null(show_major) && show_major) {
    label <- getCachedLabel("themeGridMajorColorLabel", labels, "Major grid color")
    createColorPickerUI("themeGridMajorColor", label, "#DEDEDE")
  }
})

output$themeGridMinorColorUI <- renderUI({
  show_minor <- input$themeShowMinorGrid
  
  if(!is.null(show_minor) && show_minor) {
    label <- getCachedLabel("themeGridMinorColorLabel", labels, "Minor grid color")
    createColorPickerUI("themeGridMinorColor", label, "#DEDEDE")
  }
})

# Border color pickers

output$themePanelBorderColorUI <- renderUI({
  show_panel <- input$themeShowPanelBorder
  
  if(!is.null(show_panel) && show_panel) {
    label <- getCachedLabel("themePanelBorderColorLabel", labels, "Panel border color")
    createColorPickerUI("themePanelBorderColor", label, "#B3B3B3")
  }
})

output$themeStripBorderColorUI <- renderUI({
  show_strip <- input$themeShowStripBorder
  
  if(!is.null(show_strip) && show_strip) {
    label <- getCachedLabel("themeStripBorderColorLabel", labels, "Strip border color")
    createColorPickerUI("themeStripBorderColor", label, "#B3B3B3")
  }
})

output$themeLegendKeyBorderColorUI <- renderUI({
  show_legend <- input$themeShowLegendKey
  
  if(!is.null(show_legend) && show_legend) {
    label <- getCachedLabel("themeLegendKeyBorderColorLabel", labels, "Legend key border color")
    createColorPickerUI("themeLegendKeyBorderColor", label, "#B3B3B3")
  }
})

# Title color pickers (always visible)

output$themeAxisTitleXColorUI <- renderUI({
  label <- getCachedLabel("themeAxisTitleXColorLabel", labels, "Color")
  createColorPickerUI("themeAxisTitleXColor", label, "#000000")
})

output$themeAxisTitleYColorUI <- renderUI({
  label <- getCachedLabel("themeAxisTitleYColorLabel", labels, "Color")
  createColorPickerUI("themeAxisTitleYColor", label, "#000000")
})

output$themeStripTextColorUI <- renderUI({
  label <- getCachedLabel("themeStripTextColorLabel", labels, "Color")
  createColorPickerUI("themeStripTextColor", label, "#FFFFFF")
})

output$themeStripTextXColorUI <- renderUI({
  label <- getCachedLabel("themeStripTextXColorLabel", labels, "Color")
  createColorPickerUI("themeStripTextXColor", label, "#FFFFFF")
})

output$themeStripTextYColorUI <- renderUI({
  label <- getCachedLabel("themeStripTextYColorLabel", labels, "Color")
  createColorPickerUI("themeStripTextYColor", label, "#FFFFFF")
})

output$themeLegendTitleColorUI <- renderUI({
  label <- getCachedLabel("themeLegendTitleColorLabel", labels, "Color")
  createColorPickerUI("themeLegendTitleColor", label, "#000000")
})

output$themeLegendTextColorUI <- renderUI({
  label <- getCachedLabel("themeLegendTextColorLabel", labels, "Color")
  createColorPickerUI("themeLegendTextColor", label, "#000000")
})

# Axis text color pickers (always visible)

output$themeXAxisTextColorUI <- renderUI({
  label <- getCachedLabel("themeXAxisTextColorLabel", labels, "Color")
  createColorPickerUI("themeXAxisTextColor", label, "#000000")
})

output$themeYAxisTextColorUI <- renderUI({
  label <- getCachedLabel("themeYAxisTextColorLabel", labels, "Color")
  createColorPickerUI("themeYAxisTextColor", label, "#000000")
})

# Force immediate rendering of all conditional outputs
outputOptions(output, "themeLegendBackgroundColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeLegendKeyColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeStripBackgroundColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeGridMajorColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeGridMinorColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themePanelBorderColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeStripBorderColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeLegendKeyBorderColorUI", suspendWhenHidden = FALSE)
# Force immediate rendering of other color pickers
outputOptions(output, "themeAxisTitleXColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeAxisTitleYColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeStripTextColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeStripTextXColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeStripTextYColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeLegendTitleColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeLegendTextColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeXAxisTextColorUI", suspendWhenHidden = FALSE)
outputOptions(output, "themeYAxisTextColorUI", suspendWhenHidden = FALSE)

debugLog("THEME_RENDERUI: All theme color pickers renderUI loaded", "APP_STARTUP")
