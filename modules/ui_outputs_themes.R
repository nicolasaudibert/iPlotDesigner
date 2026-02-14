# UI Outputs Themes Module - Extended Version
# Theme and background color management with comprehensive customization options

# Plot background color UI - HTML5 version
output$plotBackgroundColorUI <- renderUI({
  # Direct reactive dependency
  plot_transparent <- input$plotBackgroundTransparent
  
  if(!is.null(plot_transparent) && !plot_transparent) {
    label <- getCachedLabel("plotBackgroundLabel", labels, "Plot background")
    
    # Restore previous color when transparency is disabled
    current_value <- if(!is.null(input$plotBackgroundColor) && input$plotBackgroundColor != "") {
      input$plotBackgroundColor
    } else {
      "#FFFFFF"  # Default fallback
    }
    
    div(
      style = "margin-bottom: 10px;",
      tags$label(label, `for` = "plotBackgroundColor", style = "display: block; margin-bottom: 5px;"),
      tags$input(
        type = "color",
        id = "plotBackgroundColor",
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
        onchange = "Shiny.setInputValue('plotBackgroundColor', this.value);"
      )
    )
  }
})

# Panel background color UI - HTML5 version
output$panelBackgroundColorUI <- renderUI({
  # Direct reactive dependency
  panel_transparent <- input$panelBackgroundTransparent
  
  if(!is.null(panel_transparent) && !panel_transparent) {
    label <- getCachedLabel("panelBackgroundLabel", labels, "Panel background")
    
    # Restore previous color when transparency is disabled
    current_value <- if(!is.null(input$panelBackgroundColor) && input$panelBackgroundColor != "") {
      input$panelBackgroundColor
    } else {
      "#FFFFFF"  # Default fallback
    }
    
    div(
      style = "margin-bottom: 10px;",
      tags$label(label, `for` = "panelBackgroundColor", style = "display: block; margin-bottom: 5px;"),
      tags$input(
        type = "color",
        id = "panelBackgroundColor",
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
        onchange = "Shiny.setInputValue('panelBackgroundColor', this.value);"
      )
    )
  }
})

# Legend background fill color UI - HTML5 version
output$themeLegendBackgroundFillColorUI <- renderUI({
  # Direct reactive dependency
  legend_bg <- input$themeLegendBackground
  
  if(!is.null(legend_bg) && !legend_bg) {
    label <- getCachedLabel("themeLegendBackgroundColorLabel", labels, "Legend background color")
    
    # Restore previous color when checkbox is unchecked
    current_value <- if(!is.null(input$themeLegendBackgroundFillColor) && input$themeLegendBackgroundFillColor != "") {
      input$themeLegendBackgroundFillColor
    } else {
      "#FFFFFF"  # Default fallback
    }
    
    div(
      style = "margin-bottom: 10px;",
      tags$label(label, `for` = "themeLegendBackgroundFillColor", style = "display: block; margin-bottom: 5px;"),
      tags$input(
        type = "color",
        id = "themeLegendBackgroundFillColor",
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
        onchange = "Shiny.setInputValue('themeLegendBackgroundFillColor', this.value);"
      )
    )
  }
})

# Enhanced plot theme reactive with all new options
getPlotTheme <- reactive({
  # Direct reactive dependencies - basic settings
  current_text_size <- input$textSize
  current_font_family <- input$defaultFont
  current_font_face <- input$defaultFontFace
  current_plot_bg <- input$plotBackgroundColor
  current_panel_bg <- input$panelBackgroundColor
  current_plot_transparent <- input$plotBackgroundTransparent
  current_panel_transparent <- input$panelBackgroundTransparent
  
  # Base theme
  current_base_theme <- input$themeBaseTheme
  
  # Background elements
  current_legend_bg <- input$themeLegendBackground
  current_legend_bg_color <- input$themeLegendBackgroundColor
  current_legend_bg_fill_color <- input$themeLegendBackgroundFillColor
  current_legend_key <- input$themeLegendKey
  current_legend_key_color <- input$themeLegendKeyColor
  current_strip_bg <- input$themeStripBackground
  current_strip_bg_color <- input$themeStripBackgroundColor
  current_legend_position <- input$themeLegendPosition
  
  # Titles options
  current_use_global_titles_size <- input$themeUseGlobalTitlesSize
  current_use_global_titles_font <- input$themeUseGlobalTitlesFont
  current_titles_size <- input$themeTitlesSize
  current_titles_font <- input$themeTitlesFont
  current_axis_title_x_size <- input$themeAxisTitleXSize
  current_axis_title_x_font <- input$themeAxisTitleXFont
  current_axis_title_x_face <- input$themeAxisTitleXFace
  current_axis_title_y_size <- input$themeAxisTitleYSize
  current_axis_title_y_font <- input$themeAxisTitleYFont
  current_axis_title_y_face <- input$themeAxisTitleYFace
  
  # Titles colors
  current_axis_title_x_color <- input$themeAxisTitleXColor
  current_axis_title_y_color <- input$themeAxisTitleYColor
  current_strip_text_color <- input$themeStripTextColor
  current_strip_text_x_color <- input$themeStripTextXColor
  current_strip_text_y_color <- input$themeStripTextYColor
  current_legend_title_color <- input$themeLegendTitleColor
  current_legend_text_color <- input$themeLegendTextColor
  
  # Axis text colors
  current_x_axis_color <- input$themeXAxisTextColor
  current_y_axis_color <- input$themeYAxisTextColor
  
  # Strip text options
  current_strip_separate_xy <- input$themeStripSeparateXY
  current_strip_text_size <- input$themeStripTextSize
  current_strip_text_font <- input$themeStripTextFont
  current_strip_text_face <- input$themeStripTextFace
  current_strip_text_x_size <- input$themeStripTextXSize
  current_strip_text_x_font <- input$themeStripTextXFont
  current_strip_text_x_face <- input$themeStripTextXFace
  current_strip_text_y_size <- input$themeStripTextYSize
  current_strip_text_y_font <- input$themeStripTextYFont
  current_strip_text_y_face <- input$themeStripTextYFace
  
  # Legend text options
  current_legend_title_size <- input$themeLegendTitleSize
  current_legend_title_font <- input$themeLegendTitleFont
  current_legend_title_face <- input$themeLegendTitleFace
  current_legend_text_size <- input$themeLegendTextSize
  current_legend_text_font <- input$themeLegendTextFont
  current_legend_text_face <- input$themeLegendTextFace
  
  # Axis text customization options
  current_use_global_axis_size <- input$themeUseGlobalAxisSize
  current_use_global_axis_font <- input$themeUseGlobalAxisFont
  current_x_axis_size <- input$themeXAxisTextSize
  current_y_axis_size <- input$themeYAxisTextSize
  current_x_axis_angle <- input$themeXAxisTextAngle
  current_y_axis_angle <- input$themeYAxisTextAngle
  current_x_axis_font <- input$themeXAxisTextFamily
  current_y_axis_font <- input$themeYAxisTextFamily
  current_x_axis_face <- input$themeXAxisTextFace
  current_y_axis_face <- input$themeYAxisTextFace
  
  # Grid and border options
  current_show_major_grid <- input$themeShowMajorGrid
  current_show_minor_grid <- input$themeShowMinorGrid
  current_show_legend_key <- input$themeShowLegendKey
  current_show_panel_border <- input$themeShowPanelBorder
  current_show_strip_border <- input$themeShowStripBorder
  current_grid_major_color <- input$themeGridMajorColor
  current_grid_major_size <- input$themeGridMajorSize
  current_grid_minor_color <- input$themeGridMinorColor
  current_grid_minor_size <- input$themeGridMinorSize
  current_panel_border_color <- input$themePanelBorderColor
  current_panel_border_size <- input$themePanelBorderSize
  current_strip_border_color <- input$themeStripBorderColor
  current_strip_border_size <- input$themeStripBorderSize
  current_legend_key_border_color <- input$themeLegendKeyBorderColor
  
  if(is.null(current_text_size) || is.null(current_font_family)) {
    return(NULL)
  }
  
  createPlotTheme(
    textSize = current_text_size,
    fontFamily = current_font_family,
    fontFace = ifelse(is.null(current_font_face), "plain", current_font_face),
    plotBg = ifelse(is.null(current_plot_bg), "white", current_plot_bg),
    panelBg = ifelse(is.null(current_panel_bg), "white", current_panel_bg),
    plotTransparent = current_plot_transparent,
    panelTransparent = current_panel_transparent,
    baseTheme = ifelse(is.null(current_base_theme), "light", current_base_theme),
    legendBackground = ifelse(is.null(current_legend_bg), TRUE, current_legend_bg),
    legendBackgroundColor = ifelse(is.null(current_legend_bg_color), "#FFFFFF", current_legend_bg_color),
    legendBackgroundFillColor = ifelse(is.null(current_legend_bg_fill_color), "#FFFFFF", current_legend_bg_fill_color),
    legendKey = ifelse(is.null(current_legend_key), TRUE, current_legend_key),
    legendKeyColor = ifelse(is.null(current_legend_key_color), "#FFFFFF", current_legend_key_color),
    stripBackground = ifelse(is.null(current_strip_bg), TRUE, current_strip_bg),
    stripBackgroundColor = ifelse(is.null(current_strip_bg_color), "#B3B3B3", current_strip_bg_color),
    legendPosition = ifelse(is.null(current_legend_position), "right", current_legend_position),
    useGlobalTitlesSize = ifelse(is.null(current_use_global_titles_size), TRUE, current_use_global_titles_size),
    useGlobalTitlesFont = ifelse(is.null(current_use_global_titles_font), TRUE, current_use_global_titles_font),
    titlesSize = ifelse(is.null(current_titles_size), 11, current_titles_size),
    titlesFont = ifelse(is.null(current_titles_font), "sans", current_titles_font),
    axisTitleXSize = ifelse(is.null(current_axis_title_x_size), 11, current_axis_title_x_size),
    axisTitleXFont = ifelse(is.null(current_axis_title_x_font), "sans", current_axis_title_x_font),
    axisTitleXFace = ifelse(is.null(current_axis_title_x_face), "plain", current_axis_title_x_face),
    axisTitleYSize = ifelse(is.null(current_axis_title_y_size), 11, current_axis_title_y_size),
    axisTitleYFont = ifelse(is.null(current_axis_title_y_font), "sans", current_axis_title_y_font),
    axisTitleYFace = ifelse(is.null(current_axis_title_y_face), "plain", current_axis_title_y_face),
    axisTitleXColor = ifelse(is.null(current_axis_title_x_color), "#000000", current_axis_title_x_color),
    axisTitleYColor = ifelse(is.null(current_axis_title_y_color), "#000000", current_axis_title_y_color),
    stripSeparateXY = ifelse(is.null(current_strip_separate_xy), FALSE, current_strip_separate_xy),
    stripTextSize = ifelse(is.null(current_strip_text_size), 8.8, current_strip_text_size),
    stripTextFont = ifelse(is.null(current_strip_text_font), "sans", current_strip_text_font),
    stripTextFace = ifelse(is.null(current_strip_text_face), "plain", current_strip_text_face),
    stripTextXSize = ifelse(is.null(current_strip_text_x_size), 8.8, current_strip_text_x_size),
    stripTextXFont = ifelse(is.null(current_strip_text_x_font), "sans", current_strip_text_x_font),
    stripTextXFace = ifelse(is.null(current_strip_text_x_face), "plain", current_strip_text_x_face),
    stripTextYSize = ifelse(is.null(current_strip_text_y_size), 8.8, current_strip_text_y_size),
    stripTextYFont = ifelse(is.null(current_strip_text_y_font), "sans", current_strip_text_y_font),
    stripTextYFace = ifelse(is.null(current_strip_text_y_face), "plain", current_strip_text_y_face),
    stripTextColor = ifelse(is.null(current_strip_text_color), "#FFFFFF", current_strip_text_color),
    stripTextXColor = ifelse(is.null(current_strip_text_x_color), "#FFFFFF", current_strip_text_x_color),
    stripTextYColor = ifelse(is.null(current_strip_text_y_color), "#FFFFFF", current_strip_text_y_color),
    legendTitleSize = ifelse(is.null(current_legend_title_size), 11, current_legend_title_size),
    legendTitleFont = ifelse(is.null(current_legend_title_font), "sans", current_legend_title_font),
    legendTitleFace = ifelse(is.null(current_legend_title_face), "plain", current_legend_title_face),
    legendTextSize = ifelse(is.null(current_legend_text_size), 8.8, current_legend_text_size),
    legendTextFont = ifelse(is.null(current_legend_text_font), "sans", current_legend_text_font),
    legendTextFace = ifelse(is.null(current_legend_text_face), "plain", current_legend_text_face),
    legendTitleColor = ifelse(is.null(current_legend_title_color), "#000000", current_legend_title_color),
    legendTextColor = ifelse(is.null(current_legend_text_color), "#000000", current_legend_text_color),
    useGlobalAxisSize = ifelse(is.null(current_use_global_axis_size), TRUE, current_use_global_axis_size),
    useGlobalAxisFont = ifelse(is.null(current_use_global_axis_font), TRUE, current_use_global_axis_font),
    xAxisSize = ifelse(is.null(current_x_axis_size), 8.8, current_x_axis_size),
    yAxisSize = ifelse(is.null(current_y_axis_size), 8.8, current_y_axis_size),
    xAxisAngle = ifelse(is.null(current_x_axis_angle), 0, current_x_axis_angle),
    yAxisAngle = ifelse(is.null(current_y_axis_angle), 0, current_y_axis_angle),
    xAxisFont = ifelse(is.null(current_x_axis_font), "sans", current_x_axis_font),
    yAxisFont = ifelse(is.null(current_y_axis_font), "sans", current_y_axis_font),
    xAxisFace = ifelse(is.null(current_x_axis_face), "plain", current_x_axis_face),
    yAxisFace = ifelse(is.null(current_y_axis_face), "plain", current_y_axis_face),
    xAxisColor = ifelse(is.null(current_x_axis_color), "#000000", current_x_axis_color),
    yAxisColor = ifelse(is.null(current_y_axis_color), "#000000", current_y_axis_color),
    showMajorGrid = ifelse(is.null(current_show_major_grid), TRUE, current_show_major_grid),
    showMinorGrid = ifelse(is.null(current_show_minor_grid), TRUE, current_show_minor_grid),
    showLegendKey = ifelse(is.null(current_show_legend_key), TRUE, current_show_legend_key),
    showPanelBorder = ifelse(is.null(current_show_panel_border), TRUE, current_show_panel_border),
    showStripBorder = ifelse(is.null(current_show_strip_border), FALSE, current_show_strip_border),
    gridMajorColor = ifelse(is.null(current_grid_major_color), "#DEDEDE", current_grid_major_color),
    gridMajorSize = ifelse(is.null(current_grid_major_size), 0.5, current_grid_major_size),
    gridMinorColor = ifelse(is.null(current_grid_minor_color), "#DEDEDE", current_grid_minor_color),
    gridMinorSize = ifelse(is.null(current_grid_minor_size), 0.25, current_grid_minor_size),
    panelBorderColor = ifelse(is.null(current_panel_border_color), "#B3B3B3", current_panel_border_color),
    panelBorderSize = ifelse(is.null(current_panel_border_size), 1, current_panel_border_size),
    stripBorderColor = ifelse(is.null(current_strip_border_color), "#B3B3B3", current_strip_border_color),
    stripBorderSize = ifelse(is.null(current_strip_border_size), 0.5, current_strip_border_size),
    legendKeyBorderColor = ifelse(is.null(current_legend_key_border_color), "#B3B3B3", current_legend_key_border_color)
  )
})

# Enhanced createPlotTheme function with comprehensive customization support
createPlotTheme <- function(textSize, fontFamily, fontFace = "plain", plotBg, panelBg, 
                           plotTransparent = FALSE, panelTransparent = FALSE,
                           baseTheme = "light",
                           legendBackground = TRUE, legendBackgroundColor = "#FFFFFF",
                           legendBackgroundFillColor = "#FFFFFF",
                           legendKey = TRUE, legendKeyColor = "#FFFFFF",
                           stripBackground = TRUE, stripBackgroundColor = "#B3B3B3",
                           legendPosition = "right",
                           useGlobalTitlesSize = TRUE, useGlobalTitlesFont = TRUE,
                           titlesSize = 11, titlesFont = "sans",
                           axisTitleXSize = 11, axisTitleXFont = "sans", axisTitleXFace = "plain",
                           axisTitleYSize = 11, axisTitleYFont = "sans", axisTitleYFace = "plain",
                           axisTitleXColor = "#000000", axisTitleYColor = "#000000",
                           stripSeparateXY = FALSE,
                           stripTextSize = 8.8, stripTextFont = "sans", stripTextFace = "plain",
                           stripTextXSize = 8.8, stripTextXFont = "sans", stripTextXFace = "plain",
                           stripTextYSize = 8.8, stripTextYFont = "sans", stripTextYFace = "plain",
                           stripTextColor = "#FFFFFF", stripTextXColor = "#FFFFFF", stripTextYColor = "#FFFFFF",
                           legendTitleSize = 11, legendTitleFont = "sans", legendTitleFace = "plain",
                           legendTextSize = 8.8, legendTextFont = "sans", legendTextFace = "plain",
                           legendTitleColor = "#000000", legendTextColor = "#000000",
                           useGlobalAxisSize = TRUE, useGlobalAxisFont = TRUE,
                           xAxisSize = 8.8, yAxisSize = 8.8,
                           xAxisAngle = 0, yAxisAngle = 0,
                           xAxisFont = "sans", yAxisFont = "sans",
                           xAxisFace = "plain", yAxisFace = "plain",
                           xAxisColor = "#000000", yAxisColor = "#000000",
                           showMajorGrid = TRUE, showMinorGrid = TRUE, showLegendKey = TRUE,
                           showPanelBorder = TRUE, showStripBorder = FALSE,
                           gridMajorColor = "#DEDEDE", gridMajorSize = 0.5,
                           gridMinorColor = "#DEDEDE", gridMinorSize = 0.25,
                           panelBorderColor = "#B3B3B3", panelBorderSize = 1,
                           stripBorderColor = "#B3B3B3", stripBorderSize = 0.5,
                           legendKeyBorderColor = "#B3B3B3") {
  
  # Handle transparency settings
  finalPlotBg <- if(plotTransparent) "transparent" else plotBg
  finalPanelBg <- if(panelTransparent) "transparent" else panelBg
  
  # Select base theme
  base_theme_obj <- switch(baseTheme,
    "bw" = theme_bw(),
    "minimal" = theme_minimal(),
    "classic" = theme_classic(),
    "gray" = theme_gray(),
    "light" = theme_light(),
    "dark" = theme_dark(),
    "void" = theme_void(),
    theme_light()  # Fallback to theme_light
  )
  
  # Start with base theme and basic customizations
  theme_obj <- base_theme_obj +
    theme(
      text = element_text(size = textSize, face = fontFace, family = fontFamily),
      plot.background = element_rect(fill = finalPlotBg, color = NA),
      panel.background = element_rect(fill = finalPanelBg, color = NA),
      legend.position = legendPosition
    )
  
  # Background customizations
  if(legendBackground) {
    theme_obj <- theme_obj + theme(legend.background = element_rect(fill = finalPanelBg, color = NA))
  } else {
    theme_obj <- theme_obj + theme(legend.background = element_rect(fill = legendBackgroundFillColor, color = NA))
  }
  
  if(legendKey) {
    theme_obj <- theme_obj + theme(legend.key = element_rect(fill = finalPanelBg, color = NA))
  } else {
    theme_obj <- theme_obj + theme(legend.key = element_rect(fill = legendKeyColor, color = NA))
  }
  
  # Strip background: only customize if user explicitly unchecks the option
  if(!stripBackground) {
    theme_obj <- theme_obj + theme(strip.background = element_rect(fill = stripBackgroundColor, color = NA))
  }
  # Otherwise keep base theme default (theme_minimal has gray strip background)
  
  # Titles customizations
  axis_title_x_size <- if(useGlobalTitlesSize) textSize else axisTitleXSize
  axis_title_x_font <- if(useGlobalTitlesFont) fontFamily else axisTitleXFont
  axis_title_x_face <- if(useGlobalTitlesFont) fontFace else axisTitleXFace
  axis_title_y_size <- if(useGlobalTitlesSize) textSize else axisTitleYSize
  axis_title_y_font <- if(useGlobalTitlesFont) fontFamily else axisTitleYFont
  axis_title_y_face <- if(useGlobalTitlesFont) fontFace else axisTitleYFace
  
  theme_obj <- theme_obj + theme(
    axis.title.x = element_text(size = axis_title_x_size, face = axis_title_x_face, 
                                family = axis_title_x_font, color = axisTitleXColor),
    axis.title.y = element_text(size = axis_title_y_size, face = axis_title_y_face, 
                                family = axis_title_y_font, color = axisTitleYColor)
  )
  
  # Strip text customizations
  strip_size_base <- if(useGlobalTitlesSize) textSize * 0.8 else NULL
  
  if(stripSeparateXY) {
    strip_x_size <- if(!is.null(strip_size_base)) strip_size_base else stripTextXSize
    strip_y_size <- if(!is.null(strip_size_base)) strip_size_base else stripTextYSize
    strip_x_font <- if(useGlobalTitlesFont) fontFamily else stripTextXFont
    strip_y_font <- if(useGlobalTitlesFont) fontFamily else stripTextYFont
    strip_x_face <- if(useGlobalTitlesFont) fontFace else stripTextXFace
    strip_y_face <- if(useGlobalTitlesFont) fontFace else stripTextYFace
    
    theme_obj <- theme_obj + theme(
      strip.text.x = element_text(size = strip_x_size, face = strip_x_face, 
                                  family = strip_x_font, color = stripTextXColor),
      strip.text.y = element_text(size = strip_y_size, face = strip_y_face, 
                                  family = strip_y_font, color = stripTextYColor)
    )
  } else {
    strip_size <- if(!is.null(strip_size_base)) strip_size_base else stripTextSize
    strip_font <- if(useGlobalTitlesFont) fontFamily else stripTextFont
    strip_face <- if(useGlobalTitlesFont) fontFace else stripTextFace
    
    theme_obj <- theme_obj + theme(
      strip.text = element_text(size = strip_size, face = strip_face, 
                               family = strip_font, color = stripTextColor)
    )
  }
  
  # Legend text customizations
  legend_title_size <- if(useGlobalTitlesSize) textSize else legendTitleSize
  legend_title_font <- if(useGlobalTitlesFont) fontFamily else legendTitleFont
  legend_title_face <- if(useGlobalTitlesFont) fontFace else legendTitleFace
  legend_text_size <- if(useGlobalTitlesSize) textSize * 0.8 else legendTextSize
  legend_text_font <- if(useGlobalTitlesFont) fontFamily else legendTextFont
  legend_text_face <- if(useGlobalTitlesFont) fontFace else legendTextFace
  
  theme_obj <- theme_obj + theme(
    legend.title = element_text(size = legend_title_size, face = legend_title_face, 
                                family = legend_title_font, color = legendTitleColor),
    legend.text = element_text(size = legend_text_size, face = legend_text_face, 
                               family = legend_text_font, color = legendTextColor)
  )
  
  # Axis text customizations
  x_axis_size <- if(useGlobalAxisSize) textSize else xAxisSize
  x_axis_font <- if(useGlobalAxisFont) fontFamily else xAxisFont
  x_axis_face <- if(useGlobalAxisFont) fontFace else xAxisFace
  y_axis_size <- if(useGlobalAxisSize) textSize else yAxisSize
  y_axis_font <- if(useGlobalAxisFont) fontFamily else yAxisFont
  y_axis_face <- if(useGlobalAxisFont) fontFace else yAxisFace
  
  if(xAxisAngle != 0) {
    theme_obj <- theme_obj + theme(
      axis.text.x = element_text(size = x_axis_size, face = x_axis_face, 
                                family = x_axis_font, color = xAxisColor,
                                 angle = xAxisAngle, hjust = 1, vjust = 1)
    )
  } else {
    theme_obj <- theme_obj + theme(
      axis.text.x = element_text(size = x_axis_size, face = x_axis_face, 
                                family = x_axis_font, color = xAxisColor)
    )
  }
  
  if(yAxisAngle != 0) {
    theme_obj <- theme_obj + theme(
      axis.text.y = element_text(size = y_axis_size, face = y_axis_face, 
                                family = y_axis_font, color = yAxisColor,
                                 angle = yAxisAngle, hjust = 1, vjust = 1)
    )
  } else {
    theme_obj <- theme_obj + theme(
      axis.text.y = element_text(size = y_axis_size, face = y_axis_face, 
                                family = y_axis_font, color = yAxisColor)
    )
  }
  
  # Grid and border customizations with show/hide support
  if(showMajorGrid) {
    theme_obj <- theme_obj + theme(
      panel.grid.major = element_line(color = gridMajorColor, linewidth = gridMajorSize)
    )
  } else {
    theme_obj <- theme_obj + theme(panel.grid.major = element_blank())
  }
  
  if(showMinorGrid) {
    theme_obj <- theme_obj + theme(
      panel.grid.minor = element_line(color = gridMinorColor, linewidth = gridMinorSize)
    )
  } else {
    theme_obj <- theme_obj + theme(panel.grid.minor = element_blank())
  }
  
  # Panel border
  if(showPanelBorder) {
    theme_obj <- theme_obj + theme(
      panel.border = element_rect(color = panelBorderColor, linewidth = panelBorderSize, fill = NA)
    )
  } else {
    theme_obj <- theme_obj + theme(panel.border = element_blank())
  }
  
  # Strip border
  if(showStripBorder) {
    theme_obj <- theme_obj + theme(
      strip.background = element_rect(color = stripBorderColor, linewidth = stripBorderSize)
    )
  }
  # Otherwise keep base theme strip background (without modifying border)
  
  # Legend key border
  if(showLegendKey) {
    theme_obj <- theme_obj + theme(
      legend.key = element_rect(color = legendKeyBorderColor, fill = NA)
    )
  } else {
    theme_obj <- theme_obj + theme(legend.key = element_blank())
  }
  
  return(theme_obj)
}

debugLog("UI_OUTPUTS_THEMES: Extended theme management with comprehensive customization loaded", "APP_STARTUP")
