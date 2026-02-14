# Code Generation Theme - Titles Module
# Handles axis titles, strip text, and legend title/text (18 parameters)

# Generate elements for title customization with colors
# Returns list with comment and elements for inclusion in main theme() call
generateThemeTitlesElements <- function(input, labels) {
  
  tryCatch({
    theme_elements <- c()
    
    # Get global settings
    use_global_size <- if(!is.null(input$themeUseGlobalTitlesSize)) {
      input$themeUseGlobalTitlesSize
    } else {
      TRUE
    }
    
    use_global_font <- if(!is.null(input$themeUseGlobalTitlesFont)) {
      input$themeUseGlobalTitlesFont
    } else {
      TRUE
    }
    
    global_size <- if(!is.null(input$textSize)) input$textSize else 11
    global_font <- if(!is.null(input$defaultFont)) input$defaultFont else "sans"
    global_face <- if(!is.null(input$defaultFontFace)) input$defaultFontFace else "plain"
    
    # Axis title X
    axis_x_size <- if(use_global_size) {
      global_size
    } else {
      if(!is.null(input$themeAxisTitleXSize)) input$themeAxisTitleXSize else 11
    }
    
    axis_x_font <- if(use_global_font) {
      global_font
    } else {
      if(!is.null(input$themeAxisTitleXFont)) input$themeAxisTitleXFont else "sans"
    }
    
    axis_x_face <- if(use_global_font) {
      global_face
    } else {
      if(!is.null(input$themeAxisTitleXFace)) input$themeAxisTitleXFace else "plain"
    }
    
    axis_x_color <- if(!is.null(input$themeAxisTitleXColor)) {
      input$themeAxisTitleXColor
    } else {
      "#000000"
    }
    
    # Build element_text() only if different from defaults
    axis_x_params <- c()
    if(!use_global_size && axis_x_size != 11) {
      axis_x_params <- c(axis_x_params, paste0("size = ", axis_x_size))
    }
    if(!use_global_font && axis_x_face != "plain") {
      axis_x_params <- c(axis_x_params, paste0("face = '", axis_x_face, "'"))
    }
    if(!use_global_font && axis_x_font != "sans") {
      axis_x_params <- c(axis_x_params, paste0("family = '", axis_x_font, "'"))
    }
    if(axis_x_color != "#000000") {
      axis_x_params <- c(axis_x_params, paste0("color = '", axis_x_color, "'"))
    }
    
    if(length(axis_x_params) > 0) {
      theme_elements <- c(theme_elements,
        paste0("axis.title.x = element_text(", paste(axis_x_params, collapse = ", "), ")"))
    }
    
    # Axis title Y
    axis_y_size <- if(use_global_size) {
      global_size
    } else {
      if(!is.null(input$themeAxisTitleYSize)) input$themeAxisTitleYSize else 11
    }
    
    axis_y_font <- if(use_global_font) {
      global_font
    } else {
      if(!is.null(input$themeAxisTitleYFont)) input$themeAxisTitleYFont else "sans"
    }
    
    axis_y_face <- if(use_global_font) {
      global_face
    } else {
      if(!is.null(input$themeAxisTitleYFace)) input$themeAxisTitleYFace else "plain"
    }
    
    axis_y_color <- if(!is.null(input$themeAxisTitleYColor)) {
      input$themeAxisTitleYColor
    } else {
      "#000000"
    }
    
    axis_y_params <- c()
    if(!use_global_size && axis_y_size != 11) {
      axis_y_params <- c(axis_y_params, paste0("size = ", axis_y_size))
    }
    if(!use_global_font && axis_y_face != "plain") {
      axis_y_params <- c(axis_y_params, paste0("face = '", axis_y_face, "'"))
    }
    if(!use_global_font && axis_y_font != "sans") {
      axis_y_params <- c(axis_y_params, paste0("family = '", axis_y_font, "'"))
    }
    if(axis_y_color != "#000000") {
      axis_y_params <- c(axis_y_params, paste0("color = '", axis_y_color, "'"))
    }
    
    if(length(axis_y_params) > 0) {
      theme_elements <- c(theme_elements,
        paste0("axis.title.y = element_text(", paste(axis_y_params, collapse = ", "), ")"))
    }
    
    # Strip text - check if separate X/Y
    strip_separate_xy <- if(!is.null(input$themeStripSeparateXY)) {
      input$themeStripSeparateXY
    } else {
      FALSE
    }
    
    if(strip_separate_xy) {
      # Separate X/Y strip text
      
      # Strip text X
      strip_x_size <- if(use_global_size) {
        global_size
      } else {
        if(!is.null(input$themeStripTextXSize)) input$themeStripTextXSize else 8.8
      }
      
      strip_x_font <- if(use_global_font) {
        global_font
      } else {
        if(!is.null(input$themeStripTextXFont)) input$themeStripTextXFont else "sans"
      }
      
      strip_x_face <- if(use_global_font) {
        global_face
      } else {
        if(!is.null(input$themeStripTextXFace)) input$themeStripTextXFace else "plain"
      }
      
      strip_x_color <- if(!is.null(input$themeStripTextXColor)) {
        input$themeStripTextXColor
      } else {
        "#FFFFFF"
      }
      
      strip_x_params <- c()
      if(!use_global_size && strip_x_size != 8.8) {
        strip_x_params <- c(strip_x_params, paste0("size = ", strip_x_size))
      }
      if(!use_global_font && strip_x_face != "plain") {
        strip_x_params <- c(strip_x_params, paste0("face = '", strip_x_face, "'"))
      }
      if(!use_global_font && strip_x_font != "sans") {
        strip_x_params <- c(strip_x_params, paste0("family = '", strip_x_font, "'"))
      }
      if(strip_x_color != "#FFFFFF") {
        strip_x_params <- c(strip_x_params, paste0("color = '", strip_x_color, "'"))
      }
      
      if(length(strip_x_params) > 0) {
        theme_elements <- c(theme_elements,
          paste0("strip.text.x = element_text(", paste(strip_x_params, collapse = ", "), ")"))
      }
      
      # Strip text Y
      strip_y_size <- if(use_global_size) {
        global_size
      } else {
        if(!is.null(input$themeStripTextYSize)) input$themeStripTextYSize else 8.8
      }
      
      strip_y_font <- if(use_global_font) {
        global_font
      } else {
        if(!is.null(input$themeStripTextYFont)) input$themeStripTextYFont else "sans"
      }
      
      strip_y_face <- if(use_global_font) {
        global_face
      } else {
        if(!is.null(input$themeStripTextYFace)) input$themeStripTextYFace else "plain"
      }
      
      strip_y_color <- if(!is.null(input$themeStripTextYColor)) {
        input$themeStripTextYColor
      } else {
        "#FFFFFF"
      }
      
      strip_y_params <- c()
      if(!use_global_size && strip_y_size != 8.8) {
        strip_y_params <- c(strip_y_params, paste0("size = ", strip_y_size))
      }
      if(!use_global_font && strip_y_face != "plain") {
        strip_y_params <- c(strip_y_params, paste0("face = '", strip_y_face, "'"))
      }
      if(!use_global_font && strip_y_font != "sans") {
        strip_y_params <- c(strip_y_params, paste0("family = '", strip_y_font, "'"))
      }
      if(strip_y_color != "#FFFFFF") {
        strip_y_params <- c(strip_y_params, paste0("color = '", strip_y_color, "'"))
      }
      
      if(length(strip_y_params) > 0) {
        theme_elements <- c(theme_elements,
          paste0("strip.text.y = element_text(", paste(strip_y_params, collapse = ", "), ")"))
      }
      
    } else {
      # Unified strip text
      strip_size <- if(use_global_size) {
        global_size
      } else {
        if(!is.null(input$themeStripTextSize)) input$themeStripTextSize else 8.8
      }
      
      strip_font <- if(use_global_font) {
        global_font
      } else {
        if(!is.null(input$themeStripTextFont)) input$themeStripTextFont else "sans"
      }
      
      strip_face <- if(use_global_font) {
        global_face
      } else {
        if(!is.null(input$themeStripTextFace)) input$themeStripTextFace else "plain"
      }
      
      strip_color <- if(!is.null(input$themeStripTextColor)) {
        input$themeStripTextColor
      } else {
        "#FFFFFF"
      }
      
      strip_params <- c()
      if(!use_global_size && strip_size != 8.8) {
        strip_params <- c(strip_params, paste0("size = ", strip_size))
      }
      if(!use_global_font && strip_face != "plain") {
        strip_params <- c(strip_params, paste0("face = '", strip_face, "'"))
      }
      if(!use_global_font && strip_font != "sans") {
        strip_params <- c(strip_params, paste0("family = '", strip_font, "'"))
      }
      if(strip_color != "#FFFFFF") {
        strip_params <- c(strip_params, paste0("color = '", strip_color, "'"))
      }
      
      if(length(strip_params) > 0) {
        theme_elements <- c(theme_elements,
          paste0("strip.text = element_text(", paste(strip_params, collapse = ", "), ")"))
      }
    }
    
    # Legend title
    legend_title_size <- if(use_global_size) {
      global_size
    } else {
      if(!is.null(input$themeLegendTitleSize)) input$themeLegendTitleSize else 11
    }
    
    legend_title_font <- if(use_global_font) {
      global_font
    } else {
      if(!is.null(input$themeLegendTitleFont)) input$themeLegendTitleFont else "sans"
    }
    
    legend_title_face <- if(use_global_font) {
      global_face
    } else {
      if(!is.null(input$themeLegendTitleFace)) input$themeLegendTitleFace else "plain"
    }
    
    legend_title_color <- if(!is.null(input$themeLegendTitleColor)) {
      input$themeLegendTitleColor
    } else {
      "#000000"
    }
    
    legend_title_params <- c()
    if(!use_global_size && legend_title_size != 11) {
      legend_title_params <- c(legend_title_params, paste0("size = ", legend_title_size))
    }
    if(!use_global_font && legend_title_face != "plain") {
      legend_title_params <- c(legend_title_params, paste0("face = '", legend_title_face, "'"))
    }
    if(!use_global_font && legend_title_font != "sans") {
      legend_title_params <- c(legend_title_params, paste0("family = '", legend_title_font, "'"))
    }
    if(legend_title_color != "#000000") {
      legend_title_params <- c(legend_title_params, paste0("color = '", legend_title_color, "'"))
    }
    
    if(length(legend_title_params) > 0) {
      theme_elements <- c(theme_elements,
        paste0("legend.title = element_text(", paste(legend_title_params, collapse = ", "), ")"))
    }
    
    # Legend text
    legend_text_size <- if(use_global_size) {
      global_size
    } else {
      if(!is.null(input$themeLegendTextSize)) input$themeLegendTextSize else 8.8
    }
    
    legend_text_font <- if(use_global_font) {
      global_font
    } else {
      if(!is.null(input$themeLegendTextFont)) input$themeLegendTextFont else "sans"
    }
    
    legend_text_face <- if(use_global_font) {
      global_face
    } else {
      if(!is.null(input$themeLegendTextFace)) input$themeLegendTextFace else "plain"
    }
    
    legend_text_color <- if(!is.null(input$themeLegendTextColor)) {
      input$themeLegendTextColor
    } else {
      "#000000"
    }
    
    legend_text_params <- c()
    if(!use_global_size && legend_text_size != 8.8) {
      legend_text_params <- c(legend_text_params, paste0("size = ", legend_text_size))
    }
    if(!use_global_font && legend_text_face != "plain") {
      legend_text_params <- c(legend_text_params, paste0("face = '", legend_text_face, "'"))
    }
    if(!use_global_font && legend_text_font != "sans") {
      legend_text_params <- c(legend_text_params, paste0("family = '", legend_text_font, "'"))
    }
    if(legend_text_color != "#000000") {
      legend_text_params <- c(legend_text_params, paste0("color = '", legend_text_color, "'"))
    }
    
    if(length(legend_text_params) > 0) {
      theme_elements <- c(theme_elements,
        paste0("legend.text = element_text(", paste(legend_text_params, collapse = ", "), ")"))
    }
    
    # Return list with comment and elements
    if(length(theme_elements) > 0) {
      titles_comment <- safeGetUILabel("codeCommentTitles", labels, "Title customization")
      return(list(
        comment = titles_comment,
        elements = theme_elements
      ))
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    debugLog(paste("ERROR in generateThemeTitlesElements:", e$message), "CODE_ERROR")
    return(NULL)
  })
}
