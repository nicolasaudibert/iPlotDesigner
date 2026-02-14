# Code Generation Theme - Backgrounds Module
# Handles legend background, legend key, and strip background

# Generate elements for background customization
# Returns list with comment and elements for inclusion in main theme() call
generateThemeBackgroundsElements <- function(input, labels) {
  
  tryCatch({
    theme_elements <- c()
    
    # Legend background
    legend_bg_checkbox <- if(!is.null(input$themeLegendBackground)) {
      input$themeLegendBackground
    } else {
      TRUE  # Default: match panel
    }
    
    legend_bg_color <- if(!is.null(input$themeLegendBackgroundColor)) {
      input$themeLegendBackgroundColor
    } else {
      "#FFFFFF"
    }
    
    legend_bg_fill_color <- if(!is.null(input$themeLegendBackgroundFillColor)) {
      input$themeLegendBackgroundFillColor
    } else {
      "#FFFFFF"
    }
    
    # Generate legend.background if unchecked and fill color set
    if(!legend_bg_checkbox && legend_bg_fill_color != "#FFFFFF") {
      theme_elements <- c(theme_elements, 
        paste0("legend.background = element_rect(fill = '", legend_bg_fill_color, "', colour = NA)"))
    }
    
    # Only generate legend.box.background if old parameter is set and not default
    if(!legend_bg_checkbox && legend_bg_color != "#FFFFFF") {
      theme_elements <- c(theme_elements, 
        paste0("legend.box.background = element_rect(fill = '", legend_bg_color, "')"))
    }
    
    # Legend key
    legend_key_checkbox <- if(!is.null(input$themeLegendKey)) {
      input$themeLegendKey
    } else {
      TRUE  # Default: match panel
    }
    
    legend_key_color <- if(!is.null(input$themeLegendKeyColor)) {
      input$themeLegendKeyColor
    } else {
      "#FFFFFF"
    }
    
    # Check if legend key is hidden via show/hide checkbox
    show_legend_key <- if(!is.null(input$themeShowLegendKey)) {
      input$themeShowLegendKey
    } else {
      TRUE
    }
    
    # Only generate if NOT hidden AND (checkbox unchecked AND color not default)
    if(show_legend_key && !legend_key_checkbox && legend_key_color != "#FFFFFF") {
      theme_elements <- c(theme_elements, 
        paste0("legend.key = element_rect(fill = '", legend_key_color, "')"))
    }
    
    # Legend key border color (new parameter from Phase 1)
    legend_key_border_color <- if(!is.null(input$themeLegendKeyBorderColor)) {
      input$themeLegendKeyBorderColor
    } else {
      "#B3B3B3"  # theme_light() default
    }
    
    # Only generate if NOT hidden AND different from default
    if(show_legend_key && legend_key_border_color != "#B3B3B3") {
      theme_elements <- c(theme_elements,
        paste0("legend.key = element_rect(color = '", legend_key_border_color, "')"))
    }
    
    # Strip background
    strip_bg_checkbox <- if(!is.null(input$themeStripBackground)) {
      input$themeStripBackground
    } else {
      TRUE  # Default: match panel
    }
    
    strip_bg_color <- if(!is.null(input$themeStripBackgroundColor)) {
      input$themeStripBackgroundColor
    } else {
      "#B3B3B3"  # theme_light() default
    }
    
    # Only generate if checkbox unchecked AND color not default
    if(!strip_bg_checkbox && strip_bg_color != "#B3B3B3") {
      theme_elements <- c(theme_elements, 
        paste0("strip.background = element_rect(fill = '", strip_bg_color, "')"))
    }
    
    # Return list with comment and elements
    if(length(theme_elements) > 0) {
      backgrounds_comment <- safeGetUILabel("codeCommentBackgrounds", labels, "Background colors")
      return(list(
        comment = backgrounds_comment,
        elements = theme_elements
      ))
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    debugLog(paste("ERROR in generateThemeBackgroundsElements:", e$message), "CODE_ERROR")
    return(NULL)
  })
}
