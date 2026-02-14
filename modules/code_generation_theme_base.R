# Code Generation Theme - Base Module
# Handles base theme, global text settings, backgrounds, and legend position

# Generate elements for base theme and general parameters
# Returns list with comment and elements for inclusion in main theme() call
generateThemeBaseElements <- function(input, labels) {
  
  tryCatch({
    theme_elements <- c()
    
    # Global text settings (always included)
    text_size <- if(!is.null(input$textSize)) input$textSize else 11
    font_family <- if(!is.null(input$defaultFont)) input$defaultFont else "sans"
    font_face <- if(!is.null(input$defaultFontFace)) input$defaultFontFace else "plain"
    
    # Build text element with face if not default
    text_params <- c(
      paste0("size = ", text_size),
      paste0("family = '", font_family, "'")
    )
    if(font_face != "plain") {
      text_params <- c(text_params, paste0("face = '", font_face, "'"))
    }
    
    theme_elements <- c(theme_elements, 
      paste0("text = element_text(", paste(text_params, collapse = ", "), ")"))
    
    # Plot background
    plot_transparent <- !is.null(input$plotBackgroundTransparent) && input$plotBackgroundTransparent
    plot_bg_color <- if(!is.null(input$plotBackgroundColor)) input$plotBackgroundColor else "#FFFFFF"
    
    if(plot_transparent) {
      theme_elements <- c(theme_elements, "plot.background = element_blank()")
    } else if(plot_bg_color != "#FFFFFF" && plot_bg_color != "") {
      theme_elements <- c(theme_elements, 
        paste0("plot.background = element_rect(fill = '", plot_bg_color, "')"))
    }
    
    # Panel background
    panel_transparent <- !is.null(input$panelBackgroundTransparent) && input$panelBackgroundTransparent
    panel_bg_color <- if(!is.null(input$panelBackgroundColor)) input$panelBackgroundColor else "#FFFFFF"
    
    if(panel_transparent) {
      theme_elements <- c(theme_elements, "panel.background = element_blank()")
    } else if(panel_bg_color != "#FFFFFF" && panel_bg_color != "") {
      theme_elements <- c(theme_elements, 
        paste0("panel.background = element_rect(fill = '", panel_bg_color, "')"))
    }
    
    # Legend position (only if different from default "right")
    current_position <- if(!is.null(input$themeLegendPosition)) {
      input$themeLegendPosition
    } else {
      "right"
    }
    
    if(current_position != "right") {
      theme_elements <- c(theme_elements, 
        paste0("legend.position = '", current_position, "'"))
    }
    
    # Return list with comment and elements
    if(length(theme_elements) > 0) {
      general_comment <- safeGetUILabel("codeCommentGeneralTheme", labels, "General theme parameters")
      return(list(
        comment = general_comment,
        elements = theme_elements
      ))
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    debugLog(paste("ERROR in generateThemeBaseElements:", e$message), "CODE_ERROR")
    return(NULL)
  })
}
