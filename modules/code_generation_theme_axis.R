# Code Generation Theme - Axis Text Module
# Handles axis text appearance (size, angle, font, color)

# Generate elements for axis text customization
# Returns list with comment and elements for inclusion in main theme() call
generateThemeAxisElements <- function(input, labels) {
  
  tryCatch({
    theme_elements <- c()
    
    # Get global settings
    use_global_size <- if(!is.null(input$themeUseGlobalAxisSize)) {
      input$themeUseGlobalAxisSize
    } else {
      TRUE
    }
    
    use_global_font <- if(!is.null(input$themeUseGlobalAxisFont)) {
      input$themeUseGlobalAxisFont
    } else {
      TRUE
    }
    
    global_size <- if(!is.null(input$textSize)) input$textSize else 11
    global_font <- if(!is.null(input$defaultFont)) input$defaultFont else "sans"
    global_face <- if(!is.null(input$defaultFontFace)) input$defaultFontFace else "plain"
    
    # X-axis text
    x_size <- if(use_global_size) {
      global_size
    } else {
      if(!is.null(input$themeXAxisTextSize)) input$themeXAxisTextSize else 8.8
    }
    
    x_font <- if(use_global_font) {
      global_font
    } else {
      if(!is.null(input$themeXAxisTextFamily)) input$themeXAxisTextFamily else "sans"
    }
    
    x_face <- if(use_global_font) {
      global_face
    } else {
      if(!is.null(input$themeXAxisTextFace)) input$themeXAxisTextFace else "plain"
    }
    
    x_angle <- if(!is.null(input$themeXAxisTextAngle)) {
      input$themeXAxisTextAngle
    } else {
      0
    }
    
    x_color <- if(!is.null(input$themeXAxisTextColor)) {
      input$themeXAxisTextColor
    } else {
      "#000000"
    }
    
    # Build element_text() only if different from defaults or has angle/color
    x_params <- c()
    
    if(!use_global_size && x_size != 8.8) {
      x_params <- c(x_params, paste0("size = ", x_size))
    }
    if(!use_global_font && x_face != "plain") {
      x_params <- c(x_params, paste0("face = '", x_face, "'"))
    }
    if(!use_global_font && x_font != "sans") {
      x_params <- c(x_params, paste0("family = '", x_font, "'"))
    }
    if(x_color != "#000000") {
      x_params <- c(x_params, paste0("color = '", x_color, "'"))
    }
    if(x_angle != 0) {
      x_params <- c(x_params, paste0("angle = ", x_angle))
      # Add hjust/vjust for angled text
      x_params <- c(x_params, "hjust = 1", "vjust = 1")
    }
    
    if(length(x_params) > 0) {
      theme_elements <- c(theme_elements,
        paste0("axis.text.x = element_text(", paste(x_params, collapse = ", "), ")"))
    }
    
    # Y-axis text
    y_size <- if(use_global_size) {
      global_size
    } else {
      if(!is.null(input$themeYAxisTextSize)) input$themeYAxisTextSize else 8.8
    }
    
    y_font <- if(use_global_font) {
      global_font
    } else {
      if(!is.null(input$themeYAxisTextFamily)) input$themeYAxisTextFamily else "sans"
    }
    
    y_face <- if(use_global_font) {
      global_face
    } else {
      if(!is.null(input$themeYAxisTextFace)) input$themeYAxisTextFace else "plain"
    }
    
    y_angle <- if(!is.null(input$themeYAxisTextAngle)) {
      input$themeYAxisTextAngle
    } else {
      0
    }
    
    y_color <- if(!is.null(input$themeYAxisTextColor)) {
      input$themeYAxisTextColor
    } else {
      "#000000"
    }
    
    # Build element_text() only if different from defaults or has angle/color
    y_params <- c()
    
    if(!use_global_size && y_size != 8.8) {
      y_params <- c(y_params, paste0("size = ", y_size))
    }
    if(!use_global_font && y_face != "plain") {
      y_params <- c(y_params, paste0("face = '", y_face, "'"))
    }
    if(!use_global_font && y_font != "sans") {
      y_params <- c(y_params, paste0("family = '", y_font, "'"))
    }
    if(y_color != "#000000") {
      y_params <- c(y_params, paste0("color = '", y_color, "'"))
    }
    if(y_angle != 0) {
      y_params <- c(y_params, paste0("angle = ", y_angle))
      # Add hjust/vjust for angled text
      y_params <- c(y_params, "hjust = 1", "vjust = 1")
    }
    
    if(length(y_params) > 0) {
      theme_elements <- c(theme_elements,
        paste0("axis.text.y = element_text(", paste(y_params, collapse = ", "), ")"))
    }
    
    # Return list with comment and elements
    if(length(theme_elements) > 0) {
      axis_comment <- safeGetUILabel("codeCommentAxisText", labels, "Axis text customization")
      return(list(
        comment = axis_comment,
        elements = theme_elements
      ))
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    debugLog(paste("ERROR in generateThemeAxisElements:", e$message), "CODE_ERROR")
    return(NULL)
  })
}
