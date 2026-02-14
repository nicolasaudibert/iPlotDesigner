# Code Generation Theme - Grids and Borders Module
# Handles grids, borders, and show/hide logic

# Generate elements for grid and border customization
# Returns list with comment and elements for inclusion in main theme() call
generateThemeGridsElements <- function(input, labels) {
  
  tryCatch({
    theme_elements <- c()
    
    # Show/hide checkboxes
    show_major_grid <- if(!is.null(input$themeShowMajorGrid)) {
      input$themeShowMajorGrid
    } else {
      TRUE
    }
    
    show_minor_grid <- if(!is.null(input$themeShowMinorGrid)) {
      input$themeShowMinorGrid
    } else {
      TRUE
    }
    
    show_legend_key <- if(!is.null(input$themeShowLegendKey)) {
      input$themeShowLegendKey
    } else {
      TRUE
    }
    
    show_panel_border <- if(!is.null(input$themeShowPanelBorder)) {
      input$themeShowPanelBorder
    } else {
      TRUE
    }
    
    show_strip_border <- if(!is.null(input$themeShowStripBorder)) {
      input$themeShowStripBorder
    } else {
      FALSE  # theme_light() default is no strip border
    }
    
    # Major grid
    if(!show_major_grid) {
      # Hidden: use element_blank()
      theme_elements <- c(theme_elements, "panel.grid.major = element_blank()")
    } else {
      # Shown: check if color/size differ from defaults
      grid_major_color <- if(!is.null(input$themeGridMajorColor)) {
        input$themeGridMajorColor
      } else {
        "#DEDEDE"
      }
      
      grid_major_size <- if(!is.null(input$themeGridMajorSize)) {
        input$themeGridMajorSize
      } else {
        0.5
      }
      
      # Only generate if different from defaults
      if(grid_major_color != "#DEDEDE" || grid_major_size != 0.5) {
        theme_elements <- c(theme_elements,
          paste0("panel.grid.major = element_line(color = '", grid_major_color, 
                 "', linewidth = ", grid_major_size, ")"))
      }
    }
    
    # Minor grid
    if(!show_minor_grid) {
      # Hidden: use element_blank()
      theme_elements <- c(theme_elements, "panel.grid.minor = element_blank()")
    } else {
      # Shown: check if color/size differ from defaults
      grid_minor_color <- if(!is.null(input$themeGridMinorColor)) {
        input$themeGridMinorColor
      } else {
        "#DEDEDE"
      }
      
      grid_minor_size <- if(!is.null(input$themeGridMinorSize)) {
        input$themeGridMinorSize
      } else {
        0.25
      }
      
      # Only generate if different from defaults
      if(grid_minor_color != "#DEDEDE" || grid_minor_size != 0.25) {
        theme_elements <- c(theme_elements,
          paste0("panel.grid.minor = element_line(color = '", grid_minor_color, 
                 "', linewidth = ", grid_minor_size, ")"))
      }
    }
    
    # Panel border
    if(!show_panel_border) {
      # Hidden: use element_blank()
      theme_elements <- c(theme_elements, "panel.border = element_blank()")
    } else {
      # Shown: check if color/size differ from defaults
      panel_border_color <- if(!is.null(input$themePanelBorderColor)) {
        input$themePanelBorderColor
      } else {
        "#B3B3B3"
      }
      
      panel_border_size <- if(!is.null(input$themePanelBorderSize)) {
        input$themePanelBorderSize
      } else {
        1
      }
      
      # Only generate if different from defaults
      if(panel_border_color != "#B3B3B3" || panel_border_size != 1) {
        theme_elements <- c(theme_elements,
          paste0("panel.border = element_rect(color = '", panel_border_color, 
                 "', linewidth = ", panel_border_size, ", fill = NA)"))
      }
    }
    
    # Strip border (special: default is FALSE/hidden in theme_light)
    if(show_strip_border) {
      # Shown: generate with color/size
      strip_border_color <- if(!is.null(input$themeStripBorderColor)) {
        input$themeStripBorderColor
      } else {
        "#B3B3B3"
      }
      
      strip_border_size <- if(!is.null(input$themeStripBorderSize)) {
        input$themeStripBorderSize
      } else {
        0.5
      }
      
      # Always generate when shown since default is hidden
      theme_elements <- c(theme_elements,
        paste0("strip.background = element_rect(color = '", strip_border_color, 
               "', linewidth = ", strip_border_size, ")"))
    }
    
    # Legend key (show/hide only)
    if(!show_legend_key) {
      # Hidden: use element_blank() - takes precedence over color customization
      theme_elements <- c(theme_elements, "legend.key = element_blank()")
    }
    # If shown (default), no code needed unless custom color/border (handled in backgrounds module)
    
    # Return list with comment and elements
    if(length(theme_elements) > 0) {
      grids_comment <- safeGetUILabel("codeCommentGridsBorders", labels, "Grids and borders")
      return(list(
        comment = grids_comment,
        elements = theme_elements
      ))
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    debugLog(paste("ERROR in generateThemeGridsElements:", e$message), "CODE_ERROR")
    return(NULL)
  })
}
