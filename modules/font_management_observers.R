# Font Management Observers
# Handles system font checkbox, import, and re-import functionality

# This module is sourced by data_handling.R (server_main function)
# It manages font cache and observers for font-related user actions

# Load font utilities (required for this module)
source("modules/font_utilities.R")
debugLog("Loaded font_utilities.R for font management", "MODULE_LOAD")

# ============================================================================
# Font cache initialization
# ============================================================================

# Font cache - simple variables (not reactive)
cached_fonts <- NULL
cached_include_system <- FALSE

# ============================================================================
# Main checkbox observer - Include system fonts
# ============================================================================

observeEvent(input$includeSystemFonts, {
  if(input$includeSystemFonts) {
    
    # Check if we need to refresh cache
    if(is.null(cached_fonts) || !cached_include_system) {
      
      # Check extrafont status (three states)
      status <- checkExtrafontStatus(labels)
      
      if(status$status == "not_installed") {
        # State 1: Package not installed
        showModal(modalDialog(
          title = getCachedLabel("extrafontNotInstalledTitle", labels, 
                                "Package Not Installed"),
          p(getCachedLabel("extrafontNotInstalledMessage", labels,
            "The extrafont package is required for system fonts. Please install it first:")),
          tags$code("install.packages('extrafont')"),
          footer = tagList(
            modalButton(getCachedLabel("okLabel", labels, "OK"))
          )
        ))
        
        updateCheckboxInput(session, "includeSystemFonts", value = FALSE)
        
      } else if(status$status == "not_imported") {
        # State 2: Need to import fonts
        
        if(allowSystemFontsImport) {
          # Local mode: Allow import
          showModal(modalDialog(
            title = getCachedLabel("fontImportTitle", labels, "Import System Fonts"),
            
            p(getCachedLabel("fontImportWarning", labels,
              "This operation will import all system fonts. This may take several minutes (typically 2-5 minutes).")),
            
            p(strong(getCachedLabel("fontImportNote", labels,
              "Note: This only needs to be done once. The fonts will remain available in future sessions."))),
            
            checkboxInput("suppressFontWarnings",
              getCachedLabel("suppressFontWarningsLabel", labels,
                "Suppress warnings for fonts without metadata (recommended)"),
              value = TRUE),
            
            footer = tagList(
              actionButton("confirmFontImport",
                getCachedLabel("confirmImportLabel", labels, "Import Fonts")),
              modalButton(getCachedLabel("cancelLabel", labels, "Cancel"))
            )
          ))
        } else {
          # Server mode: Import not allowed
          showModal(modalDialog(
            title = getCachedLabel("fontImportDisabledTitle", labels, 
                                  "Font Import Disabled"),
            p(getCachedLabel("fontImportDisabledMessage", labels,
              "System font import is disabled in this deployment. Only pre-imported fonts are available.")),
            p(getCachedLabel("fontImportDisabledNote", labels,
              "If you need system fonts, please contact your administrator or use a local installation.")),
            footer = tagList(
              modalButton(getCachedLabel("okLabel", labels, "OK"))
            )
          ))
          
          updateCheckboxInput(session, "includeSystemFonts", value = FALSE)
        }
        
      } else {
        # State 3: Fonts ready (fast path)
        cached_fonts <<- getAvailableFonts(include_system_fonts = TRUE)
        cached_include_system <<- TRUE
        updateAllFontSelectors(session, cached_fonts)
        
        showNotification(
          paste(getCachedLabel("fontsLoadedMessage", labels, 
                              "System fonts loaded:"), 
                status$message),
          type = "message",
          duration = 3
        )
      }
      
    } else {
      # Use cached version (immediate)
      updateAllFontSelectors(session, cached_fonts)
    }
    
  } else {
    # Checkbox unchecked - revert to standard fonts
    cached_fonts <<- NULL
    cached_include_system <<- FALSE
    
    standard_fonts <- getAvailableFonts(include_system_fonts = FALSE)
    updateAllFontSelectors(session, standard_fonts)
  }
})

# ============================================================================
# Import confirmation observer
# ============================================================================

observeEvent(input$confirmFontImport, {
  removeModal()
  
  suppress_warnings <- input$suppressFontWarnings %||% TRUE
  
  # Import fonts with progress
  result <- importSystemFonts(suppress_warnings = suppress_warnings, 
                             session = session)
  
  if(result$success) {
    # Update cache
    cached_fonts <<- result$fonts
    cached_include_system <<- TRUE
    
    # Update all font selectors
    updateAllFontSelectors(session, cached_fonts)
    
    showNotification(
      paste(getCachedLabel("fontsImportedMessage", labels, 
                          "System fonts imported successfully:"),
            result$count, "fonts"),
      type = "message",
      duration = 5
    )
  } else {
    showNotification(
      paste(getCachedLabel("fontImportError", labels, "Font import failed:"),
            result$error),
      type = "error",
      duration = 10
    )
    
    updateCheckboxInput(session, "includeSystemFonts", value = FALSE)
  }
})

# ============================================================================
# Re-import observers (only if allowSystemFontsImport = TRUE)
# ============================================================================

if(allowSystemFontsImport) {
  
  # Re-import button click
  observeEvent(input$reimportFonts, {
    showModal(modalDialog(
      title = getCachedLabel("reimportFontsTitle", labels, 
                            "Re-import System Fonts"),
      
      p(getCachedLabel("reimportFontsQuestion", labels,
        "This will re-scan your system for fonts. Use this after installing new fonts.")),
      
      p(getCachedLabel("reimportFontsWarning", labels,
        "This operation may take several minutes.")),
      
      checkboxInput("suppressFontWarningsReimport",
        getCachedLabel("suppressFontWarningsLabel", labels,
          "Suppress warnings for fonts without metadata (recommended)"),
        value = TRUE),
      
      footer = tagList(
        actionButton("confirmReimport",
          getCachedLabel("confirmLabel", labels, "Re-import")),
        modalButton(getCachedLabel("cancelLabel", labels, "Cancel"))
      )
    ))
  })
  
  # Re-import confirmation
  observeEvent(input$confirmReimport, {
    removeModal()
    
    suppress_warnings <- input$suppressFontWarningsReimport %||% TRUE
    
    # Import fonts
    result <- importSystemFonts(suppress_warnings = suppress_warnings, 
                               session = session)
    
    if(result$success) {
      # Update cache
      cached_fonts <<- result$fonts
      cached_include_system <<- TRUE
      
      updateAllFontSelectors(session, result$fonts)
      
      showNotification(
        paste(getCachedLabel("fontsReimportedMessage", labels, 
          "System fonts re-imported successfully:"), result$count, "fonts"),
        type = "message",
        duration = 5
      )
    } else {
      showNotification(
        paste(getCachedLabel("fontReimportError", labels, 
          "Font re-import failed:"), result$error),
        type = "error",
        duration = 10
      )
    }
  })
}

debugLog("FONT_MANAGEMENT_OBSERVERS: Font management system loaded", "MODULE_LOAD")
