# Font Utilities
# Core functions for font management system
# Sourced by font_management_observers.R

# ============================================================================
# Standard fonts definition
# ============================================================================

# Standard fonts always available (R base + common system fonts)
STANDARD_FONTS <- c("sans", "serif", "mono", "Helvetica", "Times", "Courier")

# ============================================================================
# NOTE: getAvailableFonts() is defined in early_utilities.R
# ============================================================================
# 
# This function is called during UI initialization, so it must be available
# early in the loading process. It's defined in early_utilities.R with a
# simple, UI-safe implementation that doesn't depend on session objects.
# 
# The functions below (checkExtrafontStatus, importSystemFonts, etc.) are
# server-side functions that DO require session context and are loaded later.

# ============================================================================
# checkExtrafontStatus() - Three-state extrafont detection
# ============================================================================

#' Check Extrafont Package Status
#' 
#' Detects extrafont installation and font import status
#' 
#' @return List with three elements:
#'   - status: Character ("not_installed", "not_imported", or "ready")
#'   - fonts: Character vector of available fonts (NULL if not ready)
#'   - message: Descriptive message for user feedback
#'   
#' @details
#' Three possible states:
#' 1. "not_installed": extrafont package not installed
#' 2. "not_imported": Package installed but no fonts imported yet
#' 3. "ready": Package installed and fonts available

checkExtrafontStatus <- function(labels = NULL) {
  
  debugLog("checkExtrafontStatus() called", "FONT_DETECT")
  
  # State 1: Check if extrafont package is installed
  if (!requireNamespace("extrafont", quietly = TRUE)) {
    debugLog("  Status: not_installed", "FONT_DETECT")
    
    msg <- if (!is.null(labels)) {
      getCachedLabel("extrafontNotInstalledMessage", labels, "extrafont package not installed")
    } else {
      "extrafont package not installed"
    }
    
    return(list(
      status = "not_installed",
      fonts = NULL,
      message = msg
    ))
  }
  
  # State 2: Package installed - check if fonts are imported
  tryCatch({
    system_fonts <- extrafont::fonts()
    
    if (length(system_fonts) == 0) {
      debugLog("  Status: not_imported (0 fonts found)", "FONT_DETECT")
      
      msg <- if (!is.null(labels)) {
        getCachedLabel("noFontsImportedMessage", labels, "No fonts imported yet")
      } else {
        "No fonts imported yet"
      }
      
      return(list(
        status = "not_imported",
        fonts = NULL,
        message = msg
      ))
    }
    
    # Filter problematic fonts
    system_fonts <- filterProblematicFonts(system_fonts)
    
    # State 3: Fonts available
    debugLog(paste("  Status: ready (", length(system_fonts), " fonts after filtering)"), 
             "FONT_DETECT")
    
    msg <- if (!is.null(labels)) {
      paste(length(system_fonts), getCachedLabel("fontsAvailableMessage", labels, "fonts available"))
    } else {
      paste(length(system_fonts), "fonts available")
    }
    
    return(list(
      status = "ready",
      fonts = system_fonts,
      message = msg
    ))
    
  }, error = function(e) {
    debugLog(paste("  Error checking fonts:", e$message), "FONT_ERROR")
    
    msg <- if (!is.null(labels)) {
      paste(getCachedLabel("errorLabel", labels, "Error:"), e$message)
    } else {
      paste("Error:", e$message)
    }
    
    return(list(
      status = "not_imported",
      fonts = NULL,
      message = msg
    ))
  })
}

# ============================================================================
# importSystemFonts() - Import system fonts via extrafont
# ============================================================================

#' Import System Fonts
#' 
#' Imports system fonts using extrafont::font_import()
#' Shows progress notifications during import
#' 
#' @param suppress_warnings Logical. If TRUE, suppresses extrafont warnings
#'        about fonts without afm/pfm files. Default TRUE.
#' @param session Shiny session object for progress notifications
#' 
#' @return List with elements:
#'   - success: Logical indicating if import succeeded
#'   - fonts: Character vector of imported fonts (NULL on failure)
#'   - count: Number of fonts imported
#'   - error: Error message (NULL on success)

importSystemFonts <- function(suppress_warnings = TRUE, session = NULL) {
  
  debugLog("importSystemFonts() called", "FONT_IMPORT")
  debugLog(paste("  suppress_warnings =", suppress_warnings), "FONT_IMPORT")
  
  if (!requireNamespace("extrafont", quietly = TRUE)) {
    error_msg <- "extrafont package not installed"
    debugLog(paste("  ERROR:", error_msg), "FONT_ERROR")
    return(list(
      success = FALSE,
      fonts = NULL,
      count = 0,
      error = error_msg
    ))
  }
  
  # Show progress notification
  if (!is.null(session)) {
    showNotification(
      getCachedLabel("fontImportInProgress", labels, 
                     "Importing system fonts... This may take several minutes."),
      duration = NULL,
      closeButton = FALSE,
      id = "fontImportProgress",
      type = "message"
    )
  }
  
  # Import fonts with error handling
  tryCatch({
    debugLog("  Starting font import...", "FONT_IMPORT")
    
    # Suppress warnings if requested
    if (suppress_warnings) {
      suppressWarnings(extrafont::font_import(prompt = FALSE))
    } else {
      extrafont::font_import(prompt = FALSE)
    }
    
    # Load fonts into current session
    extrafont::loadfonts(quiet = TRUE)
    
    # Get imported fonts
    imported_fonts <- extrafont::fonts()
    font_count <- length(imported_fonts)
    
    debugLog(paste("  Import successful:", font_count, "fonts imported"), 
             "FONT_IMPORT")
    
    # Remove progress notification
    if (!is.null(session)) {
      removeNotification("fontImportProgress", session = session)
    }
    
    return(list(
      success = TRUE,
      fonts = imported_fonts,
      count = font_count,
      error = NULL
    ))
    
  }, error = function(e) {
    error_msg <- e$message
    debugLog(paste("  Import failed:", error_msg), "FONT_ERROR")
    
    # Remove progress notification
    if (!is.null(session)) {
      removeNotification("fontImportProgress", session = session)
    }
    
    return(list(
      success = FALSE,
      fonts = NULL,
      count = 0,
      error = error_msg
    ))
  })
}

# ============================================================================
# updateAllFontSelectors() - Update all font select inputs
# ============================================================================

#' Update All Font Selectors
#' 
#' Updates all font select inputs across the application
#' Maintains current selections where possible
#' 
#' @param session Shiny session object
#' @param fonts Character vector of available fonts

updateAllFontSelectors <- function(session, fonts) {
  
  debugLog("updateAllFontSelectors() called", "UI_UPDATE")
  debugLog(paste("  Updating with", length(fonts), "fonts"), "UI_UPDATE")
  
  # List of all font selector IDs
  font_selectors <- c(
    "defaultFont",           # Global default (ui_general_settings.R)
    "themeTitleFont",        # Plot title (ui_theme_titles.R)
    "themeSubtitleFont",     # Subplot title
    "themeAxisTitleXFont",   # X-axis title
    "themeAxisTitleYFont",   # Y-axis title
    "themeAxisTextXFont",    # X-axis text (ui_theme_axis_text.R)
    "themeAxisTextYFont"     # Y-axis text
  )
  
  # Update each selector
  for (selector_id in font_selectors) {
    
    # Get current selection to preserve it if possible
    current_value <- session$input[[selector_id]]
    
    # Update choices
    updateSelectInput(
      session = session,
      inputId = selector_id,
      choices = fonts,
      selected = if (!is.null(current_value) && current_value %in% fonts) {
        current_value
      } else {
        fonts[1]  # Default to first font if current not available
      }
    )
    
    debugLog(paste("  Updated", selector_id), "UI_UPDATE")
  }
  
  debugLog("  All font selectors updated", "UI_UPDATE")
}

# ============================================================================
# End of font_utilities.R
# ============================================================================
