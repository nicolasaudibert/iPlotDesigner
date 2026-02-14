# Code Generation - Font Setup
# Handles detection of non-standard fonts and extrafont setup code generation

# ============================================================================
# Font detection functions
# ============================================================================

#' Detect Non-Standard Fonts Used in Plot
#' 
#' Scans all font-related inputs and returns fonts that require extrafont
#' 
#' @param input Shiny input object
#' @return Character vector of non-standard fonts (empty if none)
#' 
#' @details
#' Standard fonts (always available):
#' - sans, serif, mono (R base fonts)
#' - Helvetica, Times, Courier (common system fonts)
#' 
#' Any other font requires extrafont to be loaded

detectNonStandardFonts <- function(input) {
  
  debugLog("detectNonStandardFonts() called", "CODE_FONT_DETECT")
  
  # Standard fonts that don't require extrafont
  standard_fonts <- c("sans", "serif", "mono", "Helvetica", "Times", "Courier")
  
  # List of all font input IDs in the application
  font_inputs <- c(
    "defaultFont",              # Global default font
    "themeAxisTitleXFont",      # X-axis title
    "themeAxisTitleYFont",      # Y-axis title
    "themeStripTextFont",       # Strip text (unified)
    "themeStripTextXFont",      # Strip text X
    "themeStripTextYFont",      # Strip text Y
    "themeLegendTitleFont",     # Legend title
    "themeLegendTextFont",      # Legend text
    "themeXAxisTextFamily",     # X-axis text
    "themeYAxisTextFamily"      # Y-axis text
  )
  
  # Collect all fonts actually used
  fonts_used <- character(0)
  
  for (input_id in font_inputs) {
    font_value <- input[[input_id]]
    
    if (!is.null(font_value) && nchar(font_value) > 0) {
      fonts_used <- c(fonts_used, font_value)
      debugLog(paste("  Found font in", input_id, ":", font_value), "CODE_FONT_DETECT")
    }
  }
  
  # Get unique fonts
  fonts_used <- unique(fonts_used)
  
  # Filter out standard fonts
  non_standard_fonts <- fonts_used[!fonts_used %in% standard_fonts]
  
  debugLog(paste("  Total fonts used:", length(fonts_used)), "CODE_FONT_DETECT")
  debugLog(paste("  Non-standard fonts:", length(non_standard_fonts)), "CODE_FONT_DETECT")
  
  if (length(non_standard_fonts) > 0) {
    debugLog(paste("  Non-standard fonts list:", 
                   paste(non_standard_fonts, collapse = ", ")), 
             "CODE_FONT_DETECT")
  }
  
  return(non_standard_fonts)
}

# ============================================================================
# Code generation function
# ============================================================================

#' Generate Extrafont Setup Code
#' 
#' Generates commented code for loading extrafont with instructions
#' Only called if non-standard fonts are detected
#' 
#' @param non_standard_fonts Character vector of font names
#' @param labels Localization labels object
#' @return Character string with formatted code (or empty string if no fonts)

generateFontSetupCode <- function(non_standard_fonts, labels) {
  
  debugLog("generateFontSetupCode() called", "CODE_FONT_SETUP")
  debugLog(paste("  Fonts to include:", length(non_standard_fonts)), "CODE_FONT_SETUP")
  
  # If no non-standard fonts, return empty
  if (length(non_standard_fonts) == 0) {
    debugLog("  No non-standard fonts - skipping font setup code", "CODE_FONT_SETUP")
    return("")
  }
  
  # Get localized labels
  fontSetupComment <- safeGetUILabel("codeCommentFontSetup", labels, 
                                      "Font setup (system fonts)")
  requiredFontsComment <- safeGetUILabel("codeCommentRequiredFonts", labels,
                                          "Required fonts:")
  
  # Build code lines
  code_lines <- c(
    paste("#", fontSetupComment),
    "# IMPORTANT: This code requires the extrafont package to be installed:",
    "#   install.packages('extrafont')",
    "#   extrafont::font_import()  # Run once to import system fonts",
    "#   extrafont::loadfonts()    # Run in each session",
    "#",
    paste("#", requiredFontsComment, paste(non_standard_fonts, collapse = ", ")),
    "",
    "# Load extrafont (required for non-standard fonts)",
    "library(extrafont)",
    "loadfonts(quiet = TRUE)  # Suppress verbose output",
    ""
  )
  
  debugLog("  Font setup code generated successfully", "CODE_FONT_SETUP")
  
  return(paste(code_lines, collapse = "\n"))
}

# ============================================================================
# Main integration function
# ============================================================================

#' Generate Font Setup Section for Code Header
#' 
#' Main function to be called from generateCodeHeader()
#' Detects fonts and generates setup code if needed
#' 
#' @param input Shiny input object
#' @param labels Localization labels object
#' @return Character string with font setup code (empty if not needed)

generateFontSetupSection <- function(input, labels) {
  
  debugLog("generateFontSetupSection() called", "CODE_FONT_SETUP")
  
  # Detect non-standard fonts
  non_standard_fonts <- detectNonStandardFonts(input)
  
  # Generate code if needed
  if (length(non_standard_fonts) > 0) {
    code <- generateFontSetupCode(non_standard_fonts, labels)
    debugLog("  Font setup section generated", "CODE_FONT_SETUP")
    return(code)
  } else {
    debugLog("  No font setup section needed", "CODE_FONT_SETUP")
    return("")
  }
}

# ============================================================================
# End of code_generation_fonts.R
# ============================================================================

debugLog("CODE_GENERATION_FONTS: Font code generation module loaded", "MODULE_LOAD")
