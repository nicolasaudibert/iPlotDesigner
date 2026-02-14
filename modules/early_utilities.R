# Early utilities module
# Utility functions that need to be available early in the loading sequence
# These functions are used across multiple modules and don't fit thematically into global_settings

# ============================================================================
# String escaping functions
# ============================================================================

# Escape strings for safe JavaScript injection in Shiny dialogs
escapeJavaScriptString <- function(str) {
  if(is.null(str) || str == "") {
    return("")
  }
  
  # Convert to character if not already
  str <- as.character(str)
  
  # Escape special characters for JavaScript
  escaped_str <- gsub("\\", "\\\\", str, fixed = TRUE)  # Backslash first
  escaped_str <- gsub("'", "\\'", escaped_str, fixed = TRUE)
  escaped_str <- gsub('"', '\\"', escaped_str, fixed = TRUE)
  escaped_str <- gsub("\n", "\\n", escaped_str, fixed = TRUE)
  escaped_str <- gsub("\r", "\\r", escaped_str, fixed = TRUE)
  escaped_str <- gsub("\t", "\\t", escaped_str, fixed = TRUE)
  
  return(escaped_str)
}

# Function to escape single quotes in labels for code generation
# Prevents conflicts when inserting localized text into generated R code
escapeSingleQuotesInLabel <- function(label_text) {
  if (is.null(label_text) || is.na(label_text) || label_text == "") {
    return("")
  }
  
  # Convert to character if not already
  label_text <- as.character(label_text)
  
  # Escape single quotes by replacing ' with \'
  escaped_text <- gsub("'", "\'", label_text, fixed = TRUE)
  
  return(escaped_text)
}

# Function to escape variable names for safe insertion in generated code
escapeVariableNameForCode <- function(var_name) {
  if (is.null(var_name) || is.na(var_name) || var_name == "") {
    return("")
  }
  
  var_name <- as.character(var_name)
  
  # Escape single quotes in variable names
  escaped_name <- gsub("'", "\\'", var_name, fixed = TRUE)
  
  return(escaped_name)
}

# ============================================================================
# Variable name handling utilities
# ============================================================================

# Function to escape variable names with special characters for R code
escapeVarName <- function(varName) {
  if(is.null(varName) || varName == "") {
    return("")
  }
  
  if(grepl("[^[:alnum:]_.]", varName)) {
    return(paste0("`", varName, "`"))
  } else {
    return(varName)
  }
}

# Escape variable name for use in labels/filenames
escapeVariableNameForLabels <- function(var_name) {
  if(is.null(var_name) || var_name == "") {
    return("")
  }
  
  # Remove special characters that could cause issues in filenames
  escaped_name <- gsub("[^[:alnum:]_. -]", "", var_name)
  
  # Replace multiple spaces/underscores with single underscore
  escaped_name <- gsub("[ _]+", "_", escaped_name)
  
  # Remove leading/trailing underscores
  escaped_name <- gsub("^_|_$", "", escaped_name)
  
  return(escaped_name)
}

# Function to get clean variable names for plotting
getCleanVariables <- function(input, labels) {
  list(
    indVar1 = input$independentVar1,
    indVar2 = cleanVariableName(input$independentVar2, labels),
    indVar3 = cleanVariableName(input$independentVar3, labels)
  )
}

# Function to generate configuration signature for comparison
getVariableConfigSignature <- function(var1, var2, var3, labels) {
  cleanVar1 <- if(is.null(var1) || var1 == "") "" else var1
  cleanVar2 <- if(is.null(cleanVariableName(var2, labels))) "" else cleanVariableName(var2, labels)
  cleanVar3 <- if(is.null(cleanVariableName(var3, labels))) "" else cleanVariableName(var3, labels)
  
  paste(sort(c(cleanVar1, cleanVar2, cleanVar3)), collapse = "_")
}

# ============================================================================
# Intelligent file naming system
# ============================================================================

# Generate intelligent filename based on plot configuration
generateIntelligentFilename <- function(base_name, input, extension = NULL, labels = NULL) {
  # Get variables information with fallbacks
  dep_var <- if(!is.null(input$dependentVar) && input$dependentVar != "") {
    escapeVariableNameForLabels(input$dependentVar)
  } else {
    "data"
  }
  
  ind_var1 <- if(!is.null(input$independentVar1) && input$independentVar1 != "") {
    escapeVariableNameForLabels(input$independentVar1)  
  } else {
    NULL
  }
  
  # Get clean variable names for var2 and var3
  ind_var2 <- cleanVariableForCodeGeneration(input$independentVar2, labels)
  ind_var3 <- cleanVariableForCodeGeneration(input$independentVar3, labels)
  
  if(!is.null(ind_var2)) ind_var2 <- escapeVariableNameForLabels(ind_var2)
  if(!is.null(ind_var3)) ind_var3 <- escapeVariableNameForLabels(ind_var3)
  
  # Build filename components
  filename_parts <- c(base_name)
  
  # Add dependent variable
  if(!is.null(dep_var) && dep_var != "data") {
    filename_parts <- c(filename_parts, dep_var)
  }
  
  # Add "by" separator if we have independent variables
  if(!is.null(ind_var1)) {
    filename_parts <- c(filename_parts, "by", ind_var1)
  }
  
  # Add additional variables if present
  if(!is.null(ind_var2)) {
    filename_parts <- c(filename_parts, "and", ind_var2)
  }
  if(!is.null(ind_var3)) {
    filename_parts <- c(filename_parts, "and", ind_var3)
  }
  
  # Clean and join parts
  clean_parts <- sapply(filename_parts, function(x) {
    if(is.null(x) || x == "") return(NULL)
    # Replace spaces and special characters with underscores, remove multiple underscores
    cleaned <- gsub("[^[:alnum:]_]", "_", x)
    cleaned <- gsub("_{2,}", "_", cleaned)  # Remove multiple consecutive underscores
    cleaned <- gsub("^_|_$", "", cleaned)   # Remove leading/trailing underscores
    return(cleaned)
  })
  
  # Remove NULL and empty parts
  clean_parts <- clean_parts[!sapply(clean_parts, is.null) & clean_parts != ""]
  
  filename <- paste(clean_parts, collapse = "_")
  
  # Add extension if provided
  if(!is.null(extension)) {
    filename <- paste0(filename, extension)
  }
  
  return(filename)
}


# ============================================================================
# Utility helper functions
# ============================================================================

# Extract variable number from callback ID
extractVariableNumber <- function(callback_id) {
  if (grepl("var1", callback_id)) return(1)
  if (grepl("var2", callback_id)) return(2) 
  if (grepl("var3", callback_id)) return(3)
  return(1) # fallback
}

# ============================================================================
# Font System - UI-safe font retrieval
# ============================================================================
# 
# This function is used during UI initialization and must not depend on
# session objects or other Shiny reactive contexts.
# 
# The full font management system (with session-dependent functions) is
# defined in font_utilities.R and loaded later via font_management_observers.R

# Standard fonts definition
STANDARD_FONTS <- c("sans", "serif", "mono", "Helvetica", "Times", "Courier")

#' Filter Problematic System Fonts
#' 
#' Removes fonts known to cause issues in R graphics devices
#' 
#' @param fonts Character vector of font names
#' @return Filtered character vector

filterProblematicFonts <- function(fonts) {
  
  if (length(fonts) == 0) return(fonts)
  
  # Build exclusion pattern from list
  exclude_pattern <- paste(EXCLUDED_FONTS, collapse = "|")
  
  # Filter using logical vector
  cleaned_fonts <- fonts[
    # Remove macOS system fonts (starting with .)
    !grepl("^\\.", fonts) &
      
      # Remove "System Font" generic alias
      fonts != "System Font" &
      
      # Remove empty strings
      nchar(fonts) > 0 &
      
      # Remove fonts with control characters (corrupted encoding)
      !grepl("[[:cntrl:]]", fonts) &
      
      # Remove common problematic fonts (case-insensitive, dynamic pattern built from EXCLUDED_FONTS list)
      !grepl(exclude_pattern, fonts, ignore.case = TRUE) &
      
      # Keep only fonts with standard characters
      # Allows: letters, numbers, spaces, hyphens, periods
      grepl("^[A-Za-z0-9 .\\-]+$", fonts)
  ]
  
  # Remove duplicates and sort
  cleaned_fonts <- unique(cleaned_fonts)
  cleaned_fonts <- sort(cleaned_fonts)
  
  return(cleaned_fonts)
}

#' Get Available Fonts (UI-safe version)
#' 
#' Returns list of available fonts for UI initialization.
#' This is a simplified version that works during UI construction.
#' 
#' @param include_system_fonts Logical. If TRUE, attempts to load system fonts.
#'        Default FALSE.
#' @return Character vector of font names
#' 
#' @details
#' - Standard fonts (6): sans, serif, mono, Helvetica, Times, Courier
#' - System fonts: Loaded from extrafont if available
#' - No session dependencies: safe to call during UI construction
#' - Graceful degradation: returns standard fonts if extrafont unavailable
#' 
#' @examples
#' # Standard fonts only (for UI initialization)
#' getAvailableFonts()
#' 
#' # With system fonts (if extrafont configured)
#' getAvailableFonts(include_system_fonts = TRUE)

getAvailableFonts <- function(include_system_fonts = FALSE) {
  
  # Always start with standard fonts
  available_fonts <- STANDARD_FONTS
  
  # If system fonts not requested, return immediately
  if (!include_system_fonts) {
    return(available_fonts)
  }
  
  # Try to load system fonts with error handling
  tryCatch({
    
    # Check if extrafont package is available
    if (!requireNamespace("extrafont", quietly = TRUE)) {
      # Package not installed - return standard fonts
      return(available_fonts)
    }
    
    # Try to get system fonts
    system_fonts <- extrafont::fonts()
    
    # Filter out problematic fonts
    if (length(system_fonts) > 0) {
      system_fonts <- filterProblematicFonts(system_fonts)
    }
    
    # If no fonts after filtering, return standard fonts
    if (length(system_fonts) == 0) {
      return(available_fonts)
    }
    
    # Combine standard and alphabetically sorted system fonts (remove duplicates)
    available_fonts <- unique(c(STANDARD_FONTS, sort(system_fonts)))
    
    # # Sort alphabetically
    # available_fonts <- sort(available_fonts)
    
    return(available_fonts)
    
  }, error = function(e) {
    # On any error, return standard fonts
    # This ensures UI always has valid font choices
    return(STANDARD_FONTS)
  })
}

debugLog("EARLY_UTILITIES: Early utility functions loaded", "APP_STARTUP")
