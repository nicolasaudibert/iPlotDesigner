# Dialog support module
# Support functions and utilities for enhanced user validation dialogs
# Extracted from enhanced_user_validation.R

# Note: This module will be loaded on-demand when dependencies are available

# ============================================================================
# Keyboard navigation helper function
# ============================================================================

# Add keyboard navigation to modal dialogs
# Handles Enter (confirm), Escape (cancel), and Tab (navigation between options)
addKeyboardNavigation <- function(confirmBtnId, cancelBtnId = NULL, easyClose = FALSE) {
  
  debugLog(paste("KEYBOARD_NAV: Attaching keyboard handlers - Confirm:", confirmBtnId, 
                 "Cancel:", ifelse(!is.null(cancelBtnId), cancelBtnId, "modal close")), "MODAL_INIT")
  
  # Build cancel action
  cancelAction <- if (!is.null(cancelBtnId)) {
    sprintf("$('#%s').click();", cancelBtnId)
  } else if (easyClose) {
    "$('#shiny-modal').modal('hide');"
  } else {
    "return false;" # No cancel action if not easyClose and no cancel button
  }
  
  # JavaScript code for keyboard event handling
  tags$script(HTML(sprintf("
    $(document).ready(function() {
      // Attach keyboard event handler with namespace for easy cleanup
      $(document).on('keydown.modalKeyboard', function(e) {
        // Enter key - trigger confirm button
        if(e.key === 'Enter' && !e.shiftKey) {
          var activeElement = document.activeElement;
          
          // Don't intercept Enter in textarea or if a button already has focus
          if(activeElement && (activeElement.tagName === 'TEXTAREA' || 
             activeElement.tagName === 'BUTTON')) {
            return true;
          }
          
          e.preventDefault();
          $('#%s').click();
          return false;
        }
        
        // Escape key - trigger cancel or close
        if(e.key === 'Escape') {
          e.preventDefault();
          %s
          return false;
        }
        
        // Tab key - allow default navigation between form elements
        // No custom handling needed - browser handles it
      });
      
      // Auto-focus on first input or confirm button when modal opens
      setTimeout(function() {
        var modal = $('#shiny-modal');
        if(modal.length) {
          var firstInput = modal.find('input:not([type=\"hidden\"]), select, textarea').first();
          if(firstInput.length) {
            firstInput.focus();
          } else {
            $('#%s').focus();
          }
        }
      }, 100);
      
      // Clean up event handlers when modal closes
      $('#shiny-modal').on('hidden.bs.modal', function() {
        $(document).off('keydown.modalKeyboard');
      });
      
      console.log('KEYBOARD_NAV: Keyboard navigation initialized successfully');
    });
  ", confirmBtnId, cancelAction, confirmBtnId)))
}

# ============================================================================
# Localization and label support functions
# ============================================================================

# Get localized complexity level
getLocalizedComplexityLevel <- function(complexity_level, labels) {
  switch(complexity_level,
         "simple" = safeGetUILabel("complexitySimple", labels, "Simple"),
         "moderate" = safeGetUILabel("complexityModerate", labels, "Moderate"), 
         "complex" = safeGetUILabel("complexityComplex", labels, "Complex"),
         "very_complex" = safeGetUILabel("complexityVeryComplex", labels, "Very Complex"),
         "extreme" = safeGetUILabel("complexityExtreme", labels, "Extreme"),
         complexity_level  # fallback
  )
}

# Get localized performance level
getLocalizedPerformanceLevel <- function(performance_level, labels) {
  switch(performance_level,
         "good" = safeGetUILabel("performanceLevelLow", labels, "Low Impact"),
         "acceptable" = safeGetUILabel("performanceLevelAcceptable", labels, "Acceptable"),
         "slow" = safeGetUILabel("performanceLevelHigh", labels, "High Impact"), 
         "very_slow" = safeGetUILabel("performanceLevelVeryHigh", labels, "Very High Impact"),
         performance_level  # fallback
  )
}

# ============================================================================
# Recommendation engine
# ============================================================================

# Intelligent recommendation engine
determineRecommendedAction <- function(validation_result, complexity_info, issue_type) {
  
  modality_count <- if (!is.null(complexity_info$modality_count)) {
    complexity_info$modality_count
  } else if (!is.null(validation_result$totalModalities)) {
    validation_result$totalModalities
  } else {
    0
  }
  
  performance_impact <- estimatePerformanceImpact(modality_count)
  
  # Context-aware recommendations
  if (issue_type == "numeric_variable") {
    if (modality_count <= USER_CHOICE_SETTINGS$numeric_conversion_threshold) {
      return(list(
        action = "convert_to_categorical",
        reason = "optimal_for_visualization",
        confidence = "high"
      ))
    } else {
      return(list(
        action = "reject_variable",
        reason = "too_many_unique_values", 
        confidence = "high"
      ))
    }
  }
  
  # Complexity-based recommendations
  complexity_level <- getComplexityLevel(modality_count)
  filtering_method <- getRecommendedFilteringMethod(modality_count, complexity_level)
  
  if (performance_impact$performance_level %in% c("slow", "very_slow")) {
    return(list(
      action = "filter_aggressive",
      method = "top_n",
      parameters = list(n = SMART_FILTERING_OPTIONS$top_n_default),
      reason = "performance_optimization",
      confidence = "high"
    ))
  } else if (modality_count > USER_CHOICE_SETTINGS$complexity_warning_threshold) {
    return(list(
      action = "filter_moderate",
      method = filtering_method,
      parameters = list(min_freq = SMART_FILTERING_OPTIONS$min_frequency_default),
      reason = "complexity_management",
      confidence = "medium"
    ))
  } else {
    return(list(
      action = "filter_minimal",
      method = "min_frequency",
      parameters = list(min_freq = 2),
      reason = "data_quality",
      confidence = "high"
    ))
  }
}

# ============================================================================
# Error handling and fallback functions
# ============================================================================

# Create fallback dialog for when advanced dialogs fail
createFallbackDialog <- function(variable_name, error_message, labels) {
  
  debugLog(paste("DIALOG_SUPPORT: Creating fallback dialog for", variable_name, "due to:", error_message), "DIALOG_FALLBACK")
  
  # Get localized labels
  dialogErrorTitle <- safeGetUILabel("dialogErrorTitle", labels, "Dialog Error")
  variableLabel <- safeGetUILabel("variable", labels, "Variable")
  dialogErrorMessage <- safeGetUILabel("dialogErrorMessage", labels, "An error occurred while displaying the advanced dialog.")
  errorDetailsLabel <- safeGetUILabel("errorDetails", labels, "Error details")
  fallbackMessage <- safeGetUILabel("fallbackMessage", labels, "Please use the basic validation or contact support.")
  okLabel <- safeGetUILabel("okLabel", labels, "OK")
  cancelLabel <- safeGetUILabel("cancelLabel", labels, "Cancel")
  
  showModal(modalDialog(
    title = dialogErrorTitle,
    div(
      h4(variableLabel, ": ", variable_name),
      p(dialogErrorMessage),
      p(errorDetailsLabel, ": ", error_message),
      p(fallbackMessage)
    ),
    easyClose = FALSE,
    footer = tagList(
      tags$button(
        okLabel,
        id = "fallbackDialogOk",
        class = "btn btn-primary",
        onclick = "$('#shiny-modal').modal('hide');"
      ),
      tags$button(
        cancelLabel,
        id = "fallbackDialogCancel",
        class = "btn btn-default",
        onclick = "$('#shiny-modal').modal('hide');"
      )
    ),
    
    # Add keyboard navigation
    addKeyboardNavigation("fallbackDialogOk", "fallbackDialogCancel")
  ))
}

# Validate required dependencies before dialog creation
validateDialogDependencies <- function() {
  
  missing_functions <- c()
  
  # Check for required global functions
  if (!exists("debugLog", envir = .GlobalEnv)) {
    missing_functions <- c(missing_functions, "debugLog")
  }
  
  if (!exists("estimatePerformanceImpact", envir = .GlobalEnv)) {
    missing_functions <- c(missing_functions, "estimatePerformanceImpact")
  }
  
  if (!exists("getComplexityLevel", envir = .GlobalEnv)) {
    missing_functions <- c(missing_functions, "getComplexityLevel")
  }
  
  # Check for required settings
  if (!exists("USER_CHOICE_SETTINGS", envir = .GlobalEnv)) {
    missing_functions <- c(missing_functions, "USER_CHOICE_SETTINGS")
  }
  
  if (!exists("SMART_FILTERING_OPTIONS", envir = .GlobalEnv)) {
    missing_functions <- c(missing_functions, "SMART_FILTERING_OPTIONS")
  }
  
  if (length(missing_functions) > 0) {
    debugLog(paste("DIALOG_SUPPORT: Missing dependencies:", paste(missing_functions, collapse = ", ")), "DIALOG_INIT")
    return(list(valid = FALSE, missing = missing_functions))
  }
  
  return(list(valid = TRUE))
}

debugLog("DIALOG_SUPPORT: Dialog support and utilities module loaded with keyboard navigation", "APP_STARTUP")
