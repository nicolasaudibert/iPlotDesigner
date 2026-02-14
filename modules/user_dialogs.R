# User dialogs module
# Dialog creation functions for enhanced user validation
# Extracted from enhanced_user_validation.R

# Note: This module will be loaded on-demand when dependencies are available

# ============================================================================
# Dialog type 1 - single-value modalities choice
# ============================================================================

# Dialog type 1 - single-value modalities choice (fully localized version)
showModalitiesChoiceDialog <- function(variable_name, validation_result, recommendation, 
                                       labels, session, callback_id) {
  
  debugLog(paste("MODALITIES_DIALOG: Showing choice dialog for", variable_name), "USER_OPERATION")
  
  # Get all localized labels with safe fallbacks
  modalTitle <- safeGetUILabel("modalitiesChoiceDialogTitle", labels, "Modality Filtering Options")
  variableLabel <- safeGetUILabel("variable", labels, "Variable")
  filteredLabel <- safeGetUILabel("filteredModalitiesLabel", labels, "Filtered modalities")
  chooseActionLabel <- safeGetUILabel("chooseActionLabel", labels, "Choose your preferred action:")
  confirmLabel <- safeGetUILabel("userChoiceConfirmLabel", labels, "Confirm Choice")
  cancelLabel <- safeGetUILabel("userChoiceCancelLabel", labels, "Cancel")
  
  # Dialog options with recommendations
  automaticOption <- safeGetUILabel("automaticallyExcludeInvalidOption", labels, "✓ Automatically exclude invalid modalities (Recommended)")
  keepAllOption <- safeGetUILabel("keepAllModalitiesOption", labels, "⚠ Keep all modalities (may affect plot quality and performance)")
  cancelOption <- safeGetUILabel("cancelVariableSelectionOption", labels, "Cancel variable selection")
  
  # Performance impact labels
  performanceImpactTitle <- safeGetUILabel("performanceImpactTitle", labels, "Performance Impact")
  filteredModalitiesCountLabel <- safeGetUILabel("filteredModalitiesCountLabel", labels, "Filtered modalities")
  estimatedResponseTimeLabel <- safeGetUILabel("estimatedResponseTimeLabel", labels, "Estimated response time")
  uiElementsLabel <- safeGetUILabel("uiElementsLabel", labels, "UI elements")
  memoryUsageLabel <- safeGetUILabel("memoryUsageLabel", labels, "Memory usage")
  
  # Filtered modalities examples labels
  filteredModalitiesExamplesTitle <- safeGetUILabel("filteredModalitiesExamplesTitle", labels, "Examples of filtered modalities (< 2 observations)")
  totalModalitiesLabel <- safeGetUILabel("totalModalitiesLabel", labels, "total")
  
  # Warning labels
  warningAttention <- safeGetUILabel("warningAttention", labels, "Warning")
  keepAllModalitiesWarning <- safeGetUILabel("keepAllModalitiesWarning", labels, "Keeping all modalities may make plots difficult to read and slower to generate")
  
  # JavaScript messages
  pleaseSelectOptionMessage <- safeGetUILabel("pleaseSelectOptionMessage", labels, "Please select an option")
  
  # Performance impact information
  filtered_count <- length(validation_result$filteredModalities)
  total_count <- validation_result$totalModalities
  performance_info <- estimatePerformanceImpact(total_count)
  
  # Determine default choice based on recommendation
  default_choice <- if(!is.null(recommendation$confidence) && recommendation$confidence == "high") {
    "auto_exclude"
  } else {
    NULL  # No default selection
  }
  
  debugLog(paste("MODALITIES_DIALOG: Filtered", filtered_count, "out of", total_count, "modalities"), "USER_OPERATION")
  
  # Create performance impact display
  performance_display <- div(
    style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 8px; margin: 10px 0; border-radius: 3px;",
    h6(icon("info-circle"), " ", performanceImpactTitle),
    p(
      strong(filteredModalitiesCountLabel, " : "), filtered_count, " / ", total_count, br(),
      strong(estimatedResponseTimeLabel, " : "), round(performance_info$estimated_ui_time, 1), "s", br(),
      strong(uiElementsLabel, " : "), performance_info$dom_elements, br(),
      strong(memoryUsageLabel, " : "), round(performance_info$memory_usage_mb, 1), " MB"
    )
  )
  
  # Safe variable name and callback ID for JavaScript
  safe_variable_name <- escapeJavaScriptString(variable_name)
  safe_callback_id <- escapeJavaScriptString(callback_id)
  safe_select_message <- escapeJavaScriptString(pleaseSelectOptionMessage)
  
  # Show modal with intelligent options
  showModal(modalDialog(
    title = div(
      icon("filter", style = "color: #007bff; margin-right: 10px;"),
      modalTitle
    ),
    
    div(
      # Variable information
      h5(strong(variableLabel, " : "), variable_name),
      p(strong(filteredLabel, " : "), filtered_count, " ", safeGetUILabel("modalitiesLabel", labels, "modalities"), " ", totalModalitiesLabel, " ", total_count),
      
      # Show some examples of filtered modalities
      if(filtered_count > 0) {
        div(
          style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 8px; margin: 10px 0; border-radius: 3px;",
          h6(filteredModalitiesExamplesTitle, " :"),
          p(paste(head(validation_result$filteredModalities, 5), collapse = ", "),
            if(filtered_count > 5) "..." else "")
        )
      },
      
      # Performance impact preview
      performance_display,
      
      br(),
      
      # Choice options with radio buttons
      div(
        h6(chooseActionLabel),
        div(
          style = "margin: 15px 0;",
          div(
            style = "margin: 8px 0;",
            tags$label(
              style = "font-weight: normal; cursor: pointer;",
              tags$input(
                type = "radio", 
                name = "modalitiesChoice", 
                value = "auto_exclude",
                checked = if(default_choice == "auto_exclude") "checked" else NULL,
                style = "margin-right: 8px;"
              ),
              automaticOption
            )
          ),
          div(
            style = "margin: 8px 0;",
            tags$label(
              style = "font-weight: normal; cursor: pointer;",
              tags$input(
                type = "radio", 
                name = "modalitiesChoice", 
                value = "keep_all",
                style = "margin-right: 8px;"
              ),
              keepAllOption
            )
          ),
          div(
            style = "margin: 8px 0;",
            tags$label(
              style = "font-weight: normal; cursor: pointer;",
              tags$input(
                type = "radio", 
                name = "modalitiesChoice", 
                value = "cancel",
                style = "margin-right: 8px;"
              ),
              cancelOption
            )
          )
        )
      ),
      
      # Additional warning information based on selection
      div(
        id = "modalitiesWarning",
        style = "display: none; background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; margin-top: 10px; border-radius: 3px;",
        icon("exclamation-triangle", style = "color: #721c24; margin-right: 5px;"),
        strong(warningAttention, " : "), 
        span(style = "color: #721c24;", keepAllModalitiesWarning)
      )
    ),
    
    easyClose = FALSE,
    footer = tagList(
      tags$button(
        confirmLabel,
        id = "modalitiesChoiceConfirm",
        class = "btn btn-primary",
        onclick = paste0("
          var choice = document.querySelector('input[name=\"modalitiesChoice\"]:checked');
          var selectedValue = choice ? choice.value : null;
          
          if (!selectedValue) {
            alert('", safe_select_message, "');
            return;
          }
          
          $('#shiny-modal').modal('hide');
          
          Shiny.setInputValue('modalitiesChoiceResult', {
            choice: selectedValue,
            variable: '", safe_variable_name, "',
            callbackId: '", safe_callback_id, "',
            timestamp: Date.now()
          }, {priority: 'event'});
        ")
      ),
      tags$button(cancelLabel, id = "modalitiesChoiceCancel", class = "btn btn-default", onclick = "$('#shiny-modal').modal('hide');")
    ),
    
    # JavaScript to show/hide warning with localized messages
    tags$script(HTML(paste0("
      $(document).on('change', 'input[name=\"modalitiesChoice\"]', function() {
        if ($(this).val() === 'keep_all') {
          $('#modalitiesWarning').show();
        } else {
          $('#modalitiesWarning').hide();
        }
      });
    "))),
    
    # Add keyboard navigation
    addKeyboardNavigation("modalitiesChoiceConfirm", "modalitiesChoiceCancel")
  ))
  
  debugLog("MODALITIES_DIALOG: Choice dialog displayed with keyboard navigation", "USER_OPERATION")
}
# ============================================================================
# Dialog type 2 - numeric variable conversion choice
# ============================================================================

showNumericConversionDialog <- function(variable_name, validation_result, complexity_info, 
                                        labels, session, callback_id) {
  
  debugLog(paste("NUMERIC_DIALOG: Three-threshold version for", variable_name), "USER_OPERATION")
  
  unique_count <- complexity_info$modality_count
  var_num <- extractVariableNumber(callback_id)
  
  # Get thresholds
  low_threshold <- USER_CHOICE_SETTINGS$numeric_conversion_threshold  # 40
  high_threshold <- if(!is.null(USER_CHOICE_SETTINGS$numeric_conversion_threshold_high)) {
    USER_CHOICE_SETTINGS$numeric_conversion_threshold_high  # 120
  } else {
    120  # Fallback value
  }
  
  # PROPER localization using the existing safe system
  modalTitle <- safeGetUILabel("numericConversionDialogTitle", labels, "Numeric Variable Conversion")
  variableLabel <- safeGetUILabel("variable", labels, "Variable")
  uniqueValuesLabel <- safeGetUILabel("uniqueValuesLabel", labels, "Unique values")
  conversionPreviewLabel <- safeGetUILabel("conversionPreviewLabel", labels, "Conversion Preview")
  chooseActionLabel <- safeGetUILabel("chooseActionLabel", labels, "Choose your preferred action:")
  confirmLabel <- safeGetUILabel("userChoiceConfirmLabel", labels, "Confirm Choice")
  cancelLabel <- safeGetUILabel("userChoiceCancelLabel", labels, "Cancel")
  
  # Three-tier logic based on thresholds
  if (unique_count <= low_threshold) {
    # Zone 1 - ≤ 40 values (conversion recommended)
    behavior_zone <- "recommended"
    
    convertOptionBase <- safeGetUILabel("convertToCategoricalOption", labels, "✓ Convert to categorical variable (Recommended for ≤{threshold} unique values)")
    
    if (grepl("\\{threshold\\}", convertOptionBase)) {
      convertOptionText <- paste0(gsub("\\{threshold\\}", low_threshold, convertOptionBase))
    } else {
      recommendedSuffix <- safeGetUILabel("recommendedOptionSuffix", labels, "(Recommended)")
      convertOptionText <- paste0(convertOptionBase, " (", recommendedSuffix, " pour ≤", low_threshold, " valeurs uniques)")
    }
    
    cancelOptionText <- safeGetUILabel("cancelVariableSelectionOption", labels, "Cancel variable selection")
    
    previewText <- paste(
      safeGetUILabel("conversionPreviewText", labels, "This will treat"),
      unique_count,
      safeGetUILabel("uniqueNumericValuesText", labels, "unique numeric values as separate categories")
    )
    
    default_choice <- "convert"
    disable_conversion <- FALSE
    dialog_style <- "background-color: #d4edda; border: 1px solid #c3e6cb; padding: 10px; margin: 10px 0; border-radius: 3px;"
    
  } else if (unique_count <= high_threshold) {
    # Zone 2 - 41-120 values (conversion discouraged but possible)
    behavior_zone <- "discouraged"
    
    convertOptionBase <- safeGetUILabel("convertToCategorialNotRecommendedOption", labels, "⚠ Convert to categorical variable (Not recommended for {count} values > {threshold})")
    convertOptionText <- gsub("\\{count\\}", unique_count, gsub("\\{threshold\\}", low_threshold, convertOptionBase))
    cancelOptionBase <- safeGetUILabel("cancelVariableSelectionRecommendedOption", labels, "{cancel_text} (Recommended)")
    cancelOptionText <- gsub("\\{cancel_text\\}", safeGetUILabel("cancelVariableSelectionOption", labels, "Cancel variable selection"), cancelOptionBase)
    
    warningTextBase <- safeGetUILabel("numericConversionWarningText", labels, "⚠ Warning: {count} categories will create complex and slow plots.")
    warningText <- gsub("\\{count\\}", unique_count, warningTextBase)
    previewText <- paste0(
      warningText, "\n\n",
      safeGetUILabel("suggestedAlternativesLabel", labels, "Suggested alternatives:"), "\n",
      "• ", safeGetUILabel("createBinsAlternative", labels, "Create bins/ranges using preprocessing"), "\n",
      "• ", safeGetUILabel("useDifferentVariableAlternative", labels, "Use a different grouping variable"), "\n",
      "• ", safeGetUILabel("filterDataAlternative", labels, "Filter data to reduce unique values")
    )
    
    default_choice <- "cancel"
    disable_conversion <- FALSE
    dialog_style <- "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; margin: 10px 0; border-radius: 3px;"
    
  } else {
    # Zone 3 - > 120 values (conversion impossible)
    behavior_zone <- "impossible"
    
    cannotConvertLabel <- safeGetUILabel("cannotConvertLabel", labels, "Cannot convert:")
    tooManyValuesText <- safeGetUILabel("tooManyUniqueValuesText", labels, "Too many unique values for effective categorization")
    
    convertOptionText <- paste0("✗ ", cannotConvertLabel, " ", tooManyValuesText, " (", unique_count, " > ", high_threshold, ")")
    cancelOptionText <- paste0("✓ ", safeGetUILabel("cancelVariableSelectionOption", labels, "Cancel variable selection"), " (Recommandé)")
    
    # Use HTML for proper line breaks in the modal
    previewText <- paste0(
      safeGetUILabel("suggestedAlternativesLabel", labels, "Suggested alternatives:"), "<br>",
      "• ", safeGetUILabel("createBinsAlternative", labels, "Create bins/ranges using preprocessing"), "<br>",
      "• ", safeGetUILabel("useDifferentVariableAlternative", labels, "Use a different grouping variable"), "<br>",
      "• ", safeGetUILabel("filterDataAlternative", labels, "Filter data to reduce unique values")
    )
    
    default_choice <- "cancel"
    disable_conversion <- TRUE
    dialog_style <- "background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 10px; margin: 10px 0; border-radius: 3px;"
  }
  
  debugLog(paste("NUMERIC_DIALOG: Zone", behavior_zone, "for", unique_count, "values"), "USER_OPERATION")
  
  showModal(modalDialog(
    title = div(
      icon("exchange-alt", style = "color: #28a745; margin-right: 10px;"),
      modalTitle
    ),
    
    div(
      h5(strong(variableLabel, " : "), variable_name),
      p(strong(uniqueValuesLabel, " : "), unique_count),
      
      div(
        style = dialog_style,
        h6(conversionPreviewLabel, " :"),
        if(behavior_zone == "recommended") {
          p(previewText)
        } else if(behavior_zone == "discouraged") {
          # Use pre-formatted text for line breaks in discouraged zone
          HTML(gsub("\n", "<br>", previewText))
        } else {
          # Use HTML for impossible zone
          HTML(previewText)
        }
      ),
      
      div(
        h6(chooseActionLabel),
        div(
          style = "margin: 15px 0;",
          div(
            style = "margin: 8px 0;",
            tags$label(
              style = paste0("font-weight: normal; cursor: pointer;", if(disable_conversion) " color: #6c757d;" else ""),
              tags$input(
                type = "radio", 
                name = "numericChoice", 
                value = "convert", 
                checked = if(default_choice == "convert") "checked" else NULL,
                disabled = if(disable_conversion) "disabled" else NULL,
                style = "margin-right: 8px;"
              ),
              convertOptionText
            )
          ),
          div(
            style = "margin: 8px 0;",
            tags$label(
              style = "font-weight: normal; cursor: pointer;",
              tags$input(
                type = "radio", 
                name = "numericChoice", 
                value = "cancel", 
                checked = if(default_choice == "cancel") "checked" else NULL,
                style = "margin-right: 8px;"
              ),
              cancelOptionText
            )
          )
        )
      )
    ),
    
    easyClose = FALSE,
    footer = tagList(
      tags$button(
        confirmLabel,
        id = "numericConversionConfirm",
        class = "btn btn-success", 
        onclick = paste0("
          var choice = document.querySelector('input[name=\"numericChoice\"]:checked');
          var selectedValue = choice ? choice.value : 'cancel';
          
          $('#shiny-modal').modal('hide');
          
          if (selectedValue === 'convert') {
            Shiny.setInputValue('doNumericConversion', {
              variable: '", escapeJavaScriptString(variable_name), "',
              varNum: ", var_num, ",
              timestamp: Date.now()
            }, {priority: 'event'});
          }
        ")
      ),
      tags$button(cancelLabel, id = "numericConversionCancel", class = "btn btn-default", onclick = "$('#shiny-modal').modal('hide');")
    ),
    
    # Add keyboard navigation
    addKeyboardNavigation("numericConversionConfirm", "numericConversionCancel")
  ))
  
  debugLog("NUMERIC_DIALOG: Three-threshold dialog created with keyboard navigation", "USER_OPERATION")
}

# ============================================================================
# Dialog type 3 - high complexity warning
# ============================================================================

# Load complexity warning dialog from separate module for better maintainability
source("modules/complexity_warning_dialog.R", local = FALSE, encoding = "UTF-8")
debugLog("USER_DIALOGS: complexity_warning_dialog.R loaded successfully", "MODULE_LOAD")

# ============================================================================
# Dialog orchestration function
# ============================================================================

# Main function - show intelligent validation dialog
showIntelligentValidationDialog <- function(variable_name, issue_type, validation_result, 
                                            complexity_info, labels, session, callback_id) {
  
  debugLog(paste("ENHANCED_VALIDATION: Showing intelligent dialog for", variable_name, 
                 "- Issue:", issue_type), "VALIDATION_PROCESS")
  
  # Generate recommendation for this case
  recommendation <- determineRecommendedAction(validation_result, complexity_info, issue_type)
  
  # Route to appropriate dialog based on issue type
  switch(issue_type,
         "numeric_variable" = {
           debugLog(paste("ENHANCED_VALIDATION: Calling showNumericConversionDialog for", variable_name), "VALIDATION_PROCESS")
           showNumericConversionDialog(variable_name, validation_result, complexity_info, labels, session, callback_id)
         },
         
         "single_value_modalities" = {
           debugLog(paste("ENHANCED_VALIDATION: Calling showModalitiesChoiceDialog for", variable_name), "VALIDATION_PROCESS")
           showModalitiesChoiceDialog(variable_name, validation_result, recommendation, labels, session, callback_id)
         },
         
         "high_complexity" = {
           debugLog(paste("ENHANCED_VALIDATION: Calling showComplexityWarningDialog for", variable_name), "VALIDATION_PROCESS")
           showComplexityWarningDialog(variable_name, complexity_info, recommendation, labels, session, callback_id)
         },
         
         # Default case - fallback to simple notification
         {
           debugLog(paste("ENHANCED_VALIDATION: Unknown issue type", issue_type, "- using fallback dialog"), "VALIDATION_PROCESS")
           showModal(modalDialog(
             title = "Variable Validation",
             div(
               h4("Variable: ", variable_name),
               p("Issue type: ", issue_type),
               p("Modality count: ", complexity_info$modality_count),
               p("This case is not yet fully implemented.")
             ),
             easyClose = TRUE,
             footer = modalButton("OK")
           ))
         }
  )
  
  debugLog(paste("ENHANCED_VALIDATION: Dialog completed for", variable_name), "VALIDATION_PROCESS")
}

debugLog("USER_DIALOGS: Dialog creation module loaded", "APP_STARTUP")
