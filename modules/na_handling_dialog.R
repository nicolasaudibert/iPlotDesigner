# NA handling dialog module
# Dialog for handling NA values in categorical variables

# Show NA handling dialog with three options and numeric variable warning
showNAHandlingDialog <- function(variable_name, na_count, total_count, 
                                 na_percentage, labels, session, callback_id,
                                 is_numeric = FALSE) {
  
  debugLog(paste("NA_DIALOG: Showing dialog for", variable_name, "with", na_count, "NA values -",
                 "Is numeric:", is_numeric), "VALIDATION_DIALOG")
  
  # Localized strings with safe fallbacks
  title <- safeGetUILabel("naValuesDialogTitle", labels, "Undefined Values (NA) Detected")
  
  # Message template with variable substitution
  message_template <- safeGetUILabel("naValuesMessage", labels, 
                                     "Variable {variable} contains {count} undefined values (NA) ({percentage}% of data)")
  message <- gsub("\\{variable\\}", variable_name, message_template)
  message <- gsub("\\{count\\}", na_count, message)
  message <- gsub("\\{percentage\\}", round(na_percentage, 1), message)
  
  # Option labels
  filter_option <- safeGetUILabel("filterNAOption", labels, 
                                  "✓ Filter rows with undefined values (Recommended)")
  assign_option <- safeGetUILabel("assignCodeOption", labels, 
                                  "Assign custom label for undefined values")
  keep_option <- safeGetUILabel("keepNAOption", labels, 
                                "Keep undefined values as separate category")
  
  custom_label_prompt <- safeGetUILabel("naCustomLabelPrompt", labels, 
                                        "Custom label for undefined values:")
  custom_label_default <- safeGetUILabel("naCustomLabelDefault", labels, "Unknown")
  
  choose_action <- safeGetUILabel("chooseActionLabel", labels, "Choose your preferred action:")
  confirm_label <- safeGetUILabel("userChoiceConfirmLabel", labels, "Confirm Choice")
  cancel_label <- safeGetUILabel("userChoiceCancelLabel", labels, "Cancel")
  
  # Numeric variable warning labels
  numeric_warning_title <- safeGetUILabel("naNumericVariableWarningTitle", labels, 
                                          "⚠ Numeric Variable")
  numeric_warning_message <- safeGetUILabel("naNumericVariableWarningMessage", labels,
                                            "This is a numeric variable. Choosing 'Assign custom label' or 'Keep as category' will convert it to a categorical variable.")
  
  # Build dialog
  showModal(modalDialog(
    title = div(
      icon("exclamation-triangle", style = "color: #856404; margin-right: 10px;"),
      title
    ),
    size = "m",
    
    # Main message about NA values
    div(
      style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 12px; margin-bottom: 16px;",
      strong(message)
    ),
    
    # Numeric variable warning (conditional)
    if(is_numeric) {
      div(
        style = "background-color: #f8d7da; border-left: 4px solid #f5c6cb; padding: 12px; margin-bottom: 16px;",
        icon("info-circle", style = "color: #721c24; margin-right: 5px;"),
        strong(numeric_warning_title), br(),
        span(style = "color: #721c24;", numeric_warning_message)
      )
    } else {
      NULL
    },
    
    h5(choose_action),
    
    # Radio buttons for the three options
    radioButtons(
      "naHandlingChoice",
      NULL,
      choices = setNames(
        c("filter_na", "assign_code", "keep_na"),
        c(filter_option, assign_option, keep_option)
      ),
      selected = "filter_na"
    ),
    
    # Conditional text input for custom label
    conditionalPanel(
      condition = "input.naHandlingChoice == 'assign_code'",
      textInput(
        "naCustomLabel",
        custom_label_prompt,
        value = custom_label_default,
        placeholder = custom_label_default
      )
    ),
    
    footer = tagList(
      actionButton("confirmNAHandling", confirm_label, class = "btn-primary"),
      actionButton("cancelNAHandling", cancel_label, class = "btn-default")
    ),
    
    easyClose = FALSE,
    
    # Add keyboard navigation
    addKeyboardNavigation("confirmNAHandling", "cancelNAHandling")
  ))
  
  debugLog("NA_DIALOG: Dialog displayed with keyboard navigation enabled", "VALIDATION_DIALOG")
}

debugLog("NA_HANDLING_DIALOG: NA handling dialog module loaded", "MODULE_LOAD")
