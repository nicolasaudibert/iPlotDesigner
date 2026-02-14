# Complexity Warning Dialog Module
# Complex variable validation dialog with custom filtering interface
# Extracted from user_dialogs.R for better maintainability

# ============================================================================
# Dialog type 3 - high complexity warning (extracted module)
# ============================================================================

showComplexityWarningDialog <- function(variable_name, complexity_info, recommendation, 
                                        labels, session, callback_id) {
  
  debugLog(paste("COMPLEXITY_DIALOG: Showing complexity warning for", variable_name), "USER_OPERATION")
  
  modality_count <- complexity_info$modality_count
  complexity_level <- getComplexityLevel(modality_count)
  performance_impact <- estimatePerformanceImpact(modality_count)
  
  # Get localized labels
  modalTitle <- safeGetUILabel("complexityWarningDialogTitle", labels, "Variable Complexity Warning")
  variableLabel <- safeGetUILabel("variable", labels, "Variable")
  chooseActionLabel <- safeGetUILabel("chooseActionLabel", labels, "Choose your preferred action:")
  confirmLabel <- safeGetUILabel("userChoiceConfirmLabel", labels, "Confirm Choice")
  cancelLabel <- safeGetUILabel("userChoiceCancelLabel", labels, "Cancel")
  
  # Build options with proper placeholder replacement
  smartFilterBase <- safeGetUILabel("applySmartFilteringOption", labels, "✓ Apply smart filtering (Keep top {n} most frequent, Recommended)")
  smartFilterOption <- gsub("\\{n\\}", SMART_FILTERING_OPTIONS$top_n_default, smartFilterBase)
  
  customFilterOption <- safeGetUILabel("applyCustomFilteringOption", labels, "Apply custom filtering (Choose threshold)")
  
  # Use localized performance level
  performance_level_localized <- getLocalizedPerformanceLevel(performance_impact$performance_level, labels)
  continueAllBase <- safeGetUILabel("continueWithAllModalitiesOption", labels, "Continue with all modalities (Performance impact: {level})")
  continueAllOption <- gsub("\\{level\\}", performance_level_localized, continueAllBase)
  
  cancelOption <- safeGetUILabel("cancelVariableSelectionOption", labels, "Cancel variable selection")
  
  # Determine default choice
  default_choice <- "smart_filter"  # Always default to smart filter for complex variables
  
  # Create performance impact display with color coding
  performance_color <- switch(performance_impact$performance_level,
                              "good" = "#28a745",
                              "acceptable" = "#ffc107", 
                              "slow" = "#fd7e14",
                              "very_slow" = "#dc3545",
                              "#6c757d"
  )
  
  performance_icon <- switch(performance_impact$performance_level,
                             "good" = "check-circle",
                             "acceptable" = "exclamation-circle",
                             "slow" = "clock",
                             "very_slow" = "times-circle",
                             "question-circle"
  )
  
  # Get localized complexity level
  complexity_level_localized <- getLocalizedComplexityLevel(complexity_level, labels)
  
  # Get localized performance labels
  performanceImpactTitle <- safeGetUILabel("performanceImpactTitle", labels, "Performance Impact")
  estimatedTimeLabel <- safeGetUILabel("estimatedUITime", labels, "Estimated UI response time")
  domElementsLabel <- safeGetUILabel("domElementsCount", labels, "UI elements to render")
  memoryUsageLabel <- safeGetUILabel("memoryUsageEstimate", labels, "Memory usage estimate")
  complexityLevelLabel <- safeGetUILabel("complexityLevel", labels, "Complexity level")
  
  # Use simple modality count display without placeholder
  modalityCountLabel <- safeGetUILabel("modalityCountLabel", labels, "Modalities")
  
  complexity_display <- div(
    style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 10px; margin: 10px 0; border-radius: 3px;",
    h6(icon("tachometer-alt"), " ", performanceImpactTitle),
    div(
      p(
        strong(complexityLevelLabel, " : "), 
        span(style = paste0("color: ", performance_color, "; font-weight: bold;"), 
             complexity_level_localized), br(),
        strong(modalityCountLabel, " : "), modality_count, br(),
        icon(performance_icon, style = paste0("color: ", performance_color)),
        strong(" ", estimatedTimeLabel, " : "), 
        span(style = paste0("color: ", performance_color), 
             paste0(round(performance_impact$estimated_ui_time, 1), "s")), br(),
        strong(domElementsLabel, " : "), performance_impact$dom_elements, br(),
        strong(memoryUsageLabel, " : "), round(performance_impact$memory_usage_mb, 1), " MB"
      )
    )
  )
  
  # Get localized custom filtering labels
  customFilteringTitle <- safeGetUILabel("customFilteringTitle", labels, "Custom Filtering Options")
  keepTopNLabel <- safeGetUILabel("keepTopNLabel", labels, "Keep top N modalities")
  minimumFrequencyLabel <- safeGetUILabel("minimumFrequencyLabel", labels, "Minimum frequency")
  performanceWarningText <- safeGetUILabel("performanceWarningText", labels, "Plots may be very slow to generate and difficult to read with this many modalities")
  pleaseSelectOptionMessage <- safeGetUILabel("selectOptionPrompt", labels, "Please select an option")
  
  # Safe variable name and callback ID for JavaScript
  safe_variable_name <- escapeJavaScriptString(variable_name)
  safe_callback_id <- escapeJavaScriptString(callback_id)
  safe_select_message <- escapeJavaScriptString(pleaseSelectOptionMessage)
  
  # Get modality counts for JavaScript (with fallback if not available)
  modality_counts_for_js <- tryCatch({
    # Try to get counts from the calling context
    if(exists("validation_result", envir = parent.frame()) && 
       !is.null(parent.frame()$validation_result$modalityCounts)) {
      parent.frame()$validation_result$modalityCounts
    } else {
      # Fallback: create dummy counts based on modality_count
      dummy_counts <- rep(10, modality_count)  # Assume 10 observations per modality
      names(dummy_counts) <- paste0("Modality_", 1:modality_count)
      dummy_counts
    }
  }, error = function(e) {
    # Ultimate fallback
    dummy_counts <- rep(10, modality_count)
    names(dummy_counts) <- paste0("Modality_", 1:modality_count)
    dummy_counts
  })
  
  # Create the modal dialog
  tryCatch({
    showModal(modalDialog(
      title = div(
        icon("exclamation-triangle", style = "color: #ffc107; margin-right: 10px;"),
        modalTitle
      ),
      
      div(
        h5(strong(variableLabel, " : "), variable_name),
        p(strong(modalityCountLabel, " : "), modality_count),
        
        # Complexity and performance assessment
        complexity_display,
        
        # Filtering options
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
                  name = "complexityChoice", 
                  value = "smart_filter",
                  checked = if(default_choice == "smart_filter") "checked" else NULL,
                  style = "margin-right: 8px;"
                ),
                smartFilterOption
              )
            ),
            div(
              style = "margin: 8px 0;",
              tags$label(
                style = "font-weight: normal; cursor: pointer;",
                tags$input(
                  type = "radio", 
                  name = "complexityChoice", 
                  value = "custom_filter",
                  style = "margin-right: 8px;"
                ),
                customFilterOption
              )
            ),
            div(
              style = "margin: 8px 0;",
              tags$label(
                style = "font-weight: normal; cursor: pointer;",
                tags$input(
                  type = "radio", 
                  name = "complexityChoice", 
                  value = "continue_all",
                  style = "margin-right: 8px;"
                ),
                continueAllOption
              )
            ),
            div(
              style = "margin: 8px 0;",
              tags$label(
                style = "font-weight: normal; cursor: pointer;",
                tags$input(
                  type = "radio", 
                  name = "complexityChoice", 
                  value = "cancel",
                  style = "margin-right: 8px;"
                ),
                cancelOption
              )
            )
          )
        ),
        
        # Custom filtering options (shown when custom_filter is selected) - LOCALIZED INTERFACE
        div(
          id = "customFilteringOptions",
          style = "display: none; border: 1px solid #ddd; padding: 10px; margin-top: 10px; border-radius: 3px; background-color: #f8f9fa;",
          h6(customFilteringTitle, " :"),
          
          # Radio buttons for filtering method - LOCALIZED
          div(
            style = "margin: 10px 0;",
            h6(safeGetUILabel("filteringMethodTitle", labels, "Filtering method")),
            div(
              style = "margin: 8px 0;",
              tags$label(
                style = "font-weight: normal; cursor: pointer; display: block; margin: 5px 0;",
                tags$input(
                  type = "radio", 
                  name = "filteringMethod", 
                  value = "top_n",
                  checked = "checked",
                  style = "margin-right: 8px;"
                ),
                safeGetUILabel("topNFilteringMethodOption", labels, "Keep the N most frequent modalities")
              )
            ),
            div(
              style = "margin: 8px 0;",
              tags$label(
                style = "font-weight: normal; cursor: pointer; display: block; margin: 5px 0;",
                tags$input(
                  type = "radio", 
                  name = "filteringMethod", 
                  value = "min_freq",
                  style = "margin-right: 8px;"
                ),
                safeGetUILabel("minFreqFilteringMethodOption", labels, "Keep modalities with minimum frequency")
              )
            ),
            div(
              style = "margin: 8px 0;",
              tags$label(
                style = "font-weight: normal; cursor: pointer; display: block; margin: 5px 0;",
                tags$input(
                  type = "radio", 
                  name = "filteringMethod", 
                  value = "combined",
                  style = "margin-right: 8px;"
                ),
                safeGetUILabel("combinedFilteringMethodOption", labels, "Combined: top N among those with frequency ≥ threshold")
              )
            )
          ),
          
          # Parameters inputs - LOCALIZED
          div(
            style = "display: flex; gap: 15px; margin-top: 10px;",
            div(
              id = "topNContainer",
              style = "flex: 1;",
              tags$label(safeGetUILabel("numberOfModalitiesLabel", labels, "Number of modalities (N)")),
              tags$input(
                type = "number", 
                id = "customTopN", 
                value = SMART_FILTERING_OPTIONS$top_n_default,
                min = 5, 
                max = min(200, modality_count), 
                step = 5,
                style = "width: 100%; margin-top: 5px;"
              )
            ),
            div(
              id = "minFreqContainer",
              style = "flex: 1; opacity: 0.5;",
              tags$label(safeGetUILabel("minimumFrequencyThresholdLabel", labels, "Minimum frequency threshold")),
              tags$input(
                type = "number", 
                id = "customMinFreq", 
                value = SMART_FILTERING_OPTIONS$min_frequency_default,
                min = 2, 
                max = 50, 
                step = 1,
                style = "width: 100%; margin-top: 5px;"
              )
            )
          ),
          
          # Live preview - LOCALIZED
          div(
            id = "filteringPreview",
            style = "margin-top: 10px; padding: 8px; background-color: #e9ecef; border-radius: 3px;",
            span(id = "previewText", safeGetUILabel("defaultFilteringPreviewText", labels, "Preview: The 50 most frequent modalities will be kept (50/total)"))
          )
        ),
        
        # Performance warning (shown when continue_all is selected)
        div(
          id = "performanceWarning",
          style = "display: none; background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 8px; margin-top: 10px; border-radius: 3px;",
          icon("exclamation-triangle", style = "color: #721c24; margin-right: 5px;"),
          strong(performanceImpactTitle, " : "), 
          span(style = "color: #721c24;", performanceWarningText)
        )
      ),
      
      easyClose = FALSE,
      easyClose = FALSE,
      footer = tagList(
        tags$button(
          confirmLabel,
          id = "complexityChoiceConfirm",
          class = "btn btn-warning",
          onclick = paste0("
            var choice = document.querySelector('input[name=\"complexityChoice\"]:checked');
            var selectedValue = choice ? choice.value : null;
            
            if (!selectedValue) {
              alert('", safe_select_message, "');
              return;
            }
            
            var customParams = null;
            if (selectedValue === 'custom_filter') {
              var filteringMethod = $('input[name=\"filteringMethod\"]:checked').val() || 'top_n';
              customParams = {
                method: filteringMethod,
                topN: parseInt(document.getElementById('customTopN').value) || ", SMART_FILTERING_OPTIONS$top_n_default, ",
                minFreq: parseInt(document.getElementById('customMinFreq').value) || ", SMART_FILTERING_OPTIONS$min_frequency_default, "
              };
            }
            
            $('#shiny-modal').modal('hide');
            
            Shiny.setInputValue('complexityChoiceResult', {
              choice: selectedValue,
              variable: '", safe_variable_name, "',
              callbackId: '", safe_callback_id, "',
              customParams: customParams,
              timestamp: Date.now()
            }, {priority: 'event'});
          ")
        ),
        tags$button(cancelLabel, id = "complexityChoiceCancel", class = "btn btn-default", onclick = "$('#shiny-modal').modal('hide');")
      ),
      
      # JavaScript to handle custom filtering interface
      tags$script(HTML(paste0("
        $(document).ready(function() {
          // Modality counts data (passed from R) - SAFE JSON
          var modalityCounts = ", jsonlite::toJSON(as.list(modality_counts_for_js), auto_unbox = TRUE), ";
          var totalModalities = ", modality_count, ";
          
          // Calculate how many modalities would be kept
          function calculateKeptModalities(method, topN, minFreq) {
            var counts = Object.values(modalityCounts || {});
            var keptCount = 0;
            
            if (method === 'top_n') {
              keptCount = Math.min(topN, totalModalities);
            } else if (method === 'min_freq') {
              keptCount = counts.filter(function(count) { return count >= minFreq; }).length;
            } else if (method === 'combined') {
              var freqFiltered = counts.filter(function(count) { return count >= minFreq; });
              keptCount = Math.min(topN, freqFiltered.length);
            }
            
            return keptCount;
          }
          
          // Update preview text - SIMPLIFIED LOCALIZATION
          function updateFilteringPreview() {
            var method = $('input[name=\"filteringMethod\"]:checked').val();
            var topN = parseInt($('#customTopN').val()) || ", SMART_FILTERING_OPTIONS$top_n_default, ";
            var minFreq = parseInt($('#customMinFreq').val()) || ", SMART_FILTERING_OPTIONS$min_frequency_default, ";
            
            // Calculate how many will be kept
            var keptCount = calculateKeptModalities(method, topN, minFreq);
            
            var previewText = '';
            
            // SIMPLIFIED preview templates - NO COMPLEX ESCAPING
            if (method === 'top_n') {
              previewText = 'Preview: The ' + topN + ' most frequent modalities will be kept (' + keptCount + '/' + totalModalities + ')';
            } else if (method === 'min_freq') {
              previewText = 'Preview: Modalities with at least ' + minFreq + ' observations will be kept (' + keptCount + '/' + totalModalities + ')';
            } else if (method === 'combined') {
              previewText = 'Preview: The ' + topN + ' most frequent modalities among those with >= ' + minFreq + ' observations (' + keptCount + '/' + totalModalities + ')';
            }
            
            // Color-code based on kept count
            var previewElement = $('#previewText');
            previewElement.text(previewText);
            
            // Add visual feedback based on kept count
            if (keptCount === 0) {
              previewElement.css('color', '#dc3545'); // Red
            } else if (keptCount < 5) {
              previewElement.css('color', '#fd7e14'); // Orange
            } else if (keptCount <= 20) {
              previewElement.css('color', '#28a745'); // Green
            } else {
              previewElement.css('color', '#6c757d'); // Gray
            }
          }
          
          // Event handlers
          $(document).on('change', 'input[name=\"complexityChoice\"]', function() {
            var choice = $(this).val();
            
            // Hide all conditional panels
            $('#customFilteringOptions').hide();
            $('#performanceWarning').hide();
            
            // Show relevant panel
            if (choice === 'custom_filter') {
              $('#customFilteringOptions').show();
              updateFilteringPreview();
            } else if (choice === 'continue_all') {
              $('#performanceWarning').show();
            }
          });
          
          // Handle filtering method radio buttons
          $(document).on('change', 'input[name=\"filteringMethod\"]', function() {
            var method = $(this).val();
            
            if (method === 'top_n') {
              $('#topNContainer').css('opacity', '1');
              $('#minFreqContainer').css('opacity', '0.5');
            } else if (method === 'min_freq') {
              $('#topNContainer').css('opacity', '0.5');
              $('#minFreqContainer').css('opacity', '1');
            } else if (method === 'combined') {
              $('#topNContainer').css('opacity', '1');
              $('#minFreqContainer').css('opacity', '1');
            }
            
            updateFilteringPreview();
          });
          
          // Handle input changes
          $(document).on('input', '#customTopN, #customMinFreq', function() {
            updateFilteringPreview();
          });
        });
      "))),
      
      # Add keyboard navigation
      addKeyboardNavigation("complexityChoiceConfirm", "complexityChoiceCancel")
    ))
    
    debugLog("COMPLEXITY_DIALOG: Complexity warning dialog displayed with keyboard navigation", "USER_OPERATION")
    
  }, error = function(e) {
    debugLog(paste("COMPLEXITY_DIALOG: Error in showModal:", e$message), "USER_OPERATION")
    
    # Simple fallback modal
    showModal(modalDialog(
      title = safeGetUILabel("complexityWarningDialogTitle", labels, "Variable Complexity Warning"),
      div(
        h4(safeGetUILabel("variable", labels, "Variable"), " : ", variable_name),
        p(safeGetUILabel("modalityCountLabel", labels, "Modalities"), " ", modality_count, " ", safeGetUILabel("modalitiesLabel", labels, "modalities"), "."),
        p(safeGetUILabel("continueQuestionLabel", labels, "Do you want to continue?"))
      ),
      easyClose = TRUE,
      footer = modalButton(safeGetUILabel("userChoiceCancelLabel", labels, "Cancel"))
    ))
  })
}

debugLog("COMPLEXITY_WARNING_DIALOG: Complex variable dialog module loaded", "APP_STARTUP")
