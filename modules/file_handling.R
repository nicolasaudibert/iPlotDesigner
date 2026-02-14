# File handling module with data filtering and preview functionality

# Enhanced file upload handling with advanced options support
observeEvent(input$datafile, {
  req(input$datafile)
  ext <- tools::file_ext(input$datafile$datapath)
  
  # Check if advanced options are enabled
  useAdvancedOptions <- !is.null(input$advancedImportOptions) && input$advancedImportOptions
  
  if (useAdvancedOptions) {
    # Use advanced import with preview dialog
    handleAdvancedFileImport(input$datafile, ext)
  } else {
    # Use standard import (existing behavior)
    handleStandardFileImport(input$datafile, ext)
  }
})

# Standard file import (existing behavior preserved)
handleStandardFileImport <- function(datafile, ext) {
  tryCatch({
    if(tolower(ext) %in% c("xlsx", "xls")) {
      values$filedata <- read_excel(datafile$datapath)
    } else if(tolower(ext) == "ods") {
      values$filedata <- read_ods(datafile$datapath)
    } else if(tolower(ext) == "csv") {
      values$filedata <- read_csv(datafile$datapath)
    } else {
      values$filedata <- read_tsv(datafile$datapath)
    }
    
    values$original_filename <- datafile$name
    values$file_extension <- ext
    
    # Preserve original data separately - never modified
    values$originalData <- values$filedata
    values$filteredData <- values$filedata
    
    debugLog("FILE_HANDLING: Standard import - Original data preserved and filtered data initialized", "DATA_PRESERVE")
    
    # Reset variable properties and operations history
    resetVariablePropertiesAndHistory()
    
  }, error = function(e) {
    showStandardErrorModal(e$message)
  })
}

# Advanced file import with preview dialog
handleAdvancedFileImport <- function(datafile, ext) {
  # Store file information for preview dialog
  values$selectedFile <- datafile$datapath
  values$original_filename <- datafile$name
  values$inputFileExtension <- ext
  
  # Initialize preview parameters
  initializePreviewParameters(ext)
  
  # Show preview dialog
  displayLoadedFilePreviewDialog()
}

# Initialize preview parameters based on file type
initializePreviewParameters <- function(ext) {
  values$columnSeparator <- "\t"
  values$selectedFileEncoding <- "UTF-8"
  values$naStringsFileImport <- ""
  values$nLinesDisplayedInPreview <- nRowsDisplayedInFilePreview
  values$selectedSheetIndex <- 1
  values$availableSheetNames <- NULL
  values$associatedMessage <- NULL
  
  # Set default parameters based on file extension
  if(tolower(ext) %in% c("xlsx", "xls")) {
    values$loadedFileType <- "xlsx"
  } else if(tolower(ext) == "ods") {
    values$loadedFileType <- "ods"
  } else if(tolower(ext) == "csv") {
    values$loadedFileType <- "txt"
    values$columnSeparator <- ","
  } else if(tolower(ext) == "tsv") {
    values$loadedFileType <- "txt"
    values$columnSeparator <- "\t"
  } else {
    values$loadedFileType <- "txt"
  }
}

# Display file preview dialog (adapted from file_preview_app)
displayLoadedFilePreviewDialog <- function() {
  # Get localized constants
  availableColumnSeparatorsDisplay <- c(
    safeGetUILabel("columnSeparatorTabulation", labels, "tabulation (\t)"),
    safeGetUILabel("columnSeparatorSpace", labels, "space"),
    safeGetUILabel("columnSeparatorComma", labels, "comma (,)"),
    safeGetUILabel("columnSeparatorSemicolon", labels, "semicolon (;)")
  )
  
  availableEncodingsDisplay <- c(
    "Unicode (UTF-8)",
    "Windows (latin-1)", 
    "Windows (latin-9)", 
    "Mac OS (MacRoman)", 
    safeGetUILabel("encodingUnknown", labels, "unknown")
  )
  
  # Initialize UI outputs
  output$inputFilePreview <- renderTable(NULL)
  output$inputFilePreviewMessage <- renderText(NULL)
  
  # Create main parameter selector UI
  output$loadedFilePreviewMainParameterSelectorUI <- renderUI({
    if(values$loadedFileType == "txt") {
      selectInput(
        "loadedFilePreviewMainParameterSelector", 
        safeGetUILabel("columnSeparatorSelectorLabel", labels, "Column separator"), 
        availableColumnSeparatorsDisplay, 
        selected = getSelectedSeparatorDisplay()
      )
    } else {
      # For Excel/ODS files, get sheet names
      if(values$loadedFileType == "xlsx") {
        values$availableSheetNames <- excel_sheets(values$selectedFile)
      } else if(values$loadedFileType == "ods") {
        values$availableSheetNames <- list_ods_sheets(values$selectedFile)
      }
      
      selectInput(
        "loadedFilePreviewMainParameterSelector", 
        safeGetUILabel("sheetSelectorLabel", labels, "Sheet"), 
        values$availableSheetNames
      )
    }
  })
  
  # Create encoding selector (only for text files)
  output$fileEncodingUI <- renderUI({
    if(values$loadedFileType == "txt") {
      selectInput(
        "fileEncoding", 
        safeGetUILabel("fileEncodingLabel", labels, "File encoding"),
        availableEncodingsDisplay,
        selected = "Unicode (UTF-8)"
      )
    }
  })
  
  # Create NA strings input
  output$naStringsFileImportUI <- renderUI({
    textAreaInput(
      "naStringsFileImport", 
      safeGetUILabel("naStringsFileImportLabel", labels, "Undefined values (one code per line)"), 
      "", 
      rows = 3
    )
  })
  
  
  # Show modal dialog
  showModal(
    modalDialog(
      size = "l",
      fluidRow(
        column(6, uiOutput("loadedFilePreviewMainParameterSelectorUI")),
        if(values$loadedFileType == "txt") {
          column(6, uiOutput("fileEncodingUI"))
        } else {
          column(6, NULL)
        }
      ),
      fluidRow(
        column(6, numericInput(
          "nLinesDisplayedInPreview", 
          label = safeGetUILabel("nLinesDisplayedInPreviewLabel", labels, "Maximum number of lines displayed"), 
          value = nRowsDisplayedInFilePreview, 
          min = 5, 
          step = 5
        )),
        column(6, uiOutput("naStringsFileImportUI"))
      ),
      htmlOutput("inputFilePreviewMessage"),
      DT::dataTableOutput("inputFilePreview"),
      title = safeGetUILabel("loadedFilePreviewDialogLabel", labels, "Data file preview"),
      easyClose = FALSE,
      footer = tagList(
        actionButton(
          "cancelLoadedFilePreviewButton", 
          safeGetUILabel("filePreviewCancelButtonLabel", labels, "Cancel")
        ),
        actionButton(
          "validateLoadedFilePreviewButton", 
          safeGetUILabel("filePreviewValidateButtonLabel", labels, "Load"),
          class = "btn-primary"
        )
      ),
      
      # Add keyboard navigation
      addKeyboardNavigation("validateLoadedFilePreviewButton", "cancelLoadedFilePreviewButton")
    )
  )
  
  # Initial preview update
  updateLoadedTextFilePreviewDisplay()
}

# Helper function to get selected separator display
getSelectedSeparatorDisplay <- function() {
  # Create consistent mapping using the same constants as in global_settings.R
  availableColumnSeparatorsDisplay <- c(
    safeGetUILabel("columnSeparatorTabulation", labels, "tabulation (\\t)"),
    safeGetUILabel("columnSeparatorSpace", labels, "space"),
    safeGetUILabel("columnSeparatorComma", labels, "comma (,)"),
    safeGetUILabel("columnSeparatorSemicolon", labels, "semicolon (;)")
  )
  
  # Map current separator to display text
  separator_to_display_map <- setNames(availableColumnSeparatorsDisplay, availableColumnSeparators)
  
  # Return the display text for current separator
  return(separator_to_display_map[[values$columnSeparator]] %||% availableColumnSeparatorsDisplay[1])
}

# Update preview display
updateLoadedTextFilePreviewDisplay <- function() {
  df <- loadDataFilePreview(values$nLinesDisplayedInPreview)
  
  if(!is.null(df)) {
    # Convert to standard data.frame for compatibility
    df <- as.data.frame(df)
    
    # Intelligent column width calculation
    col_widths <- sapply(1:ncol(df), function(i) {
      displayed_rows <- min(values$nLinesDisplayedInPreview, nrow(df))
      content_width <- max(nchar(as.character(df[1:displayed_rows, i])), na.rm = TRUE)
      header_width <- nchar(colnames(df)[i])
      max_width <- max(content_width, header_width, na.rm = TRUE)
      pixel_width <- max_width * 8 + 20
      pmax(80, pmin(pixel_width, 300))
    })
    
    # Build column definitions
    column_defs <- list(list(className = 'dt-center', targets = "_all"))
    for(i in seq_along(col_widths)) {
      column_defs[[length(column_defs) + 1]] <- list(
        width = paste0(col_widths[i], "px"), 
        targets = i - 1
      )
    }
    
    output$inputFilePreview <- DT::renderDataTable(
      df,
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        dom = 't',
        pageLength = 20,
        ordering = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = FALSE,
        columnDefs = column_defs,
        initComplete = JS(
          "function(settings, json) {",
          "this.api().columns.adjust();",
          "}"
        )
      ),
      rownames = FALSE,
      class = "compact"
    )
  } else {
    output$inputFilePreview <- DT::renderDataTable(NULL)
  }
  
  if(!is.null(values$associatedMessage)) {
    output$inputFilePreviewMessage <- renderText(HTML(paste0("<p>", values$associatedMessage, "</p>")))
  } else {
    output$inputFilePreviewMessage <- renderText(NULL)
  }
}

# Load data file for preview (adapted from file_preview_app)
loadDataFilePreview <- function(nLoadedRows) {
  values$associatedMessage <- NULL
  
  df <- tryCatch({
    withCallingHandlers({
      if(values$loadedFileType == "xlsx") {
        nMaxRows <- if(is.null(nLoadedRows)) Inf else nLoadedRows + 1
        df <- read_excel(
          path = values$selectedFile,
          sheet = values$selectedSheetIndex,
          na = values$naStringsFileImport,
          n_max = nMaxRows
        )
      } else if(values$loadedFileType == "ods") {
        nMaxRows <- if(is.null(nLoadedRows)) Inf else nLoadedRows + 1
        df <- read_ods(
          path = values$selectedFile,
          sheet = values$selectedSheetIndex,
          na = values$naStringsFileImport,
          n_max = nMaxRows
        )
      } else if(values$loadedFileType == "txt") {
        nLoadedRows <- if(is.null(nLoadedRows)) Inf else nLoadedRows
        df <- read_delim(
          values$selectedFile,
          delim = values$columnSeparator,
          locale = locale(encoding = values$selectedFileEncoding),
          na = values$naStringsFileImport,
          n_max = nLoadedRows,
          comment = "",
          show_col_types = FALSE
        )
      } else {
        df <- NULL
      }
    }, warning = function(war) {
      values$associatedMessage <- safeGetUILabel("loadedFilePreviewWarningMessage", labels, "Warning while reading file. Check that the display below matches your data file.")
      invokeRestart("muffleWarning")
    })
  }, error = function(err) {
    df <- NULL
    values$associatedMessage <- safeGetUILabel("loadedFilePreviewErrorMessage", labels, "Unable to read file. Try another encoding and/or check that your file format is compatible.")
  })
  
  if(!is.data.frame(df)) df <- NULL
  return(df)
}

# Observer for parameter changes in preview dialog
observeEvent(input$loadedFilePreviewMainParameterSelector, {
  if(!is.null(input$loadedFilePreviewMainParameterSelector)) {
    if(values$loadedFileType == "txt") {
      # Create proper mapping from display text to actual separator using global constants
      availableColumnSeparatorsDisplay <- c(
        safeGetUILabel("columnSeparatorTabulation", labels, "tabulation (\\t)"),
        safeGetUILabel("columnSeparatorSpace", labels, "space"),
        safeGetUILabel("columnSeparatorComma", labels, "comma (,)"),
        safeGetUILabel("columnSeparatorSemicolon", labels, "semicolon (;)")
      )
      
      # Create mapping using the same constants as global_settings.R
      separator_display_map <- setNames(availableColumnSeparators, availableColumnSeparatorsDisplay)
      
      # Find the matching separator
      selected_display <- input$loadedFilePreviewMainParameterSelector
      
      if (selected_display %in% names(separator_display_map)) {
        newSeparator <- separator_display_map[[selected_display]]
        
        if(values$columnSeparator != newSeparator) {
          values$columnSeparator <- newSeparator
          debugLog(paste("SEPARATOR_CHANGE: Changed separator to", newSeparator, "for display", selected_display), "DATA_DEBUG")
          updateLoadedTextFilePreviewDisplay()
        }
      } else {
        debugLog(paste("SEPARATOR_ERROR: Could not find separator for display:", selected_display), "DATA_ERROR")
      }
      
    } else if(values$loadedFileType != "txt" && !is.null(values$availableSheetNames)) {
      # Handle sheet selection for Excel/ODS files
      newSheetIndex <- which(input$loadedFilePreviewMainParameterSelector == values$availableSheetNames)
      if(length(newSheetIndex) > 0 && values$selectedSheetIndex != newSheetIndex) {
        values$selectedSheetIndex <- newSheetIndex
        updateLoadedTextFilePreviewDisplay()
      }
    }
  }
}, priority = 100)

# Observer for number of lines displayed
observeEvent(input$nLinesDisplayedInPreview, {
  if(!is.null(input$nLinesDisplayedInPreview) && values$nLinesDisplayedInPreview != input$nLinesDisplayedInPreview) {
    values$nLinesDisplayedInPreview <- input$nLinesDisplayedInPreview
    updateLoadedTextFilePreviewDisplay()
  }
}, priority = 100)

# Observer for encoding
observeEvent(input$fileEncoding, {
  if(!is.null(input$fileEncoding)) {
    encoding_map <- list(
      "Unicode (UTF-8)" = "UTF-8",
      "Windows (latin-1)" = "latin-1",
      "Windows (latin-9)" = "latin-9",
      "Mac OS (MacRoman)" = "macintosh",
      "unknown" = "unknown"
    )
    
    # Find matching encoding
    for(display_text in names(encoding_map)) {
      if(grepl(display_text, input$fileEncoding, fixed = TRUE)) {
        newEncoding <- encoding_map[[display_text]]
        if(values$selectedFileEncoding != newEncoding) {
          values$selectedFileEncoding <- newEncoding
          updateLoadedTextFilePreviewDisplay()
        }
        break
      }
    }
  }
}, priority = 100)

# Observer for NA strings
observeEvent(input$naStringsFileImport, {
  if(!is.null(input$naStringsFileImport)) {
    naStringsRaw <- input$naStringsFileImport
    naStringsFileImportTmp <- str_split(naStringsRaw, pattern = "\\n", simplify = TRUE)
    if(!"" %in% naStringsFileImportTmp) {
      naStringsFileImportTmp <- c("", naStringsFileImportTmp)
    }
    if(!identical(values$naStringsFileImport, naStringsFileImportTmp)) {
      values$naStringsFileImport <- naStringsFileImportTmp
      updateLoadedTextFilePreviewDisplay()
    }
  }
}, priority = 100)

# Observers for validation and cancel buttons
observeEvent(input$validateLoadedFilePreviewButton, {
  removeModal()
  df <- loadDataFilePreview(NULL)
  postProcessLoadedFile(df)
})

observeEvent(input$cancelLoadedFilePreviewButton, {
  removeModal()
  # Reset file input to allow re-selection
  reset("datafile")
})

# Post-process loaded file (common for both standard and advanced import)
postProcessLoadedFile <- function(df) {
  if(!is.null(df)) {
    # Remove columns with empty names
    dfColnames <- colnames(df)
    emptyColnamesIndices <- which(dfColnames == "")
    if(length(emptyColnamesIndices) > 0) {
      df <- df[, -emptyColnamesIndices]
    }
    
    values$filedata <- df
    values$originalData <- df
    values$filteredData <- df
    
    debugLog("FILE_HANDLING: Advanced import - Data processed and preserved", "DATA_PRESERVE")
    
    # Reset variable properties and operations history
    resetVariablePropertiesAndHistory()
  }
}

# Helper function to reset variable properties and operations history
resetVariablePropertiesAndHistory <- function() {
  # Reset variable properties
  values$var1_modalities <- NULL
  values$var1_selected <- NULL
  values$var1_colors <- NULL
  values$var1_order <- NULL
  values$var2_modalities <- NULL
  values$var2_selected <- NULL
  values$var2_colors <- NULL
  values$var2_order <- NULL
  values$var3_modalities <- NULL
  values$var3_selected <- NULL
  values$var3_colors <- NULL
  values$var3_order <- NULL
  
  # Clear user choice filtering state
  values$user_applied_filtering <- list()
  
  # Reset user operations history when new data is loaded
  tryCatch({
    initializeUserOperationsHistory(values)
    debugLog("FILE_HANDLING: User operations history reset for new dataset", "USER_OPERATION")
  }, error = function(e) {
    debugLog(paste("FILE_HANDLING: Error resetting user operations:", e$message), "USER_OPERATION")
  })
}

# Show standard error modal
showStandardErrorModal <- function(errorMessage) {
  errorTitle <- safeGetUILabel("errorTitle", labels, "Error")
  fileReadError <- safeGetUILabel("fileReadError", labels, "Error reading file: ")
  okLabel <- safeGetUILabel("okLabel", labels, "OK")
  cancelLabel <- safeGetUILabel("cancelLabel", labels, "Cancel")
  
  showModal(modalDialog(
    title = errorTitle,
    p(paste(fileReadError, errorMessage)),
    easyClose = FALSE,
    footer = tagList(
      actionButton("standardErrorOk", okLabel, class = "btn-primary"),
      actionButton("standardErrorCancel", cancelLabel, class = "btn-default")
    ),
    
    # Add keyboard navigation
    addKeyboardNavigation("standardErrorOk", "standardErrorCancel")
  ))
}

# Observer to handle error dialog OK button
# Output conditions (unchanged)
output$fileUploaded <- reactive({
  !is.null(values$filedata)
})
outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)

output$validVariablesSelected <- reactive({
  !is.null(input$dependentVar) && input$dependentVar != "" &&
  !is.null(input$independentVar1) && input$independentVar1 != ""
})
outputOptions(output, 'validVariablesSelected', suspendWhenHidden = FALSE)

# Data filtering table (unchanged)
output$filteringDT <- DT::renderDataTable({
  req(values$filedata)
  
  # Use cached labels for DataTable
  searchLabel <- safeGetUILabel("dtSearchLabel", labels, "Search:")
  lengthMenuLabel <- safeGetUILabel("dtLengthMenuLabel", labels, "Show _MENU_ entries")
  infoLabel <- safeGetUILabel("dtInfoLabel", labels, "Showing _START_ to _END_ of _TOTAL_ entries")
  firstLabel <- safeGetUILabel("dtFirstLabel", labels, "First")
  lastLabel <- safeGetUILabel("dtLastLabel", labels, "Last")
  nextLabel <- safeGetUILabel("dtNextLabel", labels, "Next")
  previousLabel <- safeGetUILabel("dtPreviousLabel", labels, "Previous")
  
  datatable(
    values$filedata,
    filter = "top",
    options = list(
      pageLength = 5,
      autoWidth = TRUE,
      language = list(
        search = searchLabel,
        lengthMenu = lengthMenuLabel,
        info = infoLabel,
        paginate = list(
          first = firstLabel,
          last = lastLabel,
          `next` = nextLabel,
          previous = previousLabel
        )
      )
    )
  )
})

# Enhanced filtered data management with preservation of original data (unchanged)
observeEvent(input$filteringDT_rows_all, {
  req(values$originalData)
  
  # Apply DataTable filtering first (from original data)
  dt_filtered_data <- values$originalData[input$filteringDT_rows_all, ]
  
  # Apply any user choice filtering on top of DataTable filtering
  final_filtered_data <- applyUserChoiceFiltering(dt_filtered_data, values)
  
  values$filteredData <- final_filtered_data
  
  debugLog(paste("FILE_HANDLING: Applied filtering - Original:", nrow(values$originalData), 
                 "DT filtered:", nrow(dt_filtered_data), "Final:", nrow(values$filteredData)), "DATA_FILTER")
                 
})

# Helper function to apply user choice filtering without modifying original data (unchanged)
applyUserChoiceFiltering <- function(data, values) {
  tryCatch({
    result_data <- data
    
    if(!is.null(values$user_applied_filtering) && length(values$user_applied_filtering) > 0) {
      for(var_name in names(values$user_applied_filtering)) {
        filter_info <- values$user_applied_filtering[[var_name]]
        
        if(!is.null(filter_info$selected_modalities) && var_name %in% names(result_data)) {
          original_rows <- nrow(result_data)
          result_data <- result_data %>%
            filter(!!sym(var_name) %in% filter_info$selected_modalities)
          
          debugLog(paste("USER_FILTERING: Applied filtering for", var_name, 
                         "- From", original_rows, "to", nrow(result_data), "rows"), "DATA_FILTER")
        }
      }
    }
    
    return(result_data)
    
  }, error = function(e) {
    debugLog(paste("ERROR in applyUserChoiceFiltering:", e$message), "DATA_ERROR")
    return(data)
  })
}

# Export functions (unchanged)
storeUserChoiceFiltering <- function(variable_name, selected_modalities, values) {
  if(is.null(values$user_applied_filtering)) {
    values$user_applied_filtering <- list()
  }
  
  values$user_applied_filtering[[variable_name]] <- list(
    selected_modalities = selected_modalities,
    applied_at = Sys.time()
  )
  
  debugLog(paste("USER_FILTERING: Stored filtering for", variable_name, 
                 "- Keeping", length(selected_modalities), "modalities"), "DATA_FILTER")
}

clearUserChoiceFiltering <- function(variable_name, values) {
  # Defensive checks
  if (is.null(variable_name) || !is.character(variable_name) || variable_name == "") {
    return()
  }
  
  if (is.null(values$user_applied_filtering)) {
    return()
  }
  
  if (!is.list(values$user_applied_filtering)) {
    debugLog("USER_FILTERING: user_applied_filtering is not a list, skipping", "DATA_FILTER")
    return()
  }
  
  if (variable_name %in% names(values$user_applied_filtering)) {
    values$user_applied_filtering[[variable_name]] <- NULL
    debugLog(paste("USER_FILTERING: Cleared filtering for", variable_name), "DATA_FILTER")
  }
}

refreshFilteredData <- function(values) {
  if(!is.null(values$originalData)) {
    current_data <- values$originalData
    values$filteredData <- applyUserChoiceFiltering(current_data, values)
    debugLog(paste("DATA_REFRESH: Refreshed filtered data to", nrow(values$filteredData), "rows"), "DATA_REFRESH")
  }
}

# Export functions to global environment
assign("storeUserChoiceFiltering", storeUserChoiceFiltering, envir = .GlobalEnv)
assign("clearUserChoiceFiltering", clearUserChoiceFiltering, envir = .GlobalEnv)  
assign("refreshFilteredData", refreshFilteredData, envir = .GlobalEnv)
assign("applyUserChoiceFiltering", applyUserChoiceFiltering, envir = .GlobalEnv)

debugLog("FILE_HANDLING: Enhanced file handling with advanced import options loaded", "MODULE_EXPORT")
