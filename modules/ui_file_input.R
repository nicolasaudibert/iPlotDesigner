# UI File Input Module - Enhanced with Advanced Import Options
# File upload section and conditional panel with preview dialog

# Function to create file input section with advanced options checkbox
createFileInputSection <- function(labels) {
  # Cache labels to prevent cascades
  datafileInputLabel <- safeGetUILabel("datafileInputLabel", labels, "Data file")
  browseInputFileLabel <- safeGetUILabel("browseInputFileLabel", labels, "Browse")
  noFileSelectedPrompt <- safeGetUILabel("noFileSelectedPrompt", labels, "No file selected")
  advancedImportOptionsLabel <- safeGetUILabel("advancedImportOptionsLabel", labels, "Advanced data import options")
  advancedImportOptionsTooltip <- safeGetUILabel("advancedImportOptionsTooltip", labels, "Preview data before loading and configure import parameters: column separator, encoding, sheet selection, undefined values")
  
  # Create layout with file input and checkbox
  fluidRow(
    column(3,
      # File input with cached labels
      fileInput(
        "datafile",
        datafileInputLabel,
        accept = c(
          'text/csv',
          'text/tsv',
          'text/tab-separated-values,text/plain',
          'application/vnd.ms-excel',
          'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
          'application/vnd.oasis.opendocument.spreadsheet'
        ),
        buttonLabel = browseInputFileLabel,
        placeholder = noFileSelectedPrompt
      )
    ),
    column(9,
      # Advanced options checkbox with tooltip - disabled by default
      div(style = "margin-top: 25px;",
        div(
          checkboxInput(
            "advancedImportOptions",
            span(
              advancedImportOptionsLabel,
              " ",
              tooltip(
                bs_icon("info-circle"),
                advancedImportOptionsTooltip,
                placement = "bottom"
              )
            ),
            value = FALSE
          )
        )
      )
    )
  )
}

# Function to create the conditional panel wrapper
createFileConditionalPanel <- function(content) {
  conditionalPanel(
    condition = "output.fileUploaded",
    content
  )
}
