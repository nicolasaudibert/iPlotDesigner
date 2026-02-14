# Code Handlers Common Module
# Common functions and utilities for code generation handlers

# Generic copy code handler
createCopyCodeHandler <- function(plotType, codeGenerator) {
  function(input, values, labels) {
    code <- codeGenerator(input, values, labels)
    
    # Use cached labels to prevent cascades
    codeCopiedNotification <- safeGetUILabel("codeCopiedNotification", labels, "Code copied to clipboard!")
    clipboardError <- safeGetUILabel("clipboardError", labels, "Failed to copy to clipboard. Please select and copy manually.")
    
    tryCatch({
      clipr::write_clip(code)
      showNotification(codeCopiedNotification, type = "success", duration = 2)
    }, error = function(e) {
      showNotification(clipboardError, type = "error", duration = 3)
    })
  }
}

# Generic download handler creator
createDownloadHandler <- function(plotType, codeGenerator) {
  downloadHandler(
    filename = function() {
      generateIntelligentFilename(paste0(plotType, "_script"), input, ".R", labels)
    },
    content = function(file) {
      writeLines(codeGenerator(input, values, labels), file)
    }
  )
}

debugLog("CODE_HANDLERS_COMMON: Common code handler functions loaded", "CODE_INIT")
