# button_observers_core.R - Core functions for button observers
# Common functions and cleanup system

# Global observer storage for proper cleanup to prevent double triggering
if(!exists("button_observers_registry")) {
  button_observers_registry <- list()
}

# Function to cleanup existing observers for a variable to prevent double triggering
cleanupButtonObservers <- function(varPrefix) {
  if(exists("button_observers_registry") && !is.null(button_observers_registry[[varPrefix]])) {
    debugLog(paste("BUTTON_CLEANUP: Destroying existing observers for", varPrefix), "BUTTON_TRACE")
    
    # Destroy all existing observers for this variable
    lapply(button_observers_registry[[varPrefix]], function(obs) {
      if(!is.null(obs) && "Observer" %in% class(obs)) {
        tryCatch(obs$destroy(), error = function(e) {})
      }
    })
    
    # Clear the registry
    button_observers_registry[[varPrefix]] <<- NULL
    debugLog(paste("BUTTON_CLEANUP: Cleared registry for", varPrefix), "BUTTON_TRACE")
  }
}

# Function to create select all/none observers for a variable
createSelectObservers <- function(varNum, varPrefix) {
  # Select All button
  selectAllId <- paste0("var", varNum, "_selectAll")
  observeEvent(input[[selectAllId]], {
    debugLog(paste("BUTTON:", selectAllId, "clicked"), "BUTTON_TRACE")
    isolate({
      modalitiesKey <- paste0("var", varNum, "_modalities")
      current_modalities <- values[[modalitiesKey]]
      if(is.null(current_modalities)) return()
      
      modalityList <- unique(current_modalities)
      varInputId <- paste0("independentVar", varNum)
      
      validation <- tryCatch({
        validateGroupingVariable(values$filteredData, input[[varInputId]])
      }, error = function(e) list(isValid = FALSE, validModalities = character(0)))
      
      validModalities <- if(validation$isValid) validation$validModalities else modalityList
      
      for(modality in modalityList) {
        modalityId <- gsub("[^[:alnum:]]", "_", modality)
        checkId <- paste0("var", varNum, "_check_", modalityId)
        should_select <- modality %in% validModalities
        tryCatch(updateCheckboxInput(session, checkId, value = should_select), error = function(e) {})
      }
    })
  }, ignoreInit = TRUE)
  
  # Select None button
  selectNoneId <- paste0("var", varNum, "_selectNone")
  observeEvent(input[[selectNoneId]], {
    debugLog(paste("BUTTON:", selectNoneId, "clicked"), "BUTTON_TRACE")
    isolate({
      modalitiesKey <- paste0("var", varNum, "_modalities")
      current_modalities <- values[[modalitiesKey]]
      if(is.null(current_modalities)) return()
      
      modalityList <- unique(current_modalities)
      for(modality in modalityList) {
        modalityId <- gsub("[^[:alnum:]]", "_", modality)
        checkId <- paste0("var", varNum, "_check_", modalityId)
        tryCatch(updateCheckboxInput(session, checkId, value = FALSE), error = function(e) {})
      }
    })
  }, ignoreInit = TRUE)
}

# Function to create reordering observers for a variable
createReorderingObservers <- function(varNum, varPrefix) {
  modalitiesKey <- paste0("var", varNum, "_modalities")
  
  observeEvent(values[[modalitiesKey]], {
    if(!is.null(values[[modalitiesKey]])) {
      debugLog(paste("BUTTON_PURE_REACTIVE: Creating", varPrefix, "reordering observers"), "UI_INIT")
      
      # Cleanup existing observers to prevent double triggering
      cleanupButtonObservers(varPrefix)
      
      modalityList <- unique(values[[modalitiesKey]])
      new_observers <- list()
      
      for(modality in modalityList) {
        local({
          mod_name <- modality
          modalityId <- gsub("[^[:alnum:]]", "_", mod_name)
          
          # Up button - pure reactive implementation without triggers
          upButtonId <- paste0("var", varNum, "_up_", modalityId)
          upObserver <- observeEvent(input[[upButtonId]], {
            debugLog(paste("BUTTON_PURE_REACTIVE: Move up clicked for", mod_name, "in", varPrefix), "BUTTON_TRACE")
            
            orderKey <- paste0("var", varNum, "_order")
            currentOrder <- values[[orderKey]]
            
            if(is.null(currentOrder)) {
              currentOrder <- unique(values[[modalitiesKey]])
              values[[orderKey]] <- currentOrder
              debugLog(paste("BUTTON_PURE_REACTIVE: Initialized", varPrefix, "order:", paste(currentOrder, collapse = ", ")), "BUTTON_TRACE")
            }
            
            currentPos <- which(currentOrder == mod_name)[1]
            
            if(!is.na(currentPos) && currentPos > 1) {
              newOrder <- currentOrder
              newOrder[c(currentPos-1, currentPos)] <- newOrder[c(currentPos, currentPos-1)]
              values[[orderKey]] <- newOrder
              
              debugLog(paste("BUTTON_PURE_REACTIVE: Moved", mod_name, "up in", varPrefix, ". New order:", paste(newOrder, collapse = ", ")), "BUTTON_TRACE")
            } else {
              debugLog(paste("BUTTON_PURE_REACTIVE: Cannot move", mod_name, "up (already at top or not found)"), "BUTTON_TRACE")
            }
          }, ignoreInit = TRUE, autoDestroy = FALSE)
          
          # Down button - pure reactive implementation without triggers
          downButtonId <- paste0("var", varNum, "_down_", modalityId)
          downObserver <- observeEvent(input[[downButtonId]], {
            debugLog(paste("BUTTON_PURE_REACTIVE: Move down clicked for", mod_name, "in", varPrefix), "BUTTON_TRACE")
            
            orderKey <- paste0("var", varNum, "_order")
            currentOrder <- values[[orderKey]]
            
            if(is.null(currentOrder)) {
              currentOrder <- unique(values[[modalitiesKey]])
              values[[orderKey]] <- currentOrder
              debugLog(paste("BUTTON_PURE_REACTIVE: Initialized", varPrefix, "order:", paste(currentOrder, collapse = ", ")), "BUTTON_TRACE")
            }
            
            currentPos <- which(currentOrder == mod_name)[1]
            
            if(!is.na(currentPos) && currentPos < length(currentOrder)) {
              newOrder <- currentOrder
              newOrder[c(currentPos, currentPos+1)] <- newOrder[c(currentPos+1, currentPos)]
              values[[orderKey]] <- newOrder
              
              debugLog(paste("BUTTON_PURE_REACTIVE: Moved", mod_name, "down in", varPrefix, ". New order:", paste(newOrder, collapse = ", ")), "BUTTON_TRACE")
            } else {
              debugLog(paste("BUTTON_PURE_REACTIVE: Cannot move", mod_name, "down (already at bottom or not found)"), "BUTTON_TRACE")
            }
          }, ignoreInit = TRUE, autoDestroy = FALSE)
          
          # Store observers for future cleanup
          new_observers[[upButtonId]] <<- upObserver
          new_observers[[downButtonId]] <<- downObserver
        })
      }
      
      # Register new observers
      button_observers_registry[[varPrefix]] <<- new_observers
      debugLog(paste("BUTTON_PURE_REACTIVE: Registered", length(new_observers), varPrefix, "observers"), "UI_INIT")
    }
  }, ignoreInit = TRUE)
}

debugLog("BUTTON_OBSERVERS_CORE: Core functions loaded", "UI_INIT")
