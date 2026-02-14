# Variable swap handlers module
# Management of variable swapping operations with swap-specific protection

# Swap variable handlers with protection and timing
observeEvent(input$swapVar1Var2, {
  if(isProtected()) return()
  
  startTime <- traceObserverTiming("swapVar1Var2", "START")
  traceObserver("swapVar1Var2", "", "TRIGGERED")
  
  # Set swap-specific flag to prevent observers from clearing properties
  values[["swap_in_progress"]] <- TRUE
  values[["swap_observers_completed"]] <- 0  # Counter for observers that have run
  debugLog("VAR_SWAP: swap_in_progress flag set to TRUE", "VAR_SWAP")
  
  globalCooldown(TRUE)
  
  var1 <- input$independentVar1
  var2 <- input$independentVar2
  
  # Swap properties BEFORE updating selectors
  swapVariableProperties(1, 2, values, input, session)
  
  # Force recalculation of filtered data to ensure coherence
  if(exists("refreshFilteredData", envir = .GlobalEnv)) {
    refreshFilteredData(values)
  }
  
  # Update selectors - this will trigger observers ASYNCHRONOUSLY
  updateSelectInput(session, "independentVar1", selected = var2)
  updateSelectInput(session, "independentVar2", selected = var1)
  
  # Force validation refresh with dedicated trigger
  values[["swap_completed_trigger"]] <- Sys.time()
  
  # Reset protection flags immediately (observers will manage swap_in_progress themselves)
  resetAllFlags(0)
  
  traceObserver("swapVar1Var2", "", "COMPLETED")
  traceObserverTiming("swapVar1Var2", "COMPLETED", startTime)
})

observeEvent(input$swapVar2Var3, {
  if(isProtected()) return()
  
  startTime <- traceObserverTiming("swapVar2Var3", "START")
  traceObserver("swapVar2Var3", "", "TRIGGERED")
  
  # Set swap-specific flag to prevent observers from clearing properties
  values[["swap_in_progress"]] <- TRUE
  values[["swap_observers_completed"]] <- 0
  debugLog("VAR_SWAP: swap_in_progress flag set to TRUE", "VAR_SWAP")
  
  globalCooldown(TRUE)
  
  var2 <- input$independentVar2
  var3 <- input$independentVar3
  
  # Swap properties BEFORE updating selectors
  swapVariableProperties(2, 3, values, input, session)
  
  # Force recalculation of filtered data to ensure coherence
  if(exists("refreshFilteredData", envir = .GlobalEnv)) {
    refreshFilteredData(values)
  }
  
  # Update selectors - this will trigger observers ASYNCHRONOUSLY
  updateSelectInput(session, "independentVar2", selected = var3)
  updateSelectInput(session, "independentVar3", selected = var2)
  
  # Force validation refresh with dedicated trigger
  values[["swap_completed_trigger"]] <- Sys.time()
  
  # Reset protection flags immediately (observers will manage swap_in_progress themselves)
  resetAllFlags(0)
  
  traceObserver("swapVar2Var3", "", "COMPLETED")
  traceObserverTiming("swapVar2Var3", "COMPLETED", startTime)
})

observeEvent(input$swapVar1Var3, {
  if(isProtected()) return()
  
  startTime <- traceObserverTiming("swapVar1Var3", "START")
  traceObserver("swapVar1Var3", "", "TRIGGERED")
  
  # Set swap-specific flag to prevent observers from clearing properties
  values[["swap_in_progress"]] <- TRUE
  values[["swap_observers_completed"]] <- 0
  debugLog("VAR_SWAP: swap_in_progress flag set to TRUE", "VAR_SWAP")
  
  globalCooldown(TRUE)
  
  var1 <- input$independentVar1
  var3 <- input$independentVar3
  
  # Swap properties BEFORE updating selectors
  swapVariableProperties(1, 3, values, input, session)
  
  # Force recalculation of filtered data to ensure coherence
  if(exists("refreshFilteredData", envir = .GlobalEnv)) {
    refreshFilteredData(values)
  }
  
  # Update selectors - this will trigger observers ASYNCHRONOUSLY
  updateSelectInput(session, "independentVar1", selected = var3)
  updateSelectInput(session, "independentVar3", selected = var1)
  
  # Force validation refresh with dedicated trigger
  values[["swap_completed_trigger"]] <- Sys.time()
  
  # Reset protection flags immediately (observers will manage swap_in_progress themselves)
  resetAllFlags(0)
  
  traceObserver("swapVar1Var3", "", "COMPLETED")
  traceObserverTiming("swapVar1Var3", "COMPLETED", startTime)
})
