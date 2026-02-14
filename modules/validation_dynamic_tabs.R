# Dynamic tab content module
# Generate conditional histogram and density tab content based on validation

# Dynamic histogram tab content with conditional rendering
output$histogramTabContent <- renderUI({
  current_ind_var1 <- input$independentVar1
  
  if(is.null(current_ind_var1) || current_ind_var1 == "") {
    return(div(
      class = "insufficient-data-content",
      h4("Aucune variable indépendante sélectionnée"),
      p("Veuillez sélectionner au moins une variable indépendante.")
    ))
  }
  
  # Check for duplicate variables
  duplicateInfo <- tryCatch({
    checkDuplicateVariables(
      current_ind_var1,
      input$independentVar2,
      input$independentVar3,
      labels
    )
  }, error = function(e) {
    debugLog(paste("Error checking duplicates in histogram tab:", e$message), "VALIDATION_ERROR")
    list(shouldDisableHistDensity = FALSE)
  })
  
  if(duplicateInfo$shouldDisableHistDensity) {
    title <- getCachedLabel("histogramDisabledTitle", labels, "Histogram disabled")
    message <- getCachedLabel("histogramDisabledMessage", labels, "Histogram plots are disabled when all independent variables are identical.")
    
    return(div(
      class = "disabled-tab-content",
      h4(title),
      p(message),
      icon("ban", class = "fa-3x", style = "color: #ccc; margin-top: 20px;")
    ))
  }
  
  # Generate normal content with cached labels
  binLabel <- getCachedLabel("binNumberLabel", labels, "Number of bins")
  alphaLabel <- getCachedLabel("alphaLabel", labels, "Transparency")
  positionLabel <- getCachedLabel("positionLabel", labels, "Position")
  densityOverlayLabel <- getCachedLabel("showDensityOverlayLabel", labels, "Show density overlay")
  insufficientTitle <- getCachedLabel("insufficientDataTitle", labels, "Insufficient Data")
  insufficientMessage <- getCachedLabel("insufficientDataMessage", labels, "The selected combination results in insufficient data for plotting.")
  suggestion1 <- getCachedLabel("insufficientDataSuggestion1", labels, "Selecting different variables")
  suggestion2 <- getCachedLabel("insufficientDataSuggestion2", labels, "Including more modalities")
  suggestion3 <- getCachedLabel("insufficientDataSuggestion3", labels, "Using a different data subset")
  
  tagList(
    wellPanel(
      fluidRow(
        column(3, sliderInput("histBins", binLabel, min = 5, max = 100, value = defaultBinNumber)),
        column(3, sliderInput("histAlpha", alphaLabel, min = 0, max = 1, value = 0.5, step = 0.1)),
        column(3, selectInput("histPosition", positionLabel, choices = c("identity", "dodge", "stack"))),
        column(3, checkboxInput("histDensityOverlay", densityOverlayLabel, FALSE))
      )
    ),
    
    # Conditional plot display based on validation
    conditionalPanel(
      condition = "output.histogramDataValidation",
      plotOutput("histPlot", height = "600px")
    ),
    conditionalPanel(
      condition = "!output.histogramDataValidation",
      div(
        class = "insufficient-data-content",
        h4(icon("exclamation-triangle"), " ", insufficientTitle),
        p(insufficientMessage),
        p(strong("Suggestions :")),
        tags$ul(
          style = "text-align: left; display: inline-block;",
          tags$li(suggestion1),
          tags$li(suggestion2),
          tags$li(suggestion3)
        )
      )
    ),
    
    br(),
    uiOutput("histPlotControls")
  )
})

# Dynamic density tab content with conditional rendering  
output$densityTabContent <- renderUI({
  current_ind_var1 <- input$independentVar1
  
  if(is.null(current_ind_var1) || current_ind_var1 == "") {
    return(div(
      class = "insufficient-data-content",
      h4("Aucune variable indépendante sélectionnée"),
      p("Veuillez sélectionner au moins une variable indépendante.")
    ))
  }
  
  # Check for duplicate variables
  duplicateInfo <- tryCatch({
    checkDuplicateVariables(
      current_ind_var1,
      input$independentVar2,
      input$independentVar3,
      labels
    )
  }, error = function(e) {
    debugLog(paste("Error checking duplicates in density tab:", e$message), "VALIDATION_ERROR")
    list(shouldDisableHistDensity = FALSE)
  })
  
  if(duplicateInfo$shouldDisableHistDensity) {
    title <- getCachedLabel("densityDisabledTitle", labels, "Density plot disabled")
    message <- getCachedLabel("densityDisabledMessage", labels, "Density plots are disabled when all independent variables are identical.")
    
    return(div(
      class = "disabled-tab-content",
      h4(title),
      p(message),
      icon("ban", class = "fa-3x", style = "color: #ccc; margin-top: 20px;")
    ))
  }
  
  # Generate normal content with cached labels
  alphaLabel <- getCachedLabel("alphaLabel", labels, "Transparency")
  lineSizeLabel <- getCachedLabel("lineSizeLabel", labels, "Line width")
  fillLabel <- getCachedLabel("fillUnderCurveLabel", labels, "Fill under curve")
  rugLabel <- getCachedLabel("showRugLabel", labels, "Show rug plot")
  insufficientTitle <- getCachedLabel("insufficientDataTitle", labels, "Insufficient Data")
  insufficientMessage <- getCachedLabel("insufficientDataMessage", labels, "The selected combination results in insufficient data for plotting.")
  suggestion1 <- getCachedLabel("insufficientDataSuggestion1", labels, "Selecting different variables")
  suggestion2 <- getCachedLabel("insufficientDataSuggestion2", labels, "Including more modalities")
  suggestion3 <- getCachedLabel("insufficientDataSuggestion3", labels, "Using a different data subset")
  
  tagList(
    wellPanel(
      fluidRow(
        column(3, sliderInput("densityAlpha", alphaLabel, min = 0, max = 1, value = 0.5, step = 0.1)),
        column(3, sliderInput("densityLineSize", lineSizeLabel, min = 0.5, max = 3, value = defaultLineSize, step = 0.5)),
        column(3, checkboxInput("densityFill", fillLabel, TRUE)),
        column(3, checkboxInput("densityShowRug", rugLabel, FALSE))
      )
    ),
    
    # Conditional plot display based on validation
    conditionalPanel(
      condition = "output.densityDataValidation",
      plotOutput("densityPlot", height = "600px")
    ),
    conditionalPanel(
      condition = "!output.densityDataValidation",
      div(
        class = "insufficient-data-content",
        h4(icon("exclamation-triangle"), " ", insufficientTitle),
        p(insufficientMessage),
        p(strong("Suggestions :")),
        tags$ul(
          style = "text-align: left; display: inline-block;",
          tags$li(suggestion1),
          tags$li(suggestion2),
          tags$li(suggestion3)
        )
      )
    ),
    
    br(),
    uiOutput("densityPlotControls")
  )
})

debugLog("VALIDATION_DYNAMIC_TABS: Dynamic tab content system loaded", "VALIDATION_INIT")