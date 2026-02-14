# UI variable selection module - 5 tabs version
# Variable selection in separate tab + 3 properties tabs + filtering tab
# Maximum separation for performance optimization

# Function to create variable selection section with 5-tab interface
createVariableSelectionSection <- function(labels) {
  # Cache labels for tabs
  variableSelectionTabLabel <- safeGetUILabel("variableSelectionTabLabel", labels, "Variable selection")
  variable1TabLabel <- safeGetUILabel("variable1TabLabel", labels, "Variable 1")
  variable2TabLabel <- safeGetUILabel("variable2TabLabel", labels, "Variable 2  ") 
  variable3TabLabel <- safeGetUILabel("variable3TabLabel", labels, "Variable 3")
  dataFilteringTabLabel <- safeGetUILabel("dataFilteringTabLabel", labels, "Data filtering")
  
  wellPanel(
    # 5-tab interface - maximum separation
    tabsetPanel(
      id = "variableConfigurationTabset",
      type = "tabs",
      
      # Tab 1 - variable selection (completely isolated)
      tabPanel(
        title = variableSelectionTabLabel,
        value = "selection_tab",
        br(),
        
        # Variable selectors section (moved from main layout)
        h4(safeGetUILabel("variableSelectionTitle", labels, "Variable selection and configuration")),
        
        fluidRow(
          column(3, uiOutput("dependentVarUI")),
          column(3, uiOutput("independentVar1UI")),
          column(3, uiOutput("independentVar2UI")),
          column(3, uiOutput("independentVar3UI"))
        ),
        
        br(),
        
        # Warning message for problematic subgroups
        uiOutput("subgroupWarningMessage"),
        
        br(),
        
        # Swap buttons section (moved from main layout)
        h5(safeGetUILabel("variableSwapTitle", labels, "Variable swapping")),
        fluidRow(
          column(3, offset = 3, uiOutput("swapVar1Var2Button")),
          column(3, uiOutput("swapVar2Var3Button")),
          column(3, uiOutput("swapVar1Var3Button"))
        )
      ),
      
      # Tab 2 - variable 1 properties (lazy loading)
      tabPanel(
        title = variable1TabLabel,
        value = "var1_tab",
        br(),
        uiOutput("var1Properties_tabbed")
      ),
      
      # Tab 3 - variable 2 properties (lazy loading)
      tabPanel(
        title = variable2TabLabel,
        value = "var2_tab",
        br(),
        uiOutput("var2Properties_tabbed") 
      ),
      
      # Tab 4 - variable 3 properties (lazy loading)
      tabPanel(
        title = variable3TabLabel,
        value = "var3_tab", 
        br(),
        uiOutput("var3Properties_tabbed")
      ),
      
      # Tab 5 - sample sizes (lazy loading)
      tabPanel(
        title = safeGetUILabel("sampleSizeTabLabel", labels, "Sample sizes"),
        value = "sample_size_tab",
        br(),
        h4(safeGetUILabel("sampleSizeTitle", labels, "Sample sizes by subgroup")),
        
        fluidRow(
          column(3, 
                 conditionalPanel(
                   condition = "input.independentVar3 !== null && input.independentVar3 !== '' && input.independentVar3 !== 'Aucune'",
                   radioButtons("sampleSizeVar3Display", 
                              safeGetUILabel("sampleSizeVar3DisplayLabel", labels, "Variable 3 display"),
                              choices = setNames(
                                c("rows", "cols", "facets"),
                                c(safeGetUILabel("sampleSizeVar3Rows", labels, "Rows (Var1 × Var3)"),
                                  safeGetUILabel("sampleSizeVar3Cols", labels, "Columns (Var2 × Var3)"),
                                  safeGetUILabel("sampleSizeVar3Facets", labels, "Facets (separate tables)"))
                              ),
                              selected = "rows")
                 )
          ),
          column(3,
                 checkboxInput("sampleSizeShowTotals", 
                             safeGetUILabel("sampleSizeShowTotalsLabel", labels, "Show marginal totals"),
                             value = FALSE)
          ),
          column(3,
                 checkboxInput("sampleSizeHideEmpty", 
                             safeGetUILabel("sampleSizeHideEmptyLabel", labels, "Hide empty rows/columns"),
                             value = FALSE)
          ),
          column(3,
                 conditionalPanel(
                   condition = "input.independentVar3 !== null && input.independentVar3 !== '' && input.independentVar3 !== 'Aucune' && input.sampleSizeVar3Display === 'facets'",
                   sliderInput("sampleSizeTablesPerRow",
                             safeGetUILabel("sampleSizeTablesPerRowLabel", labels, "Tables per row"),
                             min = 1, max = 4, value = 1, step = 1)
                 )
          )
        ),
        
        br(),
        
        uiOutput("sampleSizeTable"),
        
        br(),
        
        # Legend for highlighting - moved below table
        wellPanel(
          style = "background-color: #f8f9fa;",
          h5(safeGetUILabel("sampleSizeLegendTitle", labels, "Legend")),
          fluidRow(
            column(4, 
                   tags$span(style = paste0("background-color: ", sampleSizeZeroColor, "; padding: 2px 8px;"),
                            safeGetUILabel("sampleSizeZeroLabel", labels, "Zero count"))
            ),
            column(4,
                   tags$span(style = paste0("background-color: ", sampleSizeOneColor, "; padding: 2px 8px;"),
                            safeGetUILabel("sampleSizeOneLabel", labels, "Single value"))
            ),
            column(4,
                   tags$span(style = paste0("background-color: ", sampleSizeNormalColor, "; padding: 2px 8px; border: 1px solid #ddd;"),
                            safeGetUILabel("sampleSizeNormalLabel", labels, "Normal count"))
            )
          )
        )
      ),
      
      # Tab 6 - data filtering (lazy loading)
      tabPanel(
        title = dataFilteringTabLabel,
        value = "filtering_tab",
        br(),
        h4(safeGetUILabel("advancedFilteringTitle", labels, "Advanced data filtering")),
        
        # Warning message about filtering not reflected in code
        div(
          class = "alert alert-warning",
          style = "margin-bottom: 15px;",
          icon("triangle-exclamation"),
          " ",
          safeGetUILabel("filteringNotInCodeWarning", labels, 
                        "Note: Filtering applied via this table affects the displayed plots but is NOT reflected in the generated R code.")
        ),
        
        DTOutput("filteringDT")
      )
    )
  )
}
