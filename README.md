# iPlotDesigner - Interactive plot design and R code generation

Shiny app to import a data file, choose quantitative and categorical variables, and create customizable distribution plots (histograms, density plots, box plots, violin plots, bar plots, ridge plots) with extensive theme customization and production-ready R code generation.

This application was developed by Nicolas Audibert, using Claude (Anthropic) as an AI assistant, building upon the iScatter application architecture and extending it with advanced distribution visualization capabilities.

For details about the iScatter application, see the repository (https://github.com/nicolasaudibert/iScatter) and the related publication (in French):
https://inria.hal.science/hal-04623107v1/document

Try the app online:
- English version: https://shiny.laboratoirephonetiquephonologie.fr/iPlotDesigner/
- French version: https://shiny.laboratoirephonetiquephonologie.fr/iPlotDesigner_fr/

## Key Features

- **6 plot types**: Histogram, density, box plot, violin, bar plot, ridge plot
- **Up to 3 grouping variables**: Primary grouping, color aesthetics, faceting
- **68 theme parameters**: Comprehensive customization (fonts, colors, sizes, grids, borders)
- **Executable R code generation**: Production-ready code with full reproducibility
- **Bilingual interface**: English and French localization
- **Advanced data import**: CSV, TSV, Excel, ODS with preview and filtering

## Installation and Execution (Local)

You will need R (https://cran.r-project.org/) and RStudio (https://posit.co/download/rstudio-desktop/).

1) Download files: clone the repository, or use Code / Download ZIP
2) Open app.R in RStudio
3) Install required packages if prompted (see Dependencies below)
4) Use the Run App button to run the app in a browser

## Dependencies

### Required Packages

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "tidyverse", "ggridges",
  "DT", "Cairo", "readxl", "readODS", "writexl",
  "shinyAce", "sortable", "clipr", "data.table", "jsonlite"
))
```

### Optional Packages

```r
# Performance profiling
install.packages(c("profvis", "htmlwidgets", "digest"))

# Extended font support (100+ system fonts)
install.packages("extrafont")
# First-time setup: extrafont::font_import()
```

## Configuration

### Maximum Upload Size

Default is 20MB. To change, edit `maxUploadSizeMB` in app.R:

```r
maxUploadSizeMB <- 30   # Set to 30MB
maxUploadSizeMB <- Inf  # No limit
```

### Language

The app uses localization files in the `localization` subfolder. Default is English.

To change language, replace `iPlotDesigner.html` and `iPlotDesigner.labels.txt` with copies of localization files in another language, or use symbolic links (recommended).

## Documentation

See [TECHNICAL.md](TECHNICAL.md) for detailed technical documentation including architecture, features, and implementation details.
