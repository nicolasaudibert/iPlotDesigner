# iPlotDesigner - Technical Documentation

## Overview

iPlotDesigner is a Shiny application for interactive exploration and comparison of data distributions. It provides a comprehensive suite of visualization tools with extensive customization options and generates production-ready R code for complete research reproducibility.

---

## Application Architecture

### Modular Design

The application follows a modular architecture with 55+ specialized modules organized by functional domain:

```
iPlotDesigner/
├── app.R                      # Main application entry point
├── modules/
│   ├── debug_utils.R          # Debug system
│   ├── plot_functions.R       # Plot generation
│   ├── ui_outputs.R           # UI components
│   ├── validation_outputs.R   # Validation system
│   ├── variable_properties.R  # Variable management
│   ├── variable_management.R  # Variable operations
│   ├── code_generation_*.R    # Code generation modules
│   ├── ui_theme_*.R           # Theme UI modules
│   └── ...
├── localization/
│   ├── iPlotDesigner.English.html
│   ├── iPlotDesigner.English.labels.txt
│   ├── iPlotDesigner.French.html
│   └── iPlotDesigner.French.labels.txt
└── global_settings.R          # Global configuration
```

### Key Architectural Patterns

**Hybrid Reactive Context Management**: Critical reactive functions preserved in main files while supporting logic is modularized for maintainability.

**Lazy Loading**: Modules load on-demand to reduce startup overhead and improve performance.

**Hierarchical Debug System**: Structured debug categories (`DOMAIN_COMPONENT_LEVEL`) with profile-based filtering.

---

## Core Features

### 1. Data Import System

**Supported Formats**: CSV, TSV, Excel (xlsx/xls), ODS

**Advanced Import Options**:
- Configurable column separator and encoding
- Sheet selection for multi-sheet files
- Interactive preview with customizable row count
- User-defined NA values
- Dual import modes: Simple (default) or Advanced with full parameter control

**Interactive Filtering**: DataTable-based filtering with column-specific controls after import.

### 2. Variable Management

**Variable Types**:
- **Dependent variable**: Quantitative (numeric) - required
- **Independent variables**: Up to 3 categorical variables for grouping and faceting

**Variable Roles**:
- **Variable 1**: Primary grouping (x-axis in bar/box/violin, colors in histogram/density)
- **Variable 2**: Secondary grouping or color aesthetic
- **Variable 3**: Additional faceting dimension

**Variable Reuse System**:
- Automatic property copying when same variable selected in multiple slots
- Source/copy hierarchy with real-time synchronization
- Visual indicators (badges) for source vs. copy status
- Automatic source transfer when original source is deselected

**Variable Swapping**: Quick swap between variables while preserving all properties.

### 3. Plot Types

| Plot Type | Description | Key Options |
|-----------|-------------|-------------|
| **Histogram** | Frequency distribution | Bin width, density overlay, position |
| **Density** | Kernel density estimation | Bandwidth, alpha, fill |
| **Box Plot** | Quartile visualization | Notch, outliers, orientation |
| **Violin** | Density + box plot hybrid | Trim, scale, quantiles |
| **Bar Plot** | Mean/count with error bars | Error type (SE, SD, CI), dodge/stack |
| **Ridge Plot** | Stacked density plots | Scale, overlap, gradient fill |

### 4. Theme Customization System

**68 Customizable Parameters** organized in 4 tabs:

#### Background Elements (7 parameters)
- Legend position (right, left, top, bottom, none)
- Legend background fill color
- Legend box background (legacy support)
- Legend key background and border
- Strip background color

#### Titles Customization (29 parameters)
- Global text size and font face
- Axis titles (X/Y): size, font, face, color
- Strip text: unified or separate X/Y customization
- Legend title and text styling

**Font Face Options**: Normal (plain), Bold, Italic, Bold Italic

#### Axis Text (13 parameters)
- Global or per-axis customization
- Size, angle, font family, face, color
- Automatic hjust/vjust adjustment for rotated text

#### Grids and Borders (16 parameters)
- Show/hide toggles for major/minor grids, panel border, strip border
- Color and line width customization
- Priority logic: show/hide overrides color settings

### 5. Code Generation System

**Production-Ready Output**: Generates complete, executable R scripts.

**Features**:
- Accurate package detection (only required packages listed)
- Complete localization (comments adapt to interface language)
- User operations tracking (filtering, conversions)
- Special character safety (apostrophes, spaces in variable names)
- Configurable metadata and error handling

**Generated Code Includes**:
- Package loading
- Data import with exact parameters
- All user transformations (filtering, type conversions)
- Complete ggplot2 code with theme customization
- Export commands

**Advanced Configuration**:
- Toggle metadata inclusion
- Enable/disable error handling wrappers
- Include verification steps
- Control verbosity level

### 6. Extended Font System

**Standard Fonts** (always available): sans, serif, mono, Helvetica, Times, Courier

**Optional System Fonts** (via extrafont package):
- 100+ additional fonts when enabled
- Three-state detection: not installed, not imported, ready
- Guided import process with progress notifications
- Intelligent filtering of problematic fonts

**Font Filtering**: Automatic exclusion of:
- macOS hidden fonts (`.SF Compact`, `.New York`)
- Symbol fonts (Emoji, Wingdings)
- Fonts with encoding issues

**Server Deployment Support**:
- Configurable font import capability
- Pre-import mode for shared hosting
- Security controls for multi-user environments

---

## User Interface

### Bootstrap 5 Integration

- Modern responsive design
- Interactive tooltips with contextual help
- Improved accessibility (ARIA labels, semantic HTML)
- Full keyboard navigation support

### Tabbed Organization

**6 Main Tabs**:
1. Variable Selection
2. Variable 1 Properties
3. Variable 2 Properties
4. Variable 3 Properties
5. Sample Sizes (Effectifs)
6. Data Filtering

**Benefits**:
- Lazy loading for performance
- Reduced cognitive load
- Clear workflow progression

### Accessibility Features

- **Keyboard shortcuts**: Enter (confirm), Escape (cancel), Tab (navigate)
- **Screen reader support**: Proper ARIA labels
- **Focus management**: Automatic focus on dialog open
- 13 dialogs with full keyboard navigation

---

## Validation System

### Intelligent Variable Validation

**Automatic Recommendations**:
- Numeric-to-categorical conversion based on unique value threshold
- NA handling strategies (filter, assign label, keep as category)
- Complexity warnings for high modality counts

**User Choice Dialogs**:
- Clear explanations of each option
- Keyboard navigation support
- Localized in English and French

### Sample Size Monitoring

**Dedicated Tab** with three display modes:
- Facets: Separate tables per Variable 3 level
- Rows: Combined with Var1×Var3 combinations
- Columns: Combined with Var2×Var3 combinations

**Color-Coded Warnings**:
- Red: Zero observations (empty subgroup)
- Orange: Single observation (insufficient for density)
- Normal: Valid subgroups

---

## Performance Optimization

### Adaptive Utilities

- Automatic method selection based on dataset size
- Base R for small data, data.table for large data
- Smart thresholds (optimization activates >15,000 elements)

### Caching System

- Session-level font caching
- Label cache with monitoring
- Modal dialog caching

### Profiling Integration

- Start/stop profiling controls
- Automatic performance analysis
- Export `.Rprof` files for detailed investigation
- Integration with `profvis` for visualization

---

## Debug System

### Hierarchical Categories

Structure: `SYSTEM_COMPONENT_LEVEL`

**Levels**:
- CRITICAL: Always enabled, critical errors
- ERROR: Important errors
- WARN: Warnings
- INFO/INIT: Informational
- DEBUG: Development debugging
- TRACE: Intensive debugging

**Profiles**:
- `production`: Only CRITICAL/ERROR/WARN
- `development`: Everything except TRACE
- `verbose`: All categories
- `silent`: Only CRITICAL
- `custom`: User-defined configuration

### Font-Specific Categories

```r
# Code generation for fonts
CODE_FONT_DETECT   # Font detection
CODE_FONT_SETUP    # Setup code generation
CODE_FONT_ERROR    # Errors

# Font system operations
FONT_INIT          # Initialization
FONT_IMPORT        # Import process
FONT_DETECT        # Status detection
FONT_FILTER        # Problematic font filtering
FONT_ERROR         # System errors
```

---

## Localization

### Bilingual Support

Complete English and French localization for:
- All UI labels and tooltips
- Validation messages and dialogs
- Generated code comments
- Error messages

### Implementation

- External localization files (`.html`, `.labels.txt`)
- Cache-based label retrieval
- Fallback to English for missing translations
- 500+ localization keys

---

## Research Reproducibility

### Complete Workflow Documentation

Generated code precisely reproduces:
1. Data import with exact parameters
2. All filtering operations
3. Variable type conversions with level preservation
4. Plot generation with full customization
5. Theme settings
6. Export commands

### Collaboration Support

- Self-contained scripts (no external dependencies beyond packages)
- Clear prerequisites documentation
- Optional verification steps
- Configurable verbosity for different audiences

---

## Dependencies

### Required

| Package | Purpose |
|---------|---------|
| shiny | Core framework |
| bslib | Bootstrap 5 theming |
| bsicons | Bootstrap icons |
| tidyverse | Data manipulation, ggplot2 |
| ggridges | Ridge plots |
| DT | Interactive tables |
| Cairo | High-quality graphics |
| readxl | Excel import |
| readODS | ODS import |
| writexl | Excel export |
| shinyAce | Code editor |
| sortable | Drag-and-drop |
| clipr | Clipboard |
| data.table | Performance |
| jsonlite | JSON processing |

### Optional

| Package | Purpose |
|---------|---------|
| profvis | Performance profiling |
| htmlwidgets | Widget support |
| digest | Session ID generation |
| extrafont | System fonts |

---

## Configuration Options

### Global Settings (global_settings.R)

```r
# Upload limits
maxUploadSizeMB <- 20

# Debug configuration
debugMode <- TRUE
defaultDebugProfile <- "production"
profilingEnabled <- FALSE

# Font system
allowSystemFontsImport <- TRUE
EXCLUDED_FONTS <- c(...)  # Problematic fonts list
```

### Server Deployment

For shared hosting or Docker deployments:
- Pre-import fonts for all users
- Disable user font import capability
- Configure appropriate debug profile

---

## Usage Workflow

1. **Upload Data**: Select file, optionally configure import parameters
2. **Select Variables**: Choose dependent and independent variables
3. **Configure Properties**: Set modalities, colors, ordering per variable
4. **Filter Data** (optional): Use interactive table filtering
5. **Choose Plot Type**: Navigate to desired visualization tab
6. **Customize Theme**: Adjust appearance in Advanced Theme Options
7. **Configure Code Generation**: Set metadata and error handling preferences
8. **Export**: Download plot image and copy/save generated R code

---

## Best Practices

### For Large Datasets

- Use data filtering to reduce size before analysis
- Configure variables step-by-step using dedicated tabs
- Consider fewer grouping variables
- Monitor performance with profiling tools
- Export and reuse generated code

### For Reproducibility

- Include all metadata in generated code
- Enable verification steps for critical analyses
- Document any manual modifications
- Save complete generated scripts with data

### For Deployment

- Set appropriate debug profile (production)
- Configure upload limits based on server capacity
- Pre-import fonts if using system fonts
- Test keyboard navigation for accessibility compliance

---

## Credits

iPlotDesigner was developed collaboratively by Nicolas Audibert (Sorbonne Nouvelle University) and Claude (Anthropic), based on the iScatter application model. The application combines advanced Shiny development patterns with comprehensive data visualization capabilities for research applications.
