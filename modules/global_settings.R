# Global parameters and settings with advanced import support
# Main configuration and basic utilities

# File handling
maxUploadSizeMB <- 20
nRowsDisplayedInFilePreview <- 20
availableColumnSeparatorsDisplay <- c("tabulation (\\t)", "espace", "virgule (,)" ,"point-virgule (;)")
availableColumnSeparators <- c("\t", " ", ",", ";")
availableEncodingsDisplay <- c("Unicode (UTF-8)","Windows (latin-1)", "Windows (latin-9)", "Mac OS (MacRoman)", "inconnu")
availableEncodings <- c("UTF-8","latin-1", "latin-9", "macintosh", "unknown")

# Export formats
availableOutputFormats <- c("PNG","PDF", "EPS", "SVG")
availableOutputFormatExtensions <- c(".png", ".pdf", ".eps", ".svg")
availableUnitsForImageExport <- c("cm","in","mm")

# Default values
defaultTheme <- "spacelab"
htmlHeaderFile <- "iPlotDesigner.html"
labelsFile <- "iPlotDesigner.labels.txt"
exportedImagesUnit <- "cm"
defaultExportedPlotWidth <- 20
defaultExportedPlotHeight <- 15
defaultAlpha <- 0.7
defaultTextSize <- 12
defaultPointSize <- 3
defaultLineSize <- 1
defaultErrorBarWidth <- 0.2
defaultBinNumber <- 30

# Dialog parameters for filtered modalities
maxModalitiesInDialog <- 5
enableModalityDownload <- TRUE

# Default colors for each variable slot with improved palettes
defaultColors <- list(
  var1 = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),
  var2 = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999", "#66c2a5"),
  var3 = c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd")
)

# Sample size table highlighting colors
sampleSizeZeroColor <- "#ffcccc"       # Light red for zero counts
sampleSizeOneColor <- "#fff4cc"        # Light yellow for single counts
sampleSizeNormalColor <- "#ffffff"     # White for normal counts

# ============================================================================
# Font system settings
# ============================================================================

# Font system configuration
# Set to TRUE for local installations (allows font import)
# Set to FALSE for server deployments (import disabled)
allowSystemFontsImport <- TRUE

# Problematic fonts known to cause rendering issues
EXCLUDED_FONTS <- c(
  "LastResort",
  "Apple.*Emoji",
  "Apple.*Symbols",
  "AppleMyungjo",     # Korean font - rendering issues on macOS
  "Bodoni Ornaments",
  "Marlett",
  "Noto.*",     # Font with many variants, not included as a default to avoid overloading list - May be removed from exclusion list if needed
  "Webdings",
  "Wingdings.*",
  "Symbol"
)

# ============================================================================
# Enhanced user choice settings for advanced validation control
# ============================================================================

# Main configuration for user choice dialogs
USER_CHOICE_SETTINGS <- list(
  # Core thresholds for intelligent decision making
  numeric_conversion_threshold = 40,          # Max unique values for numeric->categorical conversion
  numeric_conversion_threshold_high = 120,    # Max unique values for discouraged but possible conversion
  complexity_warning_threshold = 200,        # Modality count triggering complexity warnings
  performance_warning_threshold = 500,       # Modality count triggering performance warnings
  
  # User interface behavior
  auto_filter_suggestions = TRUE,             # Show intelligent filtering suggestions
  show_performance_estimates = TRUE,          # Display performance impact estimates
  remember_user_preferences = FALSE,          # Remember choices within session (future enhancement)
  
  # Default dialog behavior - enabled by default
  enable_user_choice_dialogs = TRUE,         # Enabled by default
  show_advanced_options = TRUE,              # Show advanced filtering options in dialogs
  default_to_recommendations = TRUE          # Pre-select recommended options in dialogs
)

# Smart filtering configuration
SMART_FILTERING_OPTIONS <- list(
  # Default filtering parameters
  top_n_default = 50,                        # Default number of top modalities to keep
  min_frequency_default = 5,                 # Default minimum frequency threshold
  percentage_threshold_default = 1.0,        # Default percentage threshold (1% of data)
  
  # Grouping behavior
  auto_group_others = TRUE,                  # Automatically group remaining modalities as "Other"
  other_category_name = "Other",             # Name for grouped remaining modalities
  
  # Performance-based thresholds
  complexity_auto_filter_threshold = 1000,   # Auto-apply filtering for extreme complexity
  ui_performance_threshold = 300             # UI element count threshold for performance warnings
)

# Complexity assessment thresholds
COMPLEXITY_THRESHOLDS <- list(
  simple = 50,           # Up to 50 modalities: simple
  moderate = 200,        # 51-200 modalities: moderate complexity
  complex = 500,         # 201-500 modalities: complex
  very_complex = 2000,   # 501-2000 modalities: very complex
  extreme = Inf          # >2000 modalities: extreme complexity
)

# Performance impact estimation parameters
PERFORMANCE_ESTIMATION <- list(
  # Base timing estimates (in seconds)
  base_ui_time = 0.1,                    # Base UI response time
  modality_factor = 0.2,                 # Time factor per log10(modality_count)
  sparsity_penalty = 0.5,                # Additional time for sparse data
  
  # Memory usage estimates (rough approximation)
  dom_elements_per_modality = 5,         # UI elements per modality
  memory_mb_per_modality = 0.01,         # Memory usage per modality (MB)
  
  # Performance level thresholds (in seconds)
  good_threshold = 1.0,                  # <1s = good performance
  acceptable_threshold = 3.0,            # 1-3s = acceptable performance
  slow_threshold = 10.0                  # 3-10s = slow, >10s = very slow
)

# Dialog appearance and behavior
DIALOG_CONFIGURATION <- list(
  # Visual styling
  show_icons = TRUE,                     # Show icons in dialog titles
  use_color_coding = TRUE,               # Use color coding for different message types
  
  # Content limits
  max_modalities_preview = 10,           # Maximum modalities to show in previews
  max_recommendation_text_length = 200,  # Maximum length for recommendation text
  
  # Button configuration
  show_cancel_option = TRUE,             # Always show cancel option
  highlight_recommended = TRUE,          # Visually highlight recommended choices
  use_confirmation_dialogs = FALSE       # Require confirmation for destructive actions (future)
)

# ============================================================================
# Enhanced user operations configuration
# ============================================================================

# Enhanced User Operations Configuration
USER_OPERATIONS_CONFIG <- list(
  enable_operations_history = TRUE,
  include_in_code_generation = TRUE,
  track_numeric_conversions = TRUE,
  track_custom_filtering = TRUE,
  track_complexity_choices = TRUE,
  max_operations_stored = 50,  # Limit for memory management
  include_timestamps = TRUE,
  include_performance_metadata = TRUE
)

# ============================================================================
# Enhanced code generation configuration
# ============================================================================

# Enhanced code generation configuration
ENHANCED_CODE_CONFIG <- list(
  # Execution preferences
  generate_executable_operations = TRUE,
  include_error_handling = FALSE,
  include_verification_steps = FALSE,
  use_performance_optimization = FALSE,
  
  # Metadata preferences  
  include_r_version = FALSE,
  include_platform_info = FALSE,
  include_session_info = FALSE,
  include_operation_timestamps = FALSE,
  
  # Localization preferences
  localize_all_comments = TRUE,
  localize_variable_names = FALSE,
  localize_error_messages = TRUE,
  
  # Code style preferences
  include_progress_messages = TRUE,
  include_data_verification = TRUE,
  use_compact_format = FALSE
)

debugLog("GLOBAL_SETTINGS: Global settings and configurations loaded", "APP_STARTUP")
