# Sample Size Table Module
# Calculate and display sample sizes by subgroup with highlighting and options

# Calculate sample sizes for all subgroup combinations
calculateSampleSizes <- function(data, indVar1, indVar2, indVar3) {
  
  # Build grouping variables list
  group_vars <- c(indVar1)
  if (!is.null(indVar2) && indVar2 != "") {
    group_vars <- c(group_vars, indVar2)
  }
  if (!is.null(indVar3) && indVar3 != "") {
    group_vars <- c(group_vars, indVar3)
  }
  
  # Calculate counts for observed combinations
  sample_sizes <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(n = n(), .groups = 'drop')
  
  # Create all possible combinations to include zero-count groups
  all_combinations <- data %>%
    select(all_of(group_vars)) %>%
    distinct()
  
  # Get unique values for each variable
  var_levels <- list()
  for (var in group_vars) {
    var_levels[[var]] <- unique(data[[var]])
  }
  
  # Create complete grid of all combinations
  if (length(var_levels) == 1) {
    complete_grid <- tibble(!!names(var_levels)[1] := var_levels[[1]])
  } else if (length(var_levels) == 2) {
    complete_grid <- expand_grid(
      !!names(var_levels)[1] := var_levels[[1]],
      !!names(var_levels)[2] := var_levels[[2]]
    )
  } else {
    complete_grid <- expand_grid(
      !!names(var_levels)[1] := var_levels[[1]],
      !!names(var_levels)[2] := var_levels[[2]],
      !!names(var_levels)[3] := var_levels[[3]]
    )
  }
  
  # Join with observed counts and fill zeros
  sample_sizes <- complete_grid %>%
    left_join(sample_sizes, by = group_vars) %>%
    replace_na(list(n = 0))
  
  return(sample_sizes)
}

# Count problematic subgroups (zero and single-value groups)
countProblematicSubgroups <- function(sample_sizes) {
  zero_count <- sum(sample_sizes$n == 0)
  one_count <- sum(sample_sizes$n == 1)
  
  return(list(
    zero_groups = zero_count,
    one_groups = one_count,
    has_problems = (zero_count > 0 || one_count > 0)
  ))
}

# Create styled cell with appropriate background color
createStyledCell <- function(value) {
  if (value == 0) {
    color <- sampleSizeZeroColor
  } else if (value == 1) {
    color <- sampleSizeOneColor
  } else {
    color <- sampleSizeNormalColor
  }
  
  return(paste0('<span style="background-color:', color, '; padding: 2px 8px; display: inline-block; width: 100%;">', value, '</span>'))
}

# Generate sample size table with specified layout
generateSampleSizeTable <- function(data, indVar1, indVar2, indVar3,
                                   var1Selected, var1Order,
                                   var2Selected, var2Order,
                                   var3Selected, var3Order,
                                   var3Display = "facets",  # "facets", "rows", or "cols"
                                   showMarginalTotals = FALSE,
                                   hideEmptyRowsCols = FALSE,
                                   labels = NULL) {
  
  # Clean variable names
  cleanVar2 <- if (!is.null(indVar2) && indVar2 != "") indVar2 else NULL
  cleanVar3 <- if (!is.null(indVar3) && indVar3 != "") indVar3 else NULL
  
  # Filter and reorder data
  if (!is.null(var1Selected) && length(var1Selected) > 0) {
    data <- data %>% filter(!!sym(indVar1) %in% var1Selected)
  }
  if (!is.null(cleanVar2) && !is.null(var2Selected) && length(var2Selected) > 0) {
    data <- data %>% filter(!!sym(cleanVar2) %in% var2Selected)
  }
  if (!is.null(cleanVar3) && !is.null(var3Selected) && length(var3Selected) > 0) {
    data <- data %>% filter(!!sym(cleanVar3) %in% var3Selected)
  }
  
  # Reorder factors
  if (!is.null(var1Order) && length(var1Order) > 0) {
    data[[indVar1]] <- factor(data[[indVar1]], levels = var1Order)
  }
  if (!is.null(cleanVar2) && !is.null(var2Order) && length(var2Order) > 0) {
    data[[cleanVar2]] <- factor(data[[cleanVar2]], levels = var2Order)
  }
  if (!is.null(cleanVar3) && !is.null(var3Order) && length(var3Order) > 0) {
    data[[cleanVar3]] <- factor(data[[cleanVar3]], levels = var3Order)
  }
  
  # Calculate sample sizes
  if (is.null(cleanVar2) && is.null(cleanVar3)) {
    # Only Var1: simple table
    return(generateSimpleTable(data, indVar1, showMarginalTotals, hideEmptyRowsCols, labels))
  } else if (is.null(cleanVar3)) {
    # Var1 and Var2: single contingency table
    return(generateContingencyTable(data, indVar1, cleanVar2, NULL, "none",
                                   showMarginalTotals, hideEmptyRowsCols, labels))
  } else {
    # Var1, Var2, and Var3: use specified display mode
    if (var3Display == "facets") {
      return(generateFacetedTables(data, indVar1, cleanVar2, cleanVar3,
                                  showMarginalTotals, hideEmptyRowsCols, labels))
    } else if (var3Display == "rows") {
      return(generateContingencyTable(data, indVar1, cleanVar2, cleanVar3, "rows",
                                     showMarginalTotals, hideEmptyRowsCols, labels))
    } else {
      return(generateContingencyTable(data, indVar1, cleanVar2, cleanVar3, "cols",
                                     showMarginalTotals, hideEmptyRowsCols, labels))
    }
  }
}

# Generate simple table (Var1 only)
generateSimpleTable <- function(data, indVar1, showMarginalTotals, hideEmptyRowsCols, labels) {
  
  counts <- data %>%
    group_by(!!sym(indVar1)) %>%
    summarise(n = n(), .groups = 'drop')
  
  # Convert categorical column to character to preserve labels
  counts[[indVar1]] <- as.character(counts[[indVar1]])
  
  # Create styled values
  counts$display <- sapply(counts$n, createStyledCell)
  
  # Prepare table
  result_table <- counts %>%
    select(!!sym(indVar1), display)
  
  colnames(result_table) <- c(indVar1, safeGetUILabel("sampleSizeCount", labels, "Count"))
  
  # Add total if requested
  if (showMarginalTotals) {
    total_label <- safeGetUILabel("sampleSizeTotal", labels, "Total")
    total_row <- data.frame(
      var = total_label,
      display = createStyledCell(sum(counts$n))
    )
    colnames(total_row) <- colnames(result_table)
    result_table <- rbind(result_table, total_row)
  }
  
  # Return in consistent format with metadata
  result_list <- list(table = result_table, separate_vars = FALSE)
  return(list(tables = list(result_list), facet_var = NULL))
}

# Generate contingency table (Var1 x Var2, with optional Var3)
generateContingencyTable <- function(data, indVar1, indVar2, indVar3 = NULL, 
                                    var3Mode = "none", showMarginalTotals, 
                                    hideEmptyRowsCols, labels) {
  
  # Calculate complete sample sizes (including zeros)
  sample_sizes <- calculateSampleSizes(data, indVar1, indVar2, indVar3)
  
  # Build table based on var3Mode
  if (is.null(indVar3) || var3Mode == "none") {
    # Simple case: Var1 x Var2
    wide_table <- sample_sizes %>%
      pivot_wider(names_from = !!sym(indVar2), values_from = n, values_fill = 0)
    
    # Convert categorical column to character to preserve labels
    wide_table[[indVar1]] <- as.character(wide_table[[indVar1]])
    
    result_list <- list(table = wide_table, separate_vars = FALSE, var1_col = indVar1)
    
  } else if (var3Mode == "rows") {
    # Var1 and Var3 as separate row columns
    wide_table <- sample_sizes %>%
      pivot_wider(names_from = !!sym(indVar2), values_from = n, values_fill = 0)
    
    # Convert categorical columns to character to preserve labels
    wide_table[[indVar1]] <- as.character(wide_table[[indVar1]])
    wide_table[[indVar3]] <- as.character(wide_table[[indVar3]])
    
    # Reorder columns: Var1, Var3, then data columns
    col_order <- c(indVar1, indVar3, setdiff(colnames(wide_table), c(indVar1, indVar3)))
    wide_table <- wide_table[, col_order]
    
    result_list <- list(table = wide_table, separate_vars = TRUE, var1_col = indVar1, var3_col = indVar3)
    
  } else {  # var3Mode == "cols"
    # Var2 and Var3 as separate column headers (will be handled in rendering)
    # For now, create combined columns but store metadata for separate rendering
    wide_table <- sample_sizes %>%
      mutate(combined_col = interaction(!!sym(indVar2), !!sym(indVar3), sep = "|")) %>%
      select(!!sym(indVar1), combined_col, n) %>%
      pivot_wider(names_from = combined_col, values_from = n, values_fill = 0)
    
    # Convert categorical column to character to preserve labels
    wide_table[[indVar1]] <- as.character(wide_table[[indVar1]])
    
    result_list <- list(
      table = wide_table, 
      separate_vars = TRUE, 
      var2_var3_cols = TRUE,
      var1_col = indVar1,
      var2_name = indVar2,
      var3_name = indVar3,
      var2_levels = unique(sample_sizes[[indVar2]]),
      var3_levels = unique(sample_sizes[[indVar3]])
    )
  }
  
  wide_table <- result_list$table
  
  # Filter empty rows/columns if requested
  if (hideEmptyRowsCols) {
    # Identify numeric columns
    if (var3Mode == "rows") {
      numeric_col_start <- 3  # After Var1 and Var3
    } else {
      numeric_col_start <- 2  # After first column
    }
    
    numeric_cols <- numeric_col_start:ncol(wide_table)
    
    # Remove rows where all counts are zero
    row_sums <- rowSums(wide_table[, numeric_cols, drop = FALSE])
    wide_table <- wide_table[row_sums > 0, , drop = FALSE]
    
    # Remove columns where all counts are zero
    col_sums <- colSums(wide_table[, numeric_cols, drop = FALSE])
    cols_to_keep <- c(rep(TRUE, numeric_col_start - 1), col_sums > 0)
    wide_table <- wide_table[, cols_to_keep, drop = FALSE]
  }
  
  # Add marginal totals if requested
  if (showMarginalTotals) {
    total_label <- safeGetUILabel("sampleSizeTotal", labels, "Total")
    
    # Identify where numeric columns start
    if (var3Mode == "rows") {
      numeric_col_start <- 3
      # Row totals
      row_totals <- rowSums(wide_table[, numeric_col_start:ncol(wide_table), drop = FALSE])
      wide_table[[total_label]] <- row_totals
      
      # Column totals row
      total_row <- wide_table[1, ]
      total_row[[indVar1]] <- total_label
      total_row[[indVar3]] <- ""
      for (i in numeric_col_start:ncol(wide_table)) {
        total_row[[i]] <- as.numeric(sum(wide_table[[i]], na.rm = TRUE))
      }
      # Ensure character columns remain character
      total_row[[indVar1]] <- as.character(total_row[[indVar1]])
      total_row[[indVar3]] <- as.character(total_row[[indVar3]])
      wide_table <- rbind(wide_table, total_row)
    } else {
      numeric_col_start <- 2
      # Row totals
      row_totals <- rowSums(wide_table[, numeric_col_start:ncol(wide_table), drop = FALSE])
      wide_table[[total_label]] <- row_totals
      
      # Column totals row
      total_row <- wide_table[1, ]
      total_row[[1]] <- total_label
      for (i in numeric_col_start:ncol(wide_table)) {
        total_row[[i]] <- as.numeric(sum(wide_table[[i]], na.rm = TRUE))
      }
      # Ensure character column remains character
      total_row[[1]] <- as.character(total_row[[1]])
      wide_table <- rbind(wide_table, total_row)
    }
  }
  
  # Apply styling to numeric columns
  style_start <- if (var3Mode == "rows") 3 else 2
  for (i in style_start:ncol(wide_table)) {
    wide_table[[i]] <- sapply(as.numeric(wide_table[[i]]), createStyledCell)
  }
  
  result_list$table <- wide_table
  return(list(tables = list(result_list), facet_var = NULL))
}

# Generate faceted tables (one table per Var3 modality)
generateFacetedTables <- function(data, indVar1, indVar2, indVar3,
                                 showMarginalTotals, hideEmptyRowsCols, labels) {
  
  # Calculate complete sample sizes (including zeros)
  sample_sizes <- calculateSampleSizes(data, indVar1, indVar2, indVar3)
  
  var3_levels <- unique(sample_sizes[[indVar3]])
  tables_list <- list()
  
  for (level in var3_levels) {
    subset_data <- sample_sizes %>% filter(!!sym(indVar3) == level)
    
    # Create contingency table for this subset
    wide_table <- subset_data %>%
      select(!!sym(indVar1), !!sym(indVar2), n) %>%
      pivot_wider(names_from = !!sym(indVar2), values_from = n, values_fill = 0)
    
    # Convert categorical column to character to preserve labels
    wide_table[[indVar1]] <- as.character(wide_table[[indVar1]])
    
    # Filter empty rows/columns if requested
    if (hideEmptyRowsCols) {
      numeric_cols <- 2:ncol(wide_table)
      
      # Remove rows where all counts are zero
      row_sums <- rowSums(wide_table[, numeric_cols, drop = FALSE])
      wide_table <- wide_table[row_sums > 0, , drop = FALSE]
      
      # Remove columns where all counts are zero
      col_sums <- colSums(wide_table[, numeric_cols, drop = FALSE])
      cols_to_keep <- c(TRUE, col_sums > 0)
      wide_table <- wide_table[, cols_to_keep, drop = FALSE]
    }
    
    # Add marginal totals if requested
    if (showMarginalTotals) {
      total_label <- safeGetUILabel("sampleSizeTotal", labels, "Total")
      
      # Row totals
      row_totals <- rowSums(wide_table[, -1, drop = FALSE])
      wide_table[[total_label]] <- row_totals
      
      # Column totals row
      total_row <- wide_table[1, ]
      total_row[[1]] <- total_label
      for (i in 2:ncol(wide_table)) {
        total_row[[i]] <- as.numeric(sum(wide_table[[i]], na.rm = TRUE))
      }
      # Ensure character column remains character
      total_row[[1]] <- as.character(total_row[[1]])
      wide_table <- rbind(wide_table, total_row)
    }
    
    # Apply styling to numeric columns
    for (i in 2:ncol(wide_table)) {
      wide_table[[i]] <- sapply(as.numeric(wide_table[[i]]), createStyledCell)
    }
    
    # Preserve original level name (not converted to character which might become numeric)
    level_name <- if (is.factor(level)) as.character(level) else level
    tables_list[[level_name]] <- wide_table
  }
  
  return(list(tables = tables_list, facet_var = indVar3))
}

# Render sample size table with DT or HTML
renderSampleSizeTable <- function(table_data, labels, tables_per_row = 1) {
  
  if (is.null(table_data) || is.null(table_data$tables)) {
    return(NULL)
  }
  
  # If faceted, create HTML tables manually
  if (!is.null(table_data$facet_var) && length(table_data$tables) > 1) {
    # Create HTML for multiple tables with configurable layout
    html_parts <- list()
    var3_name <- table_data$facet_var
    
    # Wrapper for grid layout
    grid_style <- paste0('display: grid; grid-template-columns: repeat(', tables_per_row, 
                        ', 1fr); gap: 20px; margin-bottom: 20px;')
    html_output <- paste0('<div style="', grid_style, '">')
    
    counter <- 0
    for (facet_name in names(table_data$tables)) {
      counter <- counter + 1
      
      # Build HTML table manually
      current_table <- table_data$tables[[facet_name]]
      
      html_table <- paste0(
        '<div style="margin-bottom: 20px;">',
        '<h5 style="margin-bottom: 10px;"><strong>', var3_name, ':</strong> ', facet_name, '</h5>',
        '<table class="table table-bordered table-condensed" style="width: 100%;">',
        '<thead><tr>'
      )
      
      # Headers
      for (col_name in colnames(current_table)) {
        html_table <- paste0(html_table, '<th>', col_name, '</th>')
      }
      html_table <- paste0(html_table, '</tr></thead><tbody>')
      
      # Rows
      for (i in 1:nrow(current_table)) {
        html_table <- paste0(html_table, '<tr>')
        for (j in 1:ncol(current_table)) {
          cell_value <- as.character(current_table[i, j])
          html_table <- paste0(html_table, '<td>', cell_value, '</td>')
        }
        html_table <- paste0(html_table, '</tr>')
      }
      
      html_table <- paste0(html_table, '</tbody></table></div>')
      html_output <- paste0(html_output, html_table)
      
      # Start new row if needed
      if (counter %% tables_per_row == 0 && counter < length(table_data$tables)) {
        html_output <- paste0(html_output, '</div><div style="', grid_style, '">')
      }
    }
    
    html_output <- paste0(html_output, '</div>')
    return(HTML(html_output))
    
  } else {
    # Single table
    table_info <- table_data$tables[[1]]
    
    # Check if it has metadata for separate vars rendering
    if (is.list(table_info) && !is.null(table_info$separate_vars) && table_info$separate_vars) {
      # Render with borders using HTML
      current_table <- table_info$table
      
      # Check if this is Var2×Var3 in columns mode
      if (!is.null(table_info$var2_var3_cols) && table_info$var2_var3_cols) {
        # Mode cols: two-row header with Var2 and Var3
        html_table <- '<table class="table table-bordered table-condensed" style="width: auto;"><thead>'
        
        # Parse column names to extract Var2 and Var3 values
        col_names <- colnames(current_table)
        data_col_names <- col_names[-1]  # Exclude first column (Var1)
        
        # Split combined names (format: "Var2Value|Var3Value")
        var2_vals <- character()
        var3_vals <- character()
        for (col_name in data_col_names) {
          parts <- strsplit(col_name, "|", fixed = TRUE)[[1]]
          var2_vals <- c(var2_vals, parts[1])
          var3_vals <- c(var3_vals, parts[2])
        }
        
        # Count consecutive occurrences for colspan
        var2_counts <- rle(var2_vals)
        var3_name <- table_info$var3_name
        var2_name <- table_info$var2_name
        
        # First header row: Var2 with colspan
        html_table <- paste0(html_table, '<tr><th rowspan="2">', table_info$var1_col, '</th>')
        for (i in 1:length(var2_counts$values)) {
          html_table <- paste0(html_table, '<th colspan="', var2_counts$lengths[i], '">', 
                              var2_counts$values[i], '</th>')
        }
        html_table <- paste0(html_table, '</tr>')
        
        # Second header row: Var3 values
        html_table <- paste0(html_table, '<tr>')
        for (var3_val in var3_vals) {
          html_table <- paste0(html_table, '<th>', var3_val, '</th>')
        }
        html_table <- paste0(html_table, '</tr></thead><tbody>')
        
      } else if (!is.null(table_info$var3_col)) {
        # Mode rows: separate columns for Var1 and Var3 with rowspan for Var1
        html_table <- '<table class="table table-bordered table-condensed" style="width: auto;"><thead><tr>'
        
        # Headers
        for (col_name in colnames(current_table)) {
          html_table <- paste0(html_table, '<th>', col_name, '</th>')
        }
        html_table <- paste0(html_table, '</tr></thead><tbody>')
        
        # Rows with rowspan for Var1
        var1_col <- table_info$var1_col
        var3_col <- table_info$var3_col
        current_var1 <- NULL
        var1_count <- 0
        
        # First pass: count consecutive Var1 values
        var1_spans <- list()
        for (i in 1:nrow(current_table)) {
          var1_value <- as.character(current_table[i, var1_col])
          if (is.null(current_var1) || var1_value != current_var1) {
            if (!is.null(current_var1)) {
              var1_spans[[current_var1]] <- var1_count
            }
            current_var1 <- var1_value
            var1_count <- 1
          } else {
            var1_count <- var1_count + 1
          }
        }
        if (!is.null(current_var1)) {
          var1_spans[[current_var1]] <- var1_count
        }
        
        # Second pass: render with rowspan
        current_var1 <- NULL
        for (i in 1:nrow(current_table)) {
          html_table <- paste0(html_table, '<tr>')
          
          var1_value <- as.character(current_table[i, var1_col])
          
          # Add Var1 cell with rowspan only on first occurrence
          if (is.null(current_var1) || var1_value != current_var1) {
            rowspan <- var1_spans[[var1_value]]
            html_table <- paste0(html_table, '<td rowspan="', rowspan, '">', var1_value, '</td>')
            current_var1 <- var1_value
          }
          
          # Add remaining cells
          for (j in 2:ncol(current_table)) {
            cell_value <- as.character(current_table[i, j])
            html_table <- paste0(html_table, '<td>', cell_value, '</td>')
          }
          html_table <- paste0(html_table, '</tr>')
        }
        
        html_table <- paste0(html_table, '</tbody></table>')
        return(HTML(html_table))
        
      } else {
        # Simple case with borders
        html_table <- '<table class="table table-bordered table-condensed" style="width: auto;"><thead><tr>'
        
        # Headers
        for (col_name in colnames(current_table)) {
          html_table <- paste0(html_table, '<th>', col_name, '</th>')
        }
        html_table <- paste0(html_table, '</tr></thead><tbody>')
      }
      
      # Rows (for cols mode and simple mode)
      for (i in 1:nrow(current_table)) {
        html_table <- paste0(html_table, '<tr>')
        for (j in 1:ncol(current_table)) {
          cell_value <- as.character(current_table[i, j])
          html_table <- paste0(html_table, '<td>', cell_value, '</td>')
        }
        html_table <- paste0(html_table, '</tr>')
      }
      
      html_table <- paste0(html_table, '</tbody></table>')
      return(HTML(html_table))
    } else {
      # Simple rendering with HTML and borders for consistency
      display_table <- if (is.list(table_info)) table_info$table else table_info
      
      html_table <- '<table class="table table-bordered table-condensed" style="width: auto;"><thead><tr>'
      
      # Headers
      for (col_name in colnames(display_table)) {
        html_table <- paste0(html_table, '<th>', col_name, '</th>')
      }
      html_table <- paste0(html_table, '</tr></thead><tbody>')
      
      # Rows
      for (i in 1:nrow(display_table)) {
        html_table <- paste0(html_table, '<tr>')
        for (j in 1:ncol(display_table)) {
          cell_value <- as.character(display_table[i, j])
          html_table <- paste0(html_table, '<td>', cell_value, '</td>')
        }
        html_table <- paste0(html_table, '</tr>')
      }
      
      html_table <- paste0(html_table, '</tbody></table>')
      return(HTML(html_table))
    }
  }
}

debugLog("SAMPLE_SIZE_TABLE: Sample size table module loaded", "MODULE_LOAD")
