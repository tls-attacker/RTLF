#!/usr/bin/env Rscript

# RTLF - R-Time-Leak-Finder
# A tool to statistically evaluate timing measurements

# Check for required packages and install if missing
required_packages <- c("tidyverse", "optparse", "jsonlite", "crayon", "parallel")
missing_packages <- required_packages[!sapply(required_packages, function(pkg) requireNamespace(pkg, quietly = TRUE))]

if (length(missing_packages) > 0) {
  cat("Missing required R packages:", paste(missing_packages, collapse=", "), "\n")
  
  # Ask if user wants to install packages
  cat("Do you want to install the missing packages? (y/n): ")
  user_input <- tolower(readline())
  
  if (user_input == "y" || user_input == "yes") {
    cat("Installing missing packages...\n")
    install.packages(missing_packages)
    
    # Check if installation was successful
    still_missing <- missing_packages[!sapply(missing_packages, function(pkg) requireNamespace(pkg, quietly = TRUE))]
    if (length(still_missing) > 0) {
      cat("ERROR: Failed to install packages:", paste(still_missing, collapse=", "), "\n")
      quit(status = 1)
    }
    cat("All required packages installed successfully.\n")
  } else {
    cat("Required packages are missing. Please install them manually using:\n")
    cat("  install.packages(c(\"", paste(missing_packages, collapse="\", \""), "\"))\n", sep="")
    quit(status = 1)
  }
}

# Suppress startup messages
suppressPackageStartupMessages({
  library(tidyverse)
  library(optparse)
  library(jsonlite)
  library(crayon)
  library(parallel)
})

# Define color functions for output
heading <- bold $ blue       # Section headings
subheading <- bold $ cyan    # Subsection headings
success <- bold $ green      # Success messages
warning_color <- bold $ yellow  # Warning messages
error_color <- bold $ red    # Error messages
highlight <- bold $ magenta  # Important data
info <- bold $ white         # Standard info
result_yes <- bold $ red     # Statistical significance (changed from green to red)
result_no <- bold $ green    # No statistical significance (changed from silver to green)
table_header <- bold $ white # Table headers

# Suppress message output
options(tidyverse.quiet = TRUE)

#-------------------------------------------------------------------------------
# CLI Interface

# Define command-line options
option_list <- list(
  make_option(c("-a", "--alpha"), type="double", default=0.09, 
              help="Threshold for type-1 error rate (e.g., 0.09 for 9%% threshold) [default: %default]"),
  make_option(c("-i", "--input"), type="character", default=NULL, 
              help="Input CSV file or directory with timing measurements"),
  make_option(c("-o", "--output"), type="character", default=NULL, 
              help="Output file name or directory (supports .RDATA, .json, or .csv extension)"),
  make_option(c("-p", "--pattern"), type="character", default="*.csv", 
              help="File pattern to match when input is a directory (e.g., \"*.csv\") [default: %default]"),
  make_option(c("-r", "--recursive"), action="store_true", default=FALSE, 
              help="Recursively process subdirectories when input is a directory"),
# Format option removed - auto-detection is now the default
  make_option(c("-q", "--quiet"), action="store_true", default=FALSE, 
              help="Suppress detailed analysis results"),
  make_option(c("-t", "--threads"), type="integer", default=0, 
              help="Number of parallel threads to use when processing multiple files (0 = auto-detect) [default: %default]")
)

# Parse command-line arguments
opt_parser <- OptionParser(option_list=option_list, 
                           description="RTLF: Statistical analysis tool for timing measurements\nNote: Headers and series names are automatically detected from the input file.")
opt <- parse_args(opt_parser)

# Validate arguments
if (is.null(opt$input)) {
  stop("Input file or directory must be specified. Use --help for more information.")
}

if (is.na(opt$alpha) || opt$alpha >= 1 || opt$alpha <= 0) {
  stop("Alpha should be a decimal number between 0 and 1")
}

# Set alpha per decile
alphaPerDecile <- opt$alpha / 9

# Function to list files in a directory matching a pattern
list_files_in_dir <- function(dir_path, pattern, recursive = FALSE) {
  if (!dir.exists(dir_path)) {
    stop("Directory does not exist: ", dir_path)
  }
  
  # Use recursive flag if specified
  files <- list.files(
    path = dir_path, 
    pattern = pattern, 
    full.names = TRUE, 
    recursive = recursive
  )
  
  # Filter out directories from the results
  files <- files[!file.info(files)$isdir]
  
  if (length(files) == 0) {
    stop("No files matching pattern '", pattern, "' found in directory: ", dir_path)
  }
  
  return(files)
}

# Check if input is a directory or file
is_input_dir <- dir.exists(opt$input)

# Process directory or single file
if (is_input_dir) {
  # Input is a directory
  input_files <- list_files_in_dir(opt$input, opt$pattern, opt$recursive)
  
  # Ensure output is a directory if input is a directory
  if (!is.null(opt$output) && !dir.exists(opt$output)) {
    # Check if output is a file path with extension
    if (!dir.exists(opt$output) && grepl("\\.[^\\.]+$", opt$output)) {
      # Extract directory part
      output_dir <- dirname(opt$output)
      # Create directory if it doesn't exist
      if (!dir.exists(output_dir) && output_dir != ".") {
        dir.create(output_dir, recursive = TRUE)
      }
    } else {
      # Create output directory if it doesn't exist
      dir.create(opt$output, recursive = TRUE)
      if (!dir.exists(opt$output)) {
        stop("Failed to create output directory: ", opt$output)
      }
    }
  } else if (is.null(opt$output)) {
    # For directory processing without explicit output, don't set a default output
    # This ensures results won't be saved unless explicitly requested
  }
  
  # Fix file pattern - remove any extra spaces
  # Convert pattern to a proper regex for file.info filtering
  clean_pattern <- gsub("\\*", ".*", opt$pattern)
  clean_pattern <- gsub("\\.", "\\\\.", clean_pattern)
  
  # Filter out any non-CSV files and RDATA result files
  input_files <- input_files[!grepl("\\.RDATA$", input_files)]
  
  # Display information about files to process
  if (!opt$quiet) {
    cat(heading("\n============ RTLF Directory Processing ============\n\n"))
    cat(subheading("Processing directory:"), highlight(opt$input), "\n")
    cat(subheading("Found"), highlight(length(input_files)), 
        subheading("files matching pattern"), highlight(paste0("'", opt$pattern, "'")), "\n")
    if (opt$recursive) {
      cat(info("Recursive mode enabled, including subdirectories"), "\n")
    }
    cat("\n")
  }
} else {
  # Input is a single file
  if (!file.exists(opt$input)) {
    stop("Input file does not exist: ", opt$input)
  }
  
  input_files <- opt$input
  
  # Determine output file if not specified
  if (is.null(opt$output)) {
    opt$output <- paste0(opt$input, ".result-rtlf-alpha-", opt$alpha * 100, "%.RDATA")
  }
}

#-------------------------------------------------------------------------------
# Data Input Handling

read_input_data <- function(file_path) {
  # Auto-detect if file has a header and determine the separator
  # Check first few lines to detect if header is present
  first_lines <- readLines(file_path, n = 5)
  
  # Auto-detect delimiter (comma or semicolon) - check all sample lines
  comma_count <- 0
  semicolon_count <- 0
  
  # Count separators in first few lines
  for (line in first_lines) {
    comma_count <- comma_count + nchar(gsub("[^,]", "", line))
    semicolon_count <- semicolon_count + nchar(gsub("[^;]", "", line))
  }
  
  # Determine which separator is more common
  separator <- if (semicolon_count > comma_count) ";" else ","
  
  # Try to detect if the first row is a header using the detected separator
  first_row <- read.csv(text = first_lines[1], header = FALSE, stringsAsFactors = FALSE, sep = separator)
  second_row <- if(length(first_lines) > 1) read.csv(text = first_lines[2], header = FALSE, stringsAsFactors = FALSE, sep = separator) else NULL
  
  # Heuristics to detect header - if first row has text and second row has numbers, it's likely a header
  has_header <- FALSE
  
  # Check if first row is all character and second row has at least one numeric value
  if (!is.null(second_row)) {
    first_row_all_char <- all(sapply(first_row, function(x) !suppressWarnings(all(!is.na(as.numeric(x))))))
    second_row_has_num <- any(sapply(second_row, function(x) suppressWarnings(!is.na(as.numeric(x)))))
    
    if (first_row_all_char && second_row_has_num) {
      has_header <- TRUE
    }
    
    # Check for common header names like "Type", "Value", "Measurement", etc.
    header_keywords <- c("type", "value", "measurement", "series", "run", "x", "y")
    if (any(tolower(unlist(first_row)) %in% header_keywords)) {
      has_header <- TRUE
    }
  }
  
  # Read CSV file with the determined separator and header setting
  if (has_header) {
    data <- read.csv(file = file_path, header = TRUE, sep = separator)
  } else {
    data <- read.csv(file = file_path, header = FALSE, sep = separator)
    colnames(data) <- paste0("V", seq_len(ncol(data)))
  }
  
  # Auto-detect series names
  series1 <- "X"
  series2 <- "Y"
  
  # Look for series names in the data
  if (ncol(data) > 0) {
    # For row format, look at first column values
    if (has_header && colnames(data)[1] == "Measurement") {
      # Extract unique values from the first column (excluding header)
      unique_values <- unique(data[[1]])
      if (length(unique_values) == 2) {
        series1 <- unique_values[1]
        series2 <- unique_values[2]
      }
    }
    
    # For single-row format, look at unique values in the first column
    if (has_header && ncol(data) == 2 && colnames(data)[1] == "Type") {
      unique_values <- unique(data[[1]])
      if (length(unique_values) == 2) {
        series1 <- unique_values[1]
        series2 <- unique_values[2]
      }
    }
    
    # For columns format, use column names
    if (has_header && ncol(data) >= 2 && 
        is.numeric(data[[1]]) && is.numeric(data[[2]])) {
      series1 <- colnames(data)[1]
      series2 <- colnames(data)[2]
    }
  }
  
  # Auto-detect format
  format <- "classic" # Default format
  
  # Check if we have a Type/Value format (single-row format)
  # This must be checked FIRST because it has similar patterns to classic format
  if (ncol(data) == 2 && has_header && 
      (colnames(data)[1] == "Type" || 
       all(unique(data[[1]]) %in% c(series1, series2)))) {
    format <- "single-row"
  }
  # Check if data has X/Y format (classic)
  else if (all(unique(na.omit(data[,1])) %in% c("X", "Y"))) {
    format <- "classic"
    series1 <- "X"
    series2 <- "Y"
  }
  # Check if we have two numeric columns (columns format)
  else if (ncol(data) >= 2) {
    # Check if first column is numeric or empty
    is_col1_numeric <- all(!is.na(suppressWarnings(as.numeric(na.omit(data[[1]])))))
    is_col1_empty <- length(na.omit(data[[1]])) == 0
    # Check if second column is numeric or empty
    is_col2_numeric <- all(!is.na(suppressWarnings(as.numeric(na.omit(data[[2]])))))
    is_col2_empty <- length(na.omit(data[[2]])) == 0
    
    # If both columns are either numeric or empty, it's a columns format
    if ((is_col1_numeric || is_col1_empty) && (is_col2_numeric || is_col2_empty)) {
      format <- "columns"
    }
  }
  
  # Process data based on format
  if (format == "classic") {
    # Classic X/Y format
    # First check for rows with all empty values in measurement columns
    data_clean <- data
    if (ncol(data) > 1) {
      data_clean <- data[rowSums(is.na(data[, -1]) | data[, -1] == "" | data[, -1] == 0, na.rm = TRUE) < (ncol(data) - 1), ]
    }
    
    # Make sure we still have data after removing rows with all empty measurements
    if (nrow(data_clean) == 0) {
      stop("No valid data found after removing rows with all empty or zero measurements")
    }
    
    namedData <- data_clean %>% 
      mutate(V1 = recode(!!sym(colnames(data_clean)[1]), "X" = "1", "Y" = "2"))
    
    # Create the V2 column from the first measurement column with data
    for (i in 2:ncol(data_clean)) {
      col_name <- colnames(data_clean)[i]
      namedData$V2 <- data_clean[[i]]
      # If we found a column with valid data, break
      if (sum(!is.na(namedData$V2) & namedData$V2 != "" & namedData$V2 != 0) > 0) {
        break
      }
    }
  } 
  else if (format == "columns") {
    # Convert two-column format to X/Y format
    col1 <- colnames(data)[1]
    col2 <- colnames(data)[2]
    
    # Create new dataset in the required format
    namedData <- data.frame(
      V1 = c(rep("1", nrow(data)), rep("2", nrow(data))),
      V2 = c(data[[col1]], data[[col2]])
    )
  }
  # Rows format removed
  else if (format == "single-row") {
    # One row per measurement format (Type, Value)
    # Rename columns to match expected format
    type_col_name <- colnames(data)[1]
    value_col_name <- colnames(data)[2]
    
    # Convert series names to expected format (1 for series1, 2 for series2)
    namedData <- data %>%
      rename(V1 = !!sym(type_col_name), V2 = !!sym(value_col_name)) %>%
      mutate(V1 = recode(V1, !!series1 := "1", !!series2 := "2")) %>%
      filter(V1 %in% c("1", "2"))  # Keep only the specified series
  }
  else {
    stop("Unknown format: ", format)
  }
  
  # Convert V2 to numeric if it's not already
  if (!is.numeric(namedData$V2)) {
    namedData$V2 <- as.numeric(namedData$V2)
  }
  
  # Count how many empty/zero cells were in the original data
  original_data_empty_count <- 0
  if (ncol(data) > 1) {
    # For each measurement column, count empty/zero values
    for (i in 2:ncol(data)) {
      # Skip the first column (X/Y identifiers)
      original_data_empty_count <- original_data_empty_count + 
        sum(is.na(data[[i]]) | data[[i]] == "" | data[[i]] == "0" | data[[i]] == 0, na.rm = TRUE)
    }
  }
  
  # Check for empty or zero values in the processed data
  empty_values <- is.na(namedData$V2) | namedData$V2 == 0
  
  # Combine the counts from original data and remaining empty values
  total_empty_count <- original_data_empty_count + sum(empty_values)
  
  if (total_empty_count > 0) {
    cat(warning_color("\nWARNING:"), "Found", total_empty_count, "empty or zero values that will be ignored\n")
    
    # Remove empty or zero values from the current data
    namedData <- namedData %>% filter(!is.na(V2) & V2 != 0)
    
    # Verify we still have data for both series
    series1_count <- sum(namedData$V1 == "1")
    series2_count <- sum(namedData$V1 == "2")
    
    if (series1_count == 0) {
      stop("All values for series X are empty or zero. Cannot proceed with analysis.")
    }
    if (series2_count == 0) {
      stop("All values for series Y are empty or zero. Cannot proceed with analysis.")
    }
    
    cat(info("  Remaining measurements:"), "Series X:", highlight(series1_count), 
        "Series Y:", highlight(series2_count), "\n\n")
  }
  
  return(namedData)
}

#-------------------------------------------------------------------------------
# Analysis Functions

# Test function: compares quantiles between two distributions
test <- function(td1, td2) {
  # Check for empty or invalid inputs
  if (length(td1) == 0 || length(td2) == 0 || 
      all(is.na(td1)) || all(is.na(td2))) {
    return(rep(0, 9))
  }
  
  # Remove NAs
  td1 <- td1[!is.na(td1)]
  td2 <- td2[!is.na(td2)]
  
  # If either dataset is too small, pad with values
  if (length(td1) < 2) {
    td1 <- rep(td1, 2)
  }
  if (length(td2) < 2) {
    td2 <- rep(td2, 2)
  }
  
  q1 <- quantile(td1, probs = seq(0.1, 0.9, 0.1), names = FALSE, type = 2)
  q2 <- quantile(td2, probs = seq(0.1, 0.9, 0.1), names = FALSE, type = 2)
  t1 <- abs(q1 - q2)
  return(t1)
}

# Bootstrap function: samples with replacement
bootstrap1 <- function(dat, n) {
  # Ensure dat has at least one element and isn't NA
  if (length(dat) == 0 || all(is.na(dat))) {
    # Return zeros if the data is empty
    return(rep(0, 9))
  }
  
  # Remove any NAs from the data
  dat <- dat[!is.na(dat)]
  
  # If n is larger than the data we have, reduce n to match the data length
  n <- min(n, length(dat))
  
  # If we only have 1 element, duplicate it to avoid sampling issues
  if (length(dat) == 1) {
    dat <- rep(dat, 2)
  }
  
  x1 <- sample(dat, n, replace = TRUE)
  x2 <- sample(dat, n, replace = TRUE)
  return(test(x1, x2))
}

# Main test function
autotest <- function(data, n, B, alpha_per_decile) {
  bb1 <- data %>% select(V1, V2) %>% filter(V1 == "1")
  bb2 <- data %>% select(V1, V2) %>% filter(V1 == "2")
  
  # Ensure we have enough data to proceed
  if (length(bb1$V2) < 100 || length(bb2$V2) < 100) {
    stop("Insufficient data after filtering empty/zero values. Need at least one data point per series.")
  }
  
  # Make sure n is at least 1
  n <- max(100, n)
  
  # Replicate bootstrap B times
  q1 <- replicate(B, bootstrap1(as.numeric(bb1$V2), n))
  q2 <- replicate(B, bootstrap1(as.numeric(bb2$V2), n))
  
  # Calculate quantiles
  maxq1 <- apply(q1, 1, quantile, probs = 1 - alpha_per_decile, type = 2)
  maxq2 <- apply(q2, 1, quantile, probs = 1 - alpha_per_decile, type = 2)
  
  # Maximum of both quantiles
  maxqs <- matrix(c(maxq1, maxq2), nrow = 9, ncol = 2)
  qmax <- apply(maxqs, 1, max)
  
  # Test data
  t <- test(as.numeric(bb1$V2), as.numeric(bb2$V2))
  
  # Decision vector
  dec <- rep(0, 9)
  for (l in 1:9) {
    if (t[l] > qmax[l]) {
      dec[l] <- dec[l] + 1
    }
  }
  
  # Return result list
  return(list(dec, t, qmax, maxq1, maxq2))
}

#-------------------------------------------------------------------------------
# Output Handling

format_output <- function(output, verbose=FALSE, input_data=NULL) {
  deciles <- seq(10, 90, 10)
  decision <- output[[1]]
  difference <- output[[2]]
  threshold <- output[[3]]
  threshold_x <- output[[4]]
  threshold_y <- output[[5]]
  
  # Use passed data parameter if available, otherwise use global data
  if (is.null(input_data)) {
    if (exists("data", inherits = FALSE)) {
      input_data <- data  # Use global data if available
    } else {
      stop("No data provided to format_output function")
    }
  }
  
  # Calculate basic statistics for series X and Y
  series_x_count <- length((input_data %>% filter(V1 == "1"))$V2)
  series_y_count <- length((input_data %>% filter(V1 == "2"))$V2)
  
  # Calculate statistics for series X
  series_x_values <- as.numeric((input_data %>% filter(V1 == "1"))$V2)
  series_x_min <- min(series_x_values)
  series_x_max <- max(series_x_values)
  series_x_mean <- mean(series_x_values)
  series_x_median <- median(series_x_values)
  series_x_stdev <- sd(series_x_values)
  
  # Calculate statistics for series Y
  series_y_values <- as.numeric((input_data %>% filter(V1 == "2"))$V2)
  series_y_min <- min(series_y_values)
  series_y_max <- max(series_y_values)
  series_y_mean <- mean(series_y_values)
  series_y_median <- median(series_y_values)
  series_y_stdev <- sd(series_y_values)
  
  # Save statistics for JSON output
  stats <- list(
    series_x = list(
      count = series_x_count,
      min = series_x_min,
      max = series_x_max,
      mean = series_x_mean,
      median = series_x_median,
      stdev = series_x_stdev
    ),
    series_y = list(
      count = series_y_count,
      min = series_y_min,
      max = series_y_max,
      mean = series_y_mean,
      median = series_y_median,
      stdev = series_y_stdev
    )
  )
  
  if (verbose) {
    
    # Dataset Information
    cat(sprintf("- Series X: %s measurements\n", highlight(series_x_count)))
    cat(sprintf("- Series Y: %s measurements\n", highlight(series_y_count)))
    cat(sprintf("- Total: %s measurements\n", highlight(series_x_count + series_y_count)))
    cat("\n")
    
    # Basic Statistics
    cat(subheading("Basic Statistics:"), "\n")
    cat(info("---------------------------------------------------------------------------"), "\n")
    cat(table_header("Series          Min             Max             Mean          Std Dev     \n"))
    cat(info("---------------------------------------------------------------------------"), "\n")
    
    # Fixed table format with exact string positioning
    cat(highlight("X"), "     ", 
        format(sprintf("%12s", format(series_x_min, digits=4))), "  ",
        format(sprintf("%12s", format(series_x_max, digits=4))), "  ",
        format(sprintf("%12s", format(series_x_mean, digits=4))), "  ",
        format(sprintf("%12s", format(series_x_stdev, digits=4))), "\n")
    
    cat(highlight("Y"), "     ", 
        format(sprintf("%12s", format(series_y_min, digits=4))), "  ",
        format(sprintf("%12s", format(series_y_max, digits=4))), "  ",
        format(sprintf("%12s", format(series_y_mean, digits=4))), "  ",
        format(sprintf("%12s", format(series_y_stdev, digits=4))), "\n")
    cat(info("---------------------------------------------------------------------------"), "\n")
       
    # Detailed results table
    cat(info("------------------------------------------------------------"), "\n")
    cat(table_header(sprintf("%-10s %-12s %-12s %-18s\n", 
                "Decile", "Difference", "Threshold", "Significant?")))
    cat(info("------------------------------------------------------------"), "\n")
    
    for (i in 1:9) {
      # Use colors based on the significance of the result
      if (decision[i] > 0) {
        significant_text <- result_yes("YES")
        decile_text <- highlight(paste0(deciles[i], "%"))
      } else {
        significant_text <- result_no("no")
        decile_text <- highlight(paste0(deciles[i], "%"))
      }
      
      # Fixed spacing issues by using explicit concatenation
      cat(paste0(decile_text, "  "), 
          format(difference[i], digits=4, width=12), " ",
          format(threshold[i], digits=4, width=12), "     ", # Increased spacing
          significant_text, "\n")
    }
    cat(info("------------------------------------------------------------"), "\n\n")
    
    # Final decision
    cat(subheading("Final Decision: "))
    if (max(decision) > 0) {
      cat(result_yes("Statistically significant difference detected."), "\n\n")
    } else {
      cat(result_no("No statistically significant difference detected."), "\n\n")
    }
  }
  
  # Create data frame for output (Decision column removed)
  result_df <- data.frame(
    Decile = paste0(deciles, "%"),
    Difference = difference,
    Threshold = threshold,
    ThresholdX = threshold_x,
    ThresholdY = threshold_y
  )
  
  # Return both the result dataframe and the statistics
  return(list(results = result_df, statistics = stats))
}

save_output <- function(output, output_file, results_object, samples_count, bootstrap_iterations) {
  # Determine file format based on extension
  ext <- tolower(tools::file_ext(output_file))
  
  # Extract results dataframe and statistics
  data_frame <- results_object$results
  statistics <- results_object$statistics
  
  if (ext == "rdata") {
    save(output, file = output_file)
  } else if (ext == "json") {
    # Create enhanced JSON output with metadata
    deciles <- seq(10, 90, 10)
    decision <- output[[1]]
    
    # Create summary information
    difference_detected <- max(decision) > 0
    significant_deciles <- if (difference_detected) {
      deciles[which(decision > 0)]
    } else {
      numeric(0)
    }
    
    # Determine input file name for the JSON metadata
    input_file_name <- if (is_input_dir) {
      basename(output_file) # Use the output filename as a reference to original
    } else {
      opt$input
    }
    
    # Create enhanced JSON structure
    enhanced_json <- list(
      metadata = list(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        alpha = alphaPerDecile * 9,
        input_file = input_file_name,
        samples = samples_count,
        bootstrap_iterations = bootstrap_iterations,
        difference_detected = difference_detected,
        significant_deciles = significant_deciles,
        exit_code = if (difference_detected) 11 else 10
      ),
      statistics = statistics,
      results = lapply(1:nrow(data_frame), function(i) {
        row <- data_frame[i,]
        as.list(row)
      })
    )
    
    write_json(enhanced_json, output_file, pretty = TRUE, auto_unbox = TRUE)
  } else if (ext == "csv") {
    # For CSV output, just include the analysis results without statistics
    write.csv(data_frame, output_file, row.names = FALSE)
    
    # Create an additional CSV file with statistics
    stats_file <- paste0(tools::file_path_sans_ext(output_file), "-stats.csv")
    
    # Convert statistics to a data frame
    stats_df <- data.frame(
      Series = c("X", "Y"),
      Count = c(statistics$series_x$count, statistics$series_y$count),
      Min = c(statistics$series_x$min, statistics$series_y$min),
      Max = c(statistics$series_x$max, statistics$series_y$max),
      Mean = c(statistics$series_x$mean, statistics$series_y$mean),
      Median = c(statistics$series_x$median, statistics$series_y$median),
      StdDev = c(statistics$series_x$stdev, statistics$series_y$stdev)
    )
    
    write.csv(stats_df, stats_file, row.names = FALSE)
    
    if (!opt$quiet) {
      cat(info("Statistics saved to:"), highlight(stats_file), "\n")
    }
  } else {
    # Default to RDATA
    output_file <- paste0(tools::file_path_sans_ext(output_file), ".RDATA")
    save(output, file = output_file)
  }
}

#-------------------------------------------------------------------------------
# Main Program

# Function to process a single file
process_file <- function(input_file, output_path) {
  # Start timing execution
  start_time <- Sys.time()
  
  if (!opt$quiet) {
	cat("\n")
    cat(heading("============================================"), "\n")
    cat(subheading("Processing file:"), highlight(input_file), "\n")
    cat(heading("============================================"), "\n\n")
  }
  
  # Determine output file path
  if (is_input_dir) {
    # For directory processing, only set output file if output parameter is explicitly provided
    if (!is.null(opt$output)) {
      file_name <- basename(input_file)
      
      # Determine output extension from output parameter or use default
      if (!dir.exists(opt$output) && grepl("\\.[^\\.]+$", opt$output)) {
        output_path <- dirname(opt$output)
        output_ext <- tools::file_ext(opt$output)
      } else {
        output_path <- opt$output
        output_ext <- "RDATA"  # Default
      }
      
      # If extension is empty, use default
      if (output_ext == "") output_ext <- "RDATA"
      
      # Check if output_path contains the extension already
      if (grepl("\\.[^\\.]+$", opt$output) && !dir.exists(opt$output)) {
        # Use the filename from input but place in the directory of output
        output_dir <- dirname(opt$output)
        output_file <- normalizePath(file.path(
          output_dir, 
          paste0(tools::file_path_sans_ext(file_name), 
                 ".result-rtlf-alpha-", 
                 opt$alpha * 100, 
                 "%.", 
                 output_ext)
        ), mustWork = FALSE)
      } else {
        # Construct output file path normally
        output_file <- normalizePath(file.path(
          output_path, 
          paste0(tools::file_path_sans_ext(file_name), 
                 ".result-rtlf-alpha-", 
                 opt$alpha * 100, 
                 "%.", 
                 output_ext)
        ), mustWork = FALSE)
      }
    } else {
      # No output specified for directory processing, don't save results
      output_file <- NULL
    }
  } else {
    # For single file processing, use the specified output file
    output_file <- opt$output
  }
  
  # Process input data
  tryCatch({
    data <- read_input_data(input_file)
    
    # Get sample size (minimum of both groups)
    n <- min(
      length((data %>% filter(V1 == "1"))$V2), 
      length((data %>% filter(V1 == "2"))$V2)
    )
    
    # Number of bootstrap replications
    B <- 10000
    
    # Run analysis
    output <- autotest(data, n, B, alphaPerDecile)
    
    # Format results - pass the data to the function
    results_object <- format_output(output, !opt$quiet, data)
    
    # Only save results if output file is provided
    if (!is.null(output_file)) {
      save_output(output, output_file, results_object, n, B)
    }
    
    # Calculate and report execution time
    end_time <- Sys.time()
    exec_time <- difftime(end_time, start_time, units = "secs")
    
    if (!opt$quiet) {
      cat(subheading("Execution time:"), highlight(sprintf("%.2f seconds", as.numeric(exec_time))), "\n\n")
    }
    
    # Return analysis result (0 = no difference, 1 = difference)
    return(ifelse(max(output[[1]]) > 0, 1, 0))
  }, error = function(e) {
    cat(error_color("ERROR processing file:"), highlight(input_file), "\n")
    cat("  ", error_color(conditionMessage(e)), "\n\n")
    
    # Still calculate execution time even if there was an error
    end_time <- Sys.time()
    exec_time <- difftime(end_time, start_time, units = "secs")
    
    if (!opt$quiet) {
      cat(subheading("Execution time:"), highlight(sprintf("%.2f seconds", as.numeric(exec_time))), "\n\n")
    }
    
    return(NA)  # Return NA to indicate error
  })
}

# Create a summary file for directory processing
create_directory_summary <- function(input_files, results, output_dir) {
  # Only create summary if output directory is specified
  if (is.null(output_dir) || !dir.exists(output_dir)) {
    return()
  }
  
  # Create summary data structure
  summary_data <- list(
    metadata = list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      alpha = alphaPerDecile * 9,
      input_directory = opt$input,
      files_processed = length(input_files),
      files_with_errors = sum(is.na(results)),
      files_with_differences = sum(results == 1, na.rm = TRUE),
      files_with_no_differences = sum(results == 0, na.rm = TRUE)
    ),
    file_results = lapply(seq_along(input_files), function(i) {
      file_name <- input_files[i]
      result <- results[i]
      
      list(
        file_name = basename(file_name),
        full_path = normalizePath(file_name, mustWork = FALSE),
        result_code = if (is.na(result)) "ERROR" else if (result == 1) "DIFFERENCE" else "NO_DIFFERENCE",
        status = if (is.na(result)) "error" else "success",
        difference_detected = if (is.na(result)) NA else result == 1
      )
    })
  )
  
  # Save to a summary file
  summary_file <- file.path(output_dir, paste0("rtlf_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"))
  write_json(summary_data, summary_file, pretty = TRUE, auto_unbox = TRUE)
  
  if (!opt$quiet) {
    cat(subheading("Summary saved to:"), highlight(summary_file), "\n\n")
  }
  
  return(summary_file)
}

# Process files
if (is_input_dir) {
  # Determine number of cores to use
  if (opt$threads == 0) {
    # Auto-detect: use all available cores minus 1 (to keep system responsive)
    num_cores <- max(1, detectCores() - 1)
  } else {
    # Use specified number of threads
    num_cores <- opt$threads
  }
  
  # For small number of files, limit cores
  num_cores <- min(num_cores, length(input_files))
  
  if (!opt$quiet && num_cores > 1) {
    cat(subheading("Using parallel processing with"), highlight(num_cores), 
        subheading("cores for"), highlight(length(input_files)), subheading("files"), "\n\n")
  }
  
  # Create cluster for parallel processing
  if (num_cores > 1) {
    cl <- makeCluster(num_cores)
    
    # Export necessary objects to the cluster workers
    clusterExport(cl, c("process_file", "read_input_data", "autotest", "bootstrap1", 
                        "test", "format_output", "save_output", "opt", "is_input_dir", 
                        "alphaPerDecile", "heading", "subheading", "success", "warning_color", 
                        "error_color", "highlight", "info", "result_yes", "result_no", 
                        "table_header", "Sys.time", "difftime"))
    
    # Load required libraries on all worker nodes
    clusterEvalQ(cl, {
      library(tidyverse)
      library(jsonlite)
      library(crayon)
      
      # Define color functions for output in each worker
      heading <- bold $ blue       # Section headings
      subheading <- bold $ cyan    # Subsection headings
      success <- bold $ green      # Success messages
      warning_color <- bold $ yellow  # Warning messages
      error_color <- bold $ red    # Error messages
      highlight <- bold $ magenta  # Important data
      info <- bold $ white         # Standard info
      result_yes <- bold $ red     # Statistical significance (red for significant difference)
      result_no <- bold $ green    # No statistical significance (green for no difference)
      table_header <- bold $ white # Table headers
      
      # Define helper function for table formatting
      format_aligned <- function(number, digits=4, width=12, justify="right") {
        format(number, digits=digits, width=width, justify=justify)
      }
    })
    
    # Process files in parallel
    results <- parSapply(cl, input_files, function(file) {
      process_file(file, opt$output)
    })
    
    # Stop the cluster
    stopCluster(cl)
  } else {
    # Serial processing (single core)
    results <- sapply(input_files, function(file) {
      process_file(file, opt$output)
    })
  }
  
  # Summary statistics
  success_count <- sum(!is.na(results))
  error_count <- sum(is.na(results))
  difference_count <- sum(results == 1, na.rm = TRUE)
  no_difference_count <- sum(results == 0, na.rm = TRUE)
  
  # Display summary
  if (!opt$quiet) {
    cat("\n", heading("============================================"), "\n")
    cat(heading("Processing Summary"), "\n")
    cat(heading("============================================"), "\n\n")
    cat(subheading("Total files processed:"), highlight(length(input_files)), "\n")
    cat(subheading("Successfully processed:"), success(success_count), "\n")
    
    # Use appropriate colors for errors
    if (error_count > 0) {
      cat(subheading("Errors:"), error_color(error_count), "\n")
    } else {
      cat(subheading("Errors:"), success("0"), "\n")
    }
    
    # Use appropriate colors for differences detected
    if (difference_count > 0) {
      cat(subheading("Files with difference detected:"), highlight(difference_count), "\n")
    } else {
      cat(subheading("Files with difference detected:"), "0", "\n")
    }
    
    cat(subheading("Files with no difference detected:"), info(no_difference_count), "\n\n")
    
    # Detailed file results
    cat(heading("File Analysis Results"), "\n")
    cat(info("------------------------------------------------------------"), "\n")
    cat(table_header(sprintf("%-40s %-15s\n", "File", "Result")))
    cat(info("------------------------------------------------------------"), "\n")
    
    # Create a named result vector for easy lookup
    named_results <- results
    names(named_results) <- input_files
    
    # Sort files by result (errors first, then differences, then no differences)
    sorted_files <- input_files[order(results, decreasing = TRUE)]
    
    for (file in sorted_files) {
      file_result <- named_results[file]
      file_name <- basename(file)
      
      if (is.na(file_result)) {
        # Error
        result_text <- error_color("ERROR")
      } else if (file_result == 1) {
        # Difference detected
        result_text <- result_yes("DIFFERENCE")
      } else {
        # No difference
        result_text <- result_no("NO DIFFERENCE")
      }
      
      cat(sprintf("%-40s %-15s\n", file_name, result_text))
    }
    cat(info("------------------------------------------------------------"), "\n\n")
  }
  
  # Create summary file if output directory exists
  if (!is.null(opt$output)) {
    output_dir <- if (dir.exists(opt$output)) {
      opt$output
    } else if (dir.exists(dirname(opt$output))) {
      dirname(opt$output)
    } else {
      NULL
    }
    
    if (!is.null(output_dir)) {
      create_directory_summary(input_files, results, output_dir)
    }
  }
  
  # Exit based on overall success/failure
  if (error_count > 0) {
    quit(status = 1)  # Error in at least one file
  } else if (difference_count > 0) {
    quit(status = 11) # At least one file has difference
  } else {
    quit(status = 10) # No differences detected
  }
} else {
  # Process single file
  result <- process_file(opt$input, opt$output)
  
  # Exit with appropriate exit code
  if (is.na(result)) {
    quit(status = 1)  # Error processing file
  } else if (result == 1) {
    quit(status = 11) # Difference detected
  } else {
    quit(status = 10) # No difference detected
  }
}
