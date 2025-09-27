# automated_data_sync.R
# VMI Baseball Data Automation Script
# Downloads ALL data from target date ranges - app will filter

library(RCurl)
library(tidyverse)
library(lubridate)

# FTP credentials
FTP_HOST <- "ftp.trackmanbaseball.com"
FTP_USER <- "VMI"
FTP_PASS <- "q7MvFhmAEN"

# Local data directory
LOCAL_DATA_DIR <- "data/"

# Ensure data directory exists
if (!dir.exists(LOCAL_DATA_DIR)) {
  dir.create(LOCAL_DATA_DIR, recursive = TRUE)
}

# Function to list files in FTP directory
list_ftp_files <- function(ftp_path) {
  url <- paste0("ftp://", FTP_USER, ":", FTP_PASS, "@", FTP_HOST, ftp_path)
  tryCatch({
    files <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    strsplit(files, "\n")[[1]]
  }, error = function(e) {
    cat("Error listing files in", ftp_path, ":", e$message, "\n")
    character(0)
  })
}

# Function to download CSV file (no filtering - download everything)
download_csv_file <- function(remote_file, local_file) {
  url <- paste0("ftp://", FTP_USER, ":", FTP_PASS, "@", FTP_HOST, remote_file)
  
  tryCatch({
    download.file(url, local_file, method = "curl", quiet = TRUE)
    cat("Downloaded", remote_file, "to", local_file, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("Error downloading", remote_file, ":", e$message, "\n")
    return(FALSE)
  })
}

# Function to recursively find CSV files in specific month folders
find_csv_files_in_months <- function(base_path, target_months, max_depth = 4) {
  all_files <- character(0)
  
  # Function to recursively explore directories
  explore_directory <- function(current_path, depth = 0) {
    if (depth > max_depth) return()
    
    items <- list_ftp_files(current_path)
    if (length(items) == 0) return()
    
    for (item in items) {
      if (item == "" || item == "." || item == "..") next
      
      item_path <- paste0(current_path, item)
      
      # If it's a CSV file, add it
      if (grepl("\\.csv$", item, ignore.case = TRUE)) {
        all_files <<- c(all_files, item_path)
      } else {
        # Check if this is a month folder we want to explore
        if (depth == 0) {  # At the month level
          month_num <- sprintf("%02d", as.numeric(item))
          if (month_num %in% target_months) {
            item_path_with_slash <- paste0(item_path, "/")
            explore_directory(item_path_with_slash, depth + 1)
          }
        } else {
          # Continue exploring subdirectories
          item_path_with_slash <- paste0(item_path, "/")
          explore_directory(item_path_with_slash, depth + 1)
        }
      }
    }
  }
  
  explore_directory(base_path)
  return(all_files)
}

# Function to find files from Sept 1st forward in current month
find_recent_csv_files <- function(base_path, start_date = "2025-09-01", max_depth = 4) {
  all_files <- character(0)
  start_date <- as.Date(start_date)
  current_month <- sprintf("%02d", month(Sys.Date()))
  
  # Function to recursively explore directories
  explore_directory <- function(current_path, depth = 0) {
    if (depth > max_depth) return()
    
    items <- list_ftp_files(current_path)
    if (length(items) == 0) return()
    
    for (item in items) {
      if (item == "" || item == "." || item == "..") next
      
      item_path <- paste0(current_path, item)
      
      # If it's a CSV file, check if it's from Sept 1st or later
      if (grepl("\\.csv$", item, ignore.case = TRUE)) {
        # Extract date from path if possible (basic check)
        if (depth >= 2) {  # We're in a day folder
          all_files <<- c(all_files, item_path)
        }
      } else {
        # Navigate through month/day structure
        if (depth == 0 && item == current_month) {  # Current month only
          item_path_with_slash <- paste0(item_path, "/")
          explore_directory(item_path_with_slash, depth + 1)
        } else if (depth > 0) {  # Day folders - take everything from Sept 1st forward
          item_path_with_slash <- paste0(item_path, "/")
          explore_directory(item_path_with_slash, depth + 1)
        }
      }
    }
  }
  
  explore_directory(base_path)
  return(all_files)
}

# Function to find all CSV files (for practice folder)
find_all_csv_files <- function(base_path, max_depth = 3) {
  all_files <- character(0)
  
  # Function to recursively explore directories
  explore_directory <- function(current_path, depth = 0) {
    if (depth > max_depth) return()
    
    items <- list_ftp_files(current_path)
    if (length(items) == 0) return()
    
    for (item in items) {
      if (item == "" || item == "." || item == "..") next
      
      item_path <- paste0(current_path, item)
      
      # If it's a CSV file, add it
      if (grepl("\\.csv$", item, ignore.case = TRUE)) {
        all_files <<- c(all_files, item_path)
      } else {
        # If it might be a directory, explore it
        item_path_with_slash <- paste0(item_path, "/")
        explore_directory(item_path_with_slash, depth + 1)
      }
    }
  }
  
  explore_directory(base_path)
  return(all_files)
}

# Function to prefer non-unverified files when there are exact duplicates
remove_duplicate_files <- function(file_paths) {
  if (length(file_paths) == 0) return(character(0))
  
  # Create a data frame to analyze files
  file_df <- data.frame(
    path = file_paths,
    basename = basename(file_paths),
    is_unverified = grepl("unverified", file_paths, ignore.case = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Remove the "_unverified" part to group true duplicates
  file_df$base_name_clean <- gsub("_unverified", "", file_df$basename, ignore.case = TRUE)
  
  # Group by clean basename and prefer non-unverified
  result_files <- file_df %>%
    group_by(base_name_clean) %>%
    arrange(is_unverified) %>%  # FALSE (non-unverified) comes first
    slice(1) %>%  # Take the first (best) file for each unique game
    pull(path)
  
  return(result_files)
}

# Function to sync practice data (practice/2025/MM/DD/csvfile structure)
sync_practice_data <- function() {
  cat("Syncing practice data...\n")
  practice_base <- "/practice/2025/"
  
  # Find all CSV files in the month/day folder structure
  all_csv_files <- find_all_csv_files(practice_base)
  
  if (length(all_csv_files) == 0) {
    cat("No CSV files found in practice/2025/ folders\n")
    return(FALSE)
  }
  
  cat("Found", length(all_csv_files), "CSV files in practice folders\n")
  
  # Remove exact duplicates (prefer non-unverified)
  final_files <- remove_duplicate_files(all_csv_files)
  cat("Processing", length(final_files), "files (after removing exact duplicates)\n")
  
  downloaded_count <- 0
  for (remote_path in final_files) {
    # Create local filename from the remote path
    local_filename <- paste0("practice_", gsub("[/:]", "_", basename(remote_path)))
    local_path <- file.path(LOCAL_DATA_DIR, local_filename)
    
    if (download_csv_file(remote_path, local_path)) {
      downloaded_count <- downloaded_count + 1
    }
  }
  
  cat("Practice sync complete:", downloaded_count, "files downloaded\n")
  return(downloaded_count > 0)
}

# Function to sync v3 data - DOWNLOAD ALL FILES in target date ranges
sync_v3_data <- function() {
  cat("Syncing v3 data (ALL files in target ranges)...\n")
  v3_base <- "/v3/2025/"
  
  # Historical months: Feb through May
  historical_months <- c("02", "03", "04", "05")
  
  cat("Getting historical data from months:", paste(historical_months, collapse = ", "), "\n")
  historical_files <- find_csv_files_in_months(v3_base, historical_months, max_depth = 4)
  
  cat("Getting recent data from September 1st forward...\n")
  recent_files <- find_recent_csv_files(v3_base, start_date = "2025-09-01", max_depth = 4)
  
  # Combine all files
  all_csv_files <- c(historical_files, recent_files)
  
  if (length(all_csv_files) == 0) {
    cat("No CSV files found in v3/2025/ target date ranges\n")
    return(FALSE)
  }
  
  cat("Found", length(all_csv_files), "total CSV files in v3 target ranges\n")
  
  # Remove exact duplicates only (prefer non-unverified)
  final_files <- remove_duplicate_files(all_csv_files)
  cat("Processing", length(final_files), "files (after removing exact duplicates)\n")
  
  # Process EVERY file in smaller batches
  batch_size <- 5  # Small batches
  total_files <- length(final_files)
  downloaded_count <- 0
  
  if (total_files > 0) {
    for (i in seq(1, total_files, by = batch_size)) {
      end_idx <- min(i + batch_size - 1, total_files)
      batch_files <- final_files[i:end_idx]
      
      cat("Processing batch", ceiling(i/batch_size), "of", ceiling(total_files/batch_size), "\n")
      
      for (remote_path in batch_files) {
        # Create local filename from the remote path
        local_filename <- paste0("v3_", gsub("[/:]", "_", basename(remote_path)))
        local_path <- file.path(LOCAL_DATA_DIR, local_filename)
        
        if (download_csv_file(remote_path, local_path)) {
          downloaded_count <- downloaded_count + 1
        }
      }
      
      # Small delay between batches
      Sys.sleep(1)
    }
  }
  
  cat("V3 sync complete:", downloaded_count, "files downloaded\n")
  return(downloaded_count > 0)
}

# Main sync function
main_sync <- function() {
  cat("Starting VMI data sync at", as.character(Sys.time()), "\n")
  
  start_time <- Sys.time()
  
  # Clean old data files first
  old_files <- list.files(LOCAL_DATA_DIR, pattern = "\\.(csv|txt)$", full.names = TRUE)
  if (length(old_files) > 0) {
    file.remove(old_files)
    cat("Cleaned", length(old_files), "old data files\n")
  }
  
  # Sync both data sources
  practice_updated <- sync_practice_data()
  v3_updated <- sync_v3_data()
  
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  
  cat("Data sync completed in", round(duration, 2), "minutes\n")
  
  # Update last sync timestamp
  writeLines(as.character(Sys.time()), file.path(LOCAL_DATA_DIR, "last_sync.txt"))
  
  # Return TRUE if any data was updated
  return(practice_updated || v3_updated)
}

# Run if called directly
if (!interactive()) {
  data_updated <- main_sync()
  # Exit with code 1 if no data was found (for GitHub Actions)
  if (!data_updated) {
    cat("No VMI data found during sync\n")
    quit(status = 1)
  }
}
