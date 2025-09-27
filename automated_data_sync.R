# automated_data_sync.R
# VMI Baseball Data Automation Script
# Syncs data from TrackMan FTP, filters for VMI only

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

# Function to download and filter CSV file
download_and_filter_csv <- function(remote_file, local_file) {
  url <- paste0("ftp://", FTP_USER, ":", FTP_PASS, "@", FTP_HOST, remote_file)
  
  tryCatch({
    # Download file to temporary location
    temp_file <- tempfile(fileext = ".csv")
    download.file(url, temp_file, method = "curl", quiet = TRUE)
    
    # Read and filter data
    data <- read_csv(temp_file, show_col_types = FALSE)
    
    # Filter for VMI data only
    vmi_data <- data %>%
      filter(
        (if("PitcherTeam" %in% names(data)) PitcherTeam %in% c("VIR_KEY", "VMI_KEY") else FALSE) |
        (if("BatterTeam" %in% names(data)) BatterTeam %in% c("VIR_KEY", "VMI_KEY") else FALSE)
      )
    
    # Only save if we have VMI data
    if (nrow(vmi_data) > 0) {
      write_csv(vmi_data, local_file)
      cat("Downloaded and filtered", nrow(vmi_data), "VMI rows to", local_file, "\n")
      unlink(temp_file)
      return(TRUE)
    } else {
      cat("No VMI data found in", remote_file, "\n")
      unlink(temp_file)
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("Error processing", remote_file, ":", e$message, "\n")
    return(FALSE)
  })
}

# Function to recursively find CSV files in month/day folders
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

# Function to choose best file when duplicates exist (prefer non-unverified)
choose_best_files <- function(file_paths) {
  if (length(file_paths) == 0) return(character(0))
  
  # Group files by date pattern (extract date from path)
  file_df <- data.frame(
    path = file_paths,
    is_unverified = grepl("unverified", file_paths, ignore.case = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Extract date pattern from path (MM/DD format)
  file_df$date_key <- str_extract(file_df$path, "/\\d{2}/\\d{2}/")
  
  # Group by date and choose best file for each date
  best_files <- file_df %>%
    group_by(date_key) %>%
    arrange(is_unverified) %>%  # FALSE (non-unverified) comes first
    slice(1) %>%  # Take the first (best) file for each date
    pull(path)
  
  return(best_files)
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
  
  # Choose best files (prefer non-unverified)
  best_files <- choose_best_files(all_csv_files)
  cat("Processing", length(best_files), "best files (after removing duplicates)\n")
  
  downloaded_count <- 0
  for (remote_path in best_files) {
    # Create local filename from the remote path
    local_filename <- paste0("practice_", gsub("[/:]", "_", basename(remote_path)))
    local_path <- file.path(LOCAL_DATA_DIR, local_filename)
    
    if (download_and_filter_csv(remote_path, local_path)) {
      downloaded_count <- downloaded_count + 1
    }
  }
  
  cat("Practice sync complete:", downloaded_count, "files with VMI data\n")
  return(downloaded_count > 0)
}

# Function to sync v3 data (v3/2025/MM/DD/CSV/csvfile structure)
sync_v3_data <- function() {
  cat("Syncing v3 data (VMI only)...\n")
  v3_base <- "/v3/2025/"
  
  # Find all CSV files in the month/day/CSV folder structure
  all_csv_files <- find_all_csv_files(v3_base, max_depth = 4)  # Deeper for v3 structure
  
  if (length(all_csv_files) == 0) {
    cat("No CSV files found in v3/2025/ folders\n")
    return(FALSE)
  }
  
  cat("Found", length(all_csv_files), "CSV files in v3 folders\n")
  
  # Choose best files (prefer non-unverified)
  best_files <- choose_best_files(all_csv_files)
  cat("Processing", length(best_files), "best files (after removing duplicates)\n")
  
  # Process files in batches
  batch_size <- 10  # Smaller batches for v3 data
  total_files <- length(best_files)
  downloaded_count <- 0
  
  if (total_files > 0) {
    for (i in seq(1, total_files, by = batch_size)) {
      end_idx <- min(i + batch_size - 1, total_files)
      batch_files <- best_files[i:end_idx]
      
      cat("Processing batch", ceiling(i/batch_size), "of", ceiling(total_files/batch_size), "\n")
      
      for (remote_path in batch_files) {
        # Create local filename from the remote path
        local_filename <- paste0("v3_", gsub("[/:]", "_", basename(remote_path)))
        local_path <- file.path(LOCAL_DATA_DIR, local_filename)
        
        if (download_and_filter_csv(remote_path, local_path)) {
          downloaded_count <- downloaded_count + 1
        }
      }
      
      # Small delay between batches
      Sys.sleep(0.5)
    }
  }
  
  cat("V3 sync complete:", downloaded_count, "files with VMI data\n")
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
