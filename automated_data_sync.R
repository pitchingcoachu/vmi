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

# Function to sync practice data (2025 folder)
sync_practice_data <- function() {
  cat("Syncing practice data...\n")
  practice_path <- "/practice/2025/"
  
  files <- list_ftp_files(practice_path)
  csv_files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  
  downloaded_count <- 0
  for (file in csv_files) {
    remote_path <- paste0(practice_path, file)
    local_path <- file.path(LOCAL_DATA_DIR, paste0("practice_", file))
    
    if (download_and_filter_csv(remote_path, local_path)) {
      downloaded_count <- downloaded_count + 1
    }
  }
  
  cat("Practice sync complete:", downloaded_count, "files with VMI data\n")
  return(downloaded_count > 0)
}

# Function to sync v3 data (2025 folder) - only VMI files
sync_v3_data <- function() {
  cat("Syncing v3 data (VMI only)...\n")
  v3_path <- "/v3/2025/"
  
  files <- list_ftp_files(v3_path)
  csv_files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  
  # Process files in batches to avoid memory issues
  batch_size <- 20  # Smaller batches for efficiency
  total_files <- length(csv_files)
  
  cat("Found", total_files, "CSV files in v3/2025. Processing in batches...\n")
  
  downloaded_count <- 0
  for (i in seq(1, total_files, by = batch_size)) {
    end_idx <- min(i + batch_size - 1, total_files)
    batch_files <- csv_files[i:end_idx]
    
    cat("Processing batch", ceiling(i/batch_size), "of", ceiling(total_files/batch_size), "\n")
    
    for (file in batch_files) {
      remote_path <- paste0(v3_path, file)
      local_path <- file.path(LOCAL_DATA_DIR, paste0("v3_", file))
      
      if (download_and_filter_csv(remote_path, local_path)) {
        downloaded_count <- downloaded_count + 1
      }
    }
    
    # Small delay between batches to be respectful to the server
    Sys.sleep(0.5)
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
