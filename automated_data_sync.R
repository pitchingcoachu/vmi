# DEBUG VERSION - automated_data_sync.R
# Let's see what's actually on the FTP server

library(RCurl)
library(tidyverse)

# FTP credentials
FTP_HOST <- "ftp.trackmanbaseball.com"
FTP_USER <- "VMI"
FTP_PASS <- "q7MvFhmAEN"

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

# Main debug function
debug_ftp_structure <- function() {
  cat("=== DEBUGGING FTP STRUCTURE ===\n")
  
  # Check root directories
  cat("\n1. ROOT DIRECTORIES:\n")
  root_items <- list_ftp_files("/")
  cat("Found in root:", paste(root_items, collapse = ", "), "\n")
  
  # Check practice folder
  cat("\n2. PRACTICE FOLDER STRUCTURE:\n")
  practice_items <- list_ftp_files("/practice/")
  cat("Found in /practice/:", paste(practice_items, collapse = ", "), "\n")
  
  if ("2025" %in% practice_items) {
    practice_2025_items <- list_ftp_files("/practice/2025/")
    cat("Found in /practice/2025/:", paste(practice_2025_items, collapse = ", "), "\n")
    
    # Check first few month folders
    for (month in practice_2025_items[1:min(3, length(practice_2025_items))]) {
      if (nchar(month) <= 2) {  # Looks like a month folder
        month_path <- paste0("/practice/2025/", month, "/")
        month_items <- list_ftp_files(month_path)
        cat("Found in", month_path, ":", paste(month_items, collapse = ", "), "\n")
        
        # Check first day folder
        if (length(month_items) > 0) {
          day_path <- paste0(month_path, month_items[1], "/")
          day_items <- list_ftp_files(day_path)
          cat("Found in", day_path, ":", paste(day_items, collapse = ", "), "\n")
        }
      }
    }
  }
  
  # Check v3 folder
  cat("\n3. V3 FOLDER STRUCTURE:\n")
  v3_items <- list_ftp_files("/v3/")
  cat("Found in /v3/:", paste(v3_items, collapse = ", "), "\n")
  
  if ("2025" %in% v3_items) {
    v3_2025_items <- list_ftp_files("/v3/2025/")
    cat("Found in /v3/2025/:", paste(v3_2025_items, collapse = ", "), "\n")
    
    # Check first few month folders
    for (month in v3_2025_items[1:min(3, length(v3_2025_items))]) {
      if (nchar(month) <= 2) {  # Looks like a month folder
        month_path <- paste0("/v3/2025/", month, "/")
        month_items <- list_ftp_files(month_path)
        cat("Found in", month_path, ":", paste(month_items, collapse = ", "), "\n")
        
        # Check first day folder
        if (length(month_items) > 0) {
          day_path <- paste0(month_path, month_items[1], "/")
          day_items <- list_ftp_files(day_path)
          cat("Found in", day_path, ":", paste(day_items, collapse = ", "), "\n")
          
          # Check if there's a CSV subfolder
          if ("CSV" %in% day_items) {
            csv_path <- paste0(day_path, "CSV/")
            csv_items <- list_ftp_files(csv_path)
            cat("Found in", csv_path, ":", paste(head(csv_items, 5), collapse = ", "), "\n")
          }
        }
      }
    }
  }
  
  cat("\n=== DEBUG COMPLETE ===\n")
}

# Create data directory
LOCAL_DATA_DIR <- "data/"
if (!dir.exists(LOCAL_DATA_DIR)) {
  dir.create(LOCAL_DATA_DIR, recursive = TRUE)
}

# Run debug and create a simple file so workflow doesn't fail
debug_ftp_structure()

# Create a debug file so we have something to commit
writeLines(c(
  paste("Debug run completed at:", Sys.time()),
  "Check the GitHub Actions log to see the FTP structure"
), file.path(LOCAL_DATA_DIR, "debug_log.txt"))

cat("Debug file created. Check the workflow logs for FTP structure details.\n")
