# Enhanced automated_data_sync.R
# Downloads from FTP and pushes each unique record into a Neon (Postgres) database.

library(RCurl)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(DBI)
library(RPostgres)

# Load utilities and configuration
source("csv_filter_utils.R")
source("config/school_config.R")

`%||%` <- function(x, y) if (!is.null(x)) x else y

neon_cfg <- school_config$neon
if (is.null(neon_cfg)) {
  stop("Neon configuration is missing. Please populate school_config$neon with connection info before running the sync.")
}

neon_cfg$table_prefix <- neon_cfg$table_prefix %||% tolower(school_config$team_code)
neon_cfg$schema <- neon_cfg$schema %||% "public"
neon_cfg$sslmode <- neon_cfg$sslmode %||% "require"
neon_cfg$port <- as.integer(neon_cfg$port %||% 5432)

required_keys <- c("host", "dbname", "user", "pass")
missing_keys <- required_keys[sapply(required_keys, function(key) !nzchar(neon_cfg[[key]] %||% ""))]
if (length(missing_keys) > 0) {
  stop("Neon configuration is missing the following keys: ", paste(missing_keys, collapse = ", "))
}

FTP_HOST <- "ftp.trackmanbaseball.com"
FTP_USER <- "VMI"
FTP_PASS <- "q7MvFhmAEN"
FTP_USERPWD <- paste0(FTP_USER, ":", FTP_PASS)

LOCAL_DATA_DIR      <- "data/"
LOCAL_PRACTICE_DIR  <- file.path(LOCAL_DATA_DIR, "practice")
LOCAL_V3_DIR        <- file.path(LOCAL_DATA_DIR, "v3")

dir.create(LOCAL_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOCAL_PRACTICE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOCAL_V3_DIR, recursive = TRUE, showWarnings = FALSE)

pref <- gsub("[^a-z0-9_]+", "", tolower(neon_cfg$table_prefix))
practice_table_name <- paste0(pref, "_practice_data")
v3_table_name <- paste0(pref, "_v3_data")
processed_table_name <- paste0(pref, "_processed_files")

key_columns <- c("Date", "Pitcher", "Batter", "PitchNo", "PlateLocSide", "PlateLocHeight", 
                 "RelSpeed", "TaggedPitchType", "Balls", "Strikes")

connect_neon <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = neon_cfg$host,
    port = neon_cfg$port,
    dbname = neon_cfg$dbname,
    user = neon_cfg$user,
    password = neon_cfg$pass,
    sslmode = neon_cfg$sslmode
  )
}

conn <- connect_neon()
on.exit(
  if (!is.null(conn) && DBI::dbIsValid(conn)) {
    DBI::dbDisconnect(conn)
  }
, add = TRUE)

ensure_schema <- function() {
  if (nzchar(neon_cfg$schema) && neon_cfg$schema != "public") {
    DBI::dbExecute(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(conn, neon_cfg$schema)))
  }
}

qualify_id <- function(name) {
  if (nzchar(neon_cfg$schema) && neon_cfg$schema != "public") {
    DBI::Id(schema = neon_cfg$schema, table = name)
  } else {
    DBI::Id(table = name)
  }
}

qualify_sql <- function(name) {
  if (nzchar(neon_cfg$schema) && neon_cfg$schema != "public") {
    paste0(DBI::dbQuoteIdentifier(conn, neon_cfg$schema), ".", DBI::dbQuoteIdentifier(conn, name))
  } else {
    DBI::dbQuoteIdentifier(conn, name)
  }
}

ensure_processed_files_table <- function() {
  if (!DBI::dbExistsTable(conn, qualify_id(processed_table_name))) {
    DBI::dbExecute(conn, paste0(
      "CREATE TABLE ", qualify_sql(processed_table_name),
      " (file_path TEXT PRIMARY KEY, processed_at TIMESTAMPTZ DEFAULT now())"
    ))
  }
}

is_file_processed <- function(remote_path) {
  sql <- paste0("SELECT 1 FROM ", qualify_sql(processed_table_name), " WHERE file_path = $1 LIMIT 1")
  res <- tryCatch(DBI::dbGetQuery(conn, sql, list(remote_path)), error = function(e) NULL)
  if (is.null(res)) return(FALSE)
  nrow(res) > 0
}

mark_file_processed <- function(remote_path) {
  sql <- paste0("INSERT INTO ", qualify_sql(processed_table_name), " (file_path) VALUES ($1) ON CONFLICT DO NOTHING")
  DBI::dbExecute(conn, sql, list(remote_path))
}

ensure_data_table <- function(table_name, sample_row) {
  if (!DBI::dbExistsTable(conn, qualify_id(table_name))) {
    sample <- sample_row[0, , drop = FALSE]
    DBI::dbWriteTable(conn, qualify_id(table_name), sample, row.names = FALSE)
  }
}

index_columns <- c("Date", "Pitcher", "TaggedPitchType", "SessionType", "PlayID")

ensure_table_indexes <- function(table_name) {
  table_id <- qualify_id(table_name)
  if (!DBI::dbExistsTable(conn, table_id)) return()
  available_fields <- DBI::dbListFields(conn, table_id)
  cols <- intersect(index_columns, available_fields)
  if (!length(cols)) return()
  quoted_table <- qualify_sql(table_name)
  for (col in cols) {
    safe_col <- DBI::dbQuoteIdentifier(conn, col)
    idx_name <- paste0(table_name, "_", tolower(col), "_idx")
    idx_name <- gsub("[^a-z0-9_]+", "_", idx_name)
    quoted_idx <- DBI::dbQuoteIdentifier(conn, idx_name)
    sql <- paste0("CREATE INDEX IF NOT EXISTS ", quoted_idx, " ON ", quoted_table, " (", safe_col, ")")
    tryCatch(DBI::dbExecute(conn, sql), error = function(e) {
      message("Warning: failed to create index ", idx_name, ": ", e$message)
    })
  }
}

ensure_all_columns_text <- function(table_name) {
  fields <- DBI::dbListFields(conn, qualify_id(table_name))
  for (field in fields) {
    sql <- paste0(
      "ALTER TABLE ", qualify_sql(table_name),
      " ALTER COLUMN ", DBI::dbQuoteIdentifier(conn, field),
      " TYPE TEXT USING (", DBI::dbQuoteIdentifier(conn, field), "::text)"
    )
    tryCatch(DBI::dbExecute(conn, sql), error = function(e) {
      # Already text or incompatible conversion; skip
    })
  }
}

coerce_to_text <- function(df) {
  convert_col <- function(col) {
    if (is.list(col)) {
      sapply(col, function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x), USE.NAMES = FALSE)
    } else {
      converted <- as.character(col)
      converted[is.na(col)] <- NA_character_
      converted
    }
  }
  dplyr::mutate(df, dplyr::across(dplyr::everything(), convert_col))
}

ensure_unique_index <- function(table_name, cols) {
  if (length(cols) == 0) return()
  idx_name <- paste0(table_name, "_", paste(cols, collapse = "_"), "_uniq")
  quoted_cols <- paste(DBI::dbQuoteIdentifier(conn, cols), collapse = ", ")
  sql <- paste0(
    "CREATE UNIQUE INDEX IF NOT EXISTS ",
    DBI::dbQuoteIdentifier(conn, idx_name),
    " ON ", qualify_sql(table_name), " (", quoted_cols, ")"
  )
  DBI::dbExecute(conn, sql)
}

upload_to_neon <- function(table_name, data) {
  text_ready <- coerce_to_text(data)
  ensure_data_table(table_name, text_ready)
  ensure_all_columns_text(table_name)
  ensure_table_indexes(table_name)
  available_keys <- intersect(key_columns, names(text_ready))
  ensure_unique_index(table_name, available_keys)
  temp_table <- paste0(table_name, "_tmp_", as.integer(runif(1, 1, 1e8)))
  DBI::dbWriteTable(conn, qualify_id(temp_table), text_ready, temporary = TRUE, row.names = FALSE)
  fields <- DBI::dbListFields(conn, qualify_id(table_name))
  quoted_fields <- paste(DBI::dbQuoteIdentifier(conn, fields), collapse = ", ")
  sql <- paste0(
    "INSERT INTO ", qualify_sql(table_name), " (", quoted_fields, ") ",
    "SELECT ", quoted_fields, " FROM ", qualify_sql(temp_table), " ON CONFLICT DO NOTHING"
  )
  DBI::dbExecute(conn, sql)
  DBI::dbRemoveTable(conn, qualify_id(temp_table))
}

list_ftp_files <- function(ftp_path) {
  url <- paste0("ftp://", FTP_HOST, ftp_path)
  tryCatch({
    files <- getURL(url, userpwd = FTP_USERPWD, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    strsplit(files, "\n")[[1]]
  }, error = function(e) {
    cat("Error listing files in", ftp_path, ":", e$message, "\n")
    character(0)
  })
}

dedupe_dataframe <- function(df) {
  available_keys <- intersect(key_columns, names(df))
  if (length(available_keys) == 0) return(df)
  df %>% distinct(across(all_of(available_keys)), .keep_all = TRUE)
}

download_csv <- function(remote_file, table_name) {
  if (is_file_processed(remote_file)) {
    cat("Skipping already processed file:", remote_file, "\n")
    return(FALSE)
  }
  filename <- basename(remote_file)
  if (should_exclude_csv(filename)) {
    return(FALSE)
  }

  url <- paste0("ftp://", FTP_HOST, remote_file)
  temp_file <- tempfile(fileext = ".csv")
  success <- FALSE
  tryCatch({
    bin <- RCurl::getBinaryURL(url, userpwd = FTP_USERPWD, ftp.use.epsv = FALSE)
    writeBin(bin, temp_file)
    data <- read_csv(temp_file, show_col_types = FALSE)
    data <- dedupe_dataframe(data)
    if (nrow(data) > 0) {
      upload_to_neon(table_name, data)
      mark_file_processed(remote_file)
      cat("Uploaded", nrow(data), "rows from", remote_file, "to Neon\n")
      success <<- TRUE
    } else {
      cat("No data found in", remote_file, "\n")
    }
  }, error = function(e) {
    cat("Error processing", remote_file, ":", e$message, "\n")
  }, finally = {
    unlink(temp_file)
  })
  success
}

sync_practice_data <- function() {
  cat("Syncing practice data to Neon...\n")
  years <- as.character(2025:year(Sys.Date()))
  downloaded_count <- 0

  for (yr in years) {
    practice_base_path <- paste0("/practice/", yr, "/")
    month_dirs <- list_ftp_files(practice_base_path)
    month_dirs <- month_dirs[grepl("^\\d{2}$", month_dirs)]

    for (month_dir in month_dirs) {
      month_path <- paste0(practice_base_path, month_dir, "/")
      day_dirs <- list_ftp_files(month_path)
      day_dirs <- day_dirs[grepl("^\\d{2}$", day_dirs)]

      for (day_dir in day_dirs) {
        day_path <- paste0(month_path, day_dir, "/")
        files_in_day <- list_ftp_files(day_path)
        csv_files <- files_in_day[grepl("\\.csv$", files_in_day, ignore.case = TRUE)]
        csv_files <- csv_files[!grepl("playerpositioning", csv_files, ignore.case = TRUE)]

        for (file in csv_files) {
          remote_path <- paste0(day_path, file)
          if (download_csv(remote_path, practice_table_name)) {
            downloaded_count <- downloaded_count + 1
          }
          Sys.sleep(0.1)
        }
      }
    }
  }
  cat("Practice sync complete: ", downloaded_count, "files uploaded to Neon\n")
  downloaded_count > 0
}

is_date_in_range <- function(file_path) {
  date_match <- stringr::str_match(file_path, "(20\\d{2})/(0[1-9]|1[0-2])/(0[1-9]|[12]\\d|3[01])")
  if (is.na(date_match[1])) {
    return(TRUE)
  }
  file_date <- as.Date(paste(date_match[2], date_match[3], date_match[4], sep = "-"))
  start_date <- as.Date("2025-08-10")
  file_date >= start_date
}

sync_v3_data <- function() {
  cat("Syncing v3 data to Neon...\n")
  years <- as.character(2025:year(Sys.Date()))
  downloaded_count <- 0

  for (yr in years) {
    v3_base_path <- paste0("/v3/", yr, "/")
    month_dirs <- list_ftp_files(v3_base_path)
    month_dirs <- month_dirs[grepl("^\\d{2}$", month_dirs)]

    for (month_dir in month_dirs) {
      month_path <- paste0(v3_base_path, month_dir, "/")
      day_dirs <- list_ftp_files(month_path)
      day_dirs <- day_dirs[grepl("^\\d{2}$", day_dirs)]

      for (day_dir in day_dirs) {
        day_path <- paste0(month_path, day_dir, "/")
        full_date_path <- paste0(yr, "/", month_dir, "/", day_dir)
        if (!is_date_in_range(full_date_path)) next

        files_in_day <- list_ftp_files(day_path)
        has_csv_dir <- "CSV" %in% files_in_day
        if (has_csv_dir) {
          csv_path <- paste0(day_path, "CSV/")
          csv_files <- list_ftp_files(csv_path)
        } else {
          csv_files <- files_in_day
        }
        csv_files <- csv_files[grepl("\\.csv$", csv_files, ignore.case = TRUE)]
        csv_files <- csv_files[!grepl("playerpositioning|unverified", csv_files, ignore.case = TRUE)]

        for (file in csv_files) {
          remote_path <- if (has_csv_dir) paste0(day_path, "CSV/", file) else paste0(day_path, file)
          if (download_csv(remote_path, v3_table_name)) {
            downloaded_count <- downloaded_count + 1
          }
          Sys.sleep(0.1)
        }
      }
    }
  }
  cat("V3 sync complete: ", downloaded_count, "files uploaded to Neon\n")
  downloaded_count > 0
}

main_sync <- function() {
  cat("Starting Neon data sync at", as.character(Sys.time()), "\n")
  ensure_schema()
  ensure_processed_files_table()

  start_time <- Sys.time()
  last_sync_file <- file.path(LOCAL_DATA_DIR, "last_sync.txt")
  if (!file.exists(last_sync_file)) {
    cat("First run detected - cleaning metadata files\n")
    old_files <- list.files(LOCAL_DATA_DIR, pattern = "\\.(csv|txt)$", full.names = TRUE, recursive = FALSE)
    if (length(old_files) > 0) {
      file.remove(old_files)
      cat("Cleaned", length(old_files), "metadata files\n")
    }
  } else {
    cat("Incremental sync - keeping metadata files\n")
  }

  practice_updated <- sync_practice_data()
  v3_updated <- sync_v3_data()

  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  cat("Data sync completed in", round(duration, 2), "minutes\n")

  writeLines(as.character(Sys.time()), last_sync_file)
  if (practice_updated || v3_updated) {
    writeLines(as.character(Sys.time()), file.path(LOCAL_DATA_DIR, "new_data_flag.txt"))
  }

  practice_updated || v3_updated
}

if (!interactive()) {
  data_updated <- main_sync()
  if (!data_updated) {
    cat("No new data found during sync - this is normal for incremental sync\n")
  } else {
    cat("New data was uploaded to Neon\n")
  }
  cat("Sync completed successfully\n")
}
