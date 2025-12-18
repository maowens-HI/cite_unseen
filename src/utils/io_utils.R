# ==============================================================================
# I/O UTILITIES
# Helper functions for file operations
# ==============================================================================

# ------------------------------------------------------------------------------
# FILE READING
# ------------------------------------------------------------------------------

#' Read file with encoding detection
#'
#' @param path File path
#' @param encodings Vector of encodings to try
#' @return File contents as character string
read_with_encoding <- function(path,
                               encodings = c("UTF-8", "latin1", "windows-1252")) {

  for (enc in encodings) {
    result <- tryCatch(
      {
        content <- readLines(path, encoding = enc, warn = FALSE)
        paste(content, collapse = "\n")
      },
      error = function(e) NULL
    )

    if (!is.null(result)) {
      return(result)
    }
  }

  stop("Could not read file with any encoding: ", path)
}

#' Read multiple files into a tibble
#'
#' @param paths Vector of file paths
#' @param id_from_name Function to extract ID from filename
#' @return Tibble with file_path, id, and content columns
read_files_to_tibble <- function(paths, id_from_name = basename) {

  tibble(
    file_path = paths,
    id = map_chr(paths, id_from_name),
    content = map_chr(paths, function(p) {
      tryCatch(
        read_with_encoding(p),
        error = function(e) NA_character_
      )
    })
  )
}

# ------------------------------------------------------------------------------
# FILE WRITING
# ------------------------------------------------------------------------------

#' Write with backup
#'
#' @param data Data to write
#' @param path Output file path
#' @param write_fn Writing function (default: write_csv)
write_with_backup <- function(data, path, write_fn = write_csv) {

  # Create backup if file exists
  if (file.exists(path)) {
    backup_path <- paste0(path, ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(path, backup_path)
    message("Backed up existing file to: ", backup_path)
  }

  # Write new file
  write_fn(data, path)
  message("Wrote: ", path)
}

#' Safe file write with directory creation
#'
#' @param data Data to write
#' @param path Output file path
#' @param write_fn Writing function
safe_write <- function(data, path, write_fn = write_csv) {

  # Create directory if needed
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created directory: ", dir_path)
  }

  # Write file
  write_fn(data, path)
  message("Wrote: ", path)
}

# ------------------------------------------------------------------------------
# PARQUET SUPPORT
# ------------------------------------------------------------------------------

#' Write data to Parquet format
#'
#' @param data Data frame or tibble
#' @param path Output file path
write_parquet_safe <- function(data, path) {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    warning("arrow package not available, writing CSV instead")
    path <- str_replace(path, "\\.parquet$", ".csv")
    write_csv(data, path)
  } else {
    arrow::write_parquet(data, path)
  }

  message("Wrote: ", path)
}

#' Read Parquet file
#'
#' @param path File path
#' @return Tibble
read_parquet_safe <- function(path) {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("arrow package required for Parquet files")
  }

  arrow::read_parquet(path)
}

# ------------------------------------------------------------------------------
# DIRECTORY OPERATIONS
# ------------------------------------------------------------------------------

#' Ensure directory exists
#'
#' @param path Directory path
#' @return Invisible path
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Created: ", path)
  }
  invisible(path)
}

#' List files with pattern, returning full paths
#'
#' @param dir Directory to search
#' @param pattern Regex pattern
#' @param recursive Search subdirectories?
#' @return Character vector of file paths
list_files_full <- function(dir, pattern = NULL, recursive = FALSE) {
  list.files(dir, pattern = pattern, full.names = TRUE, recursive = recursive)
}

#' Get file info as tibble
#'
#' @param paths Vector of file paths
#' @return Tibble with file information
file_info_tibble <- function(paths) {

  info <- file.info(paths)

  tibble(
    path = paths,
    name = basename(paths),
    size_bytes = info$size,
    size_mb = round(info$size / 1e6, 2),
    modified = info$mtime,
    is_dir = info$isdir
  )
}

# ------------------------------------------------------------------------------
# CHECKPOINTING
# ------------------------------------------------------------------------------

#' Save checkpoint
#'
#' @param data Data to save
#' @param checkpoint_num Checkpoint number
#' @param dir Checkpoint directory
save_checkpoint <- function(data, checkpoint_num, dir = "data/intermediate") {

  ensure_dir(dir)
  path <- file.path(dir, sprintf("checkpoint_%05d.rds", checkpoint_num))
  saveRDS(data, path)
  message("Checkpoint saved: ", path)
}

#' Load latest checkpoint
#'
#' @param dir Checkpoint directory
#' @return List with data and checkpoint number, or NULL if none
load_latest_checkpoint <- function(dir = "data/intermediate") {

  checkpoints <- list_files_full(dir, pattern = "checkpoint_\\d+\\.rds")

  if (length(checkpoints) == 0) {
    return(NULL)
  }

  # Get latest by number
  nums <- as.integer(str_extract(basename(checkpoints), "\\d+"))
  latest_idx <- which.max(nums)

  list(
    data = readRDS(checkpoints[latest_idx]),
    checkpoint_num = nums[latest_idx],
    path = checkpoints[latest_idx]
  )
}

#' Clean old checkpoints
#'
#' @param dir Checkpoint directory
#' @param keep_n Number of recent checkpoints to keep
clean_checkpoints <- function(dir = "data/intermediate", keep_n = 5) {

  checkpoints <- list_files_full(dir, pattern = "checkpoint_\\d+\\.rds")

  if (length(checkpoints) <= keep_n) {
    message("Only ", length(checkpoints), " checkpoints found, nothing to clean")
    return(invisible(NULL))
  }

  # Sort by number and remove old ones
  nums <- as.integer(str_extract(basename(checkpoints), "\\d+"))
  sorted_idx <- order(nums, decreasing = TRUE)
  to_remove <- checkpoints[sorted_idx[(keep_n + 1):length(checkpoints)]]

  file.remove(to_remove)
  message("Removed ", length(to_remove), " old checkpoints")
}

# ------------------------------------------------------------------------------
# LOGGING
# ------------------------------------------------------------------------------

#' Simple logging to file
#'
#' @param message Message to log
#' @param log_file Log file path
log_message <- function(message, log_file = "logs/analysis.log") {

  ensure_dir(dirname(log_file))

  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  log_line <- paste(timestamp, message)

  write(log_line, file = log_file, append = TRUE)
}

#' Start timed operation
#'
#' @param operation_name Name of operation
#' @return Start time
start_timer <- function(operation_name) {
  message("\nStarting: ", operation_name)
  Sys.time()
}

#' End timed operation and report
#'
#' @param start_time Start time from start_timer
#' @param operation_name Name of operation
end_timer <- function(start_time, operation_name) {
  elapsed <- Sys.time() - start_time
  message(sprintf("Completed: %s (%.2f %s)",
                  operation_name,
                  elapsed,
                  attr(elapsed, "units")))
}

# ------------------------------------------------------------------------------
# MAIN EXECUTION (if run as script)
# ------------------------------------------------------------------------------

if (sys.nframe() == 0) {
  message("I/O utilities loaded.")
  message("Available functions:")
  message("  - read_with_encoding()")
  message("  - read_files_to_tibble()")
  message("  - write_with_backup()")
  message("  - safe_write()")
  message("  - write_parquet_safe()")
  message("  - ensure_dir()")
  message("  - save_checkpoint()")
  message("  - load_latest_checkpoint()")
}
