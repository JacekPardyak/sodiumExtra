
.validate_id <- function(x, name = "id") {
  if (!is.character(x) || length(x) != 1L ||
      is.na(x) || !nzchar(x) ||
      x %in% c(".", "..") ||
      grepl("[/\\\\]", x)) {
    stop(name, " must be a non-empty identifier without path separators.")
  }
  
  invisible(x)
}

.read_raw <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }
  
  size <- file.info(path)$size
  
  if (is.na(size) || size > .Machine$integer.max) {
    stop("Invalid or excessively large file: ", path)
  }
  
  readBin(path, what = "raw", n = as.integer(size))
}

.write_raw <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE,
             showWarnings = FALSE)
  writeBin(data, path)
  invisible(path)
}
