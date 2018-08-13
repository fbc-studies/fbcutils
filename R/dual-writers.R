
# Write to csv and rds ----------------------------------------------------

# Write the same data to csv and rds files, always writing locally
#' @export
write_csv_rds <- function(data, path, ...) {
  # Temporary local files
  temp_csv <- tempfile()
  temp_rds <- tempfile()

  # Ensure temp files get deleted even if function throws error
  on.exit({remove_if_exists(temp_csv, temp_rds)})

  # Write data to temp files
  data.table::fwrite(data, file = temp_csv, sep = ";")
  readr::write_rds(data, path = temp_rds, ...)

  # Move to destination path
  p <- tools::file_path_sans_ext(path)
  file.rename(from = temp_csv, to = paste0(p, ".csv"))
  file.rename(from = temp_rds, to = paste0(p, ".rds"))
}


# Check if files exists; remove them if they do
remove_if_exists <- function(..., .files = list()) {
  files <- c(.files, list(...))
  exist <- vapply(files, file.exists, logical(1))
  if (any(exist)) {
    lapply(files[exist], file.remove)
  }
}


# Write to sav and rds ----------------------------------------------------

# Write the same data to sav and rds files, always writing locally
#' @export
write_sav_rds <- function(data, path, ...) {
  # Temporary local files
  temp_sav <- tempfile()
  temp_rds <- tempfile()

  # Ensure temp files get deleted even if function throws error
  on.exit({remove_if_exists(temp_sav, temp_rds)})

  # Write data to temp files
  haven::write_sav(format_sav(data), path = temp_sav)
  readr::write_rds(data, path = temp_rds, ...)

  # Move to destination path
  p <- tools::file_path_sans_ext(path)
  file.rename(from = temp_sav, to = paste0(p, ".sav"))
  file.rename(from = temp_rds, to = paste0(p, ".rds"))
}


# sav_rds helpers ---------------------------------------------------------

# Some magic to format sav files to look pretty in SPSS
#' @export
format_sav <- function(data) {
  data %>%
    purrr::modify_if(lubridate::is.Date, set_format_spss, "EDATE10") %>%
    purrr::modify_if(is_probably_int, as.integer)
}

# Set the format.spss attribute for a vector
set_format_spss <- function(x, value) {
  attr(x, "format.spss") <- value
  x
}

# Can a numeric vector be coerced to integer?
is_probably_int <- function(x) {
  if (!is.numeric(x) || any(is.infinite(x))) {
    return(FALSE)
  }

  x <- x[!is.na(x)]
  all(floor(x) == x)
}
