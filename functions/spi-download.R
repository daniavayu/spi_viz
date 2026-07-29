# Internal download engine for the SPI GitHub repository.
# Not exported — called by spi_get() and spi_get_raw().

SPI_GITHUB_BASE <- "https://raw.githubusercontent.com/worldbank/SPI"

# Package-level in-session cache, keyed on "version|file_path".
.spi_cache <- new.env(parent = emptyenv())

# ---------------------------------------------------------------------------
# Shared version validation (used by spi_download, spi_get, and inventory)
# ---------------------------------------------------------------------------

#' Validate the `version` argument for SPI functions
#'
#' Shared guard that enforces a consistent contract on the `version` argument
#' across `spi_download()`, `spi_get()`, `spi_update_inventory()`,
#' `spi_clear_inventory()`, and `.spi_get_inventory()`. Centralising the
#' check here ensures that future changes (e.g. adding a length limit or
#' branch-name validation) are applied in one place.
#'
#' @param version The value to validate.
#' @return `version`, invisibly, if valid.
#' @keywords internal
.spi_validate_version <- function(version) {
  if (!is.character(version) || length(version) != 1L || !nzchar(version)) {
    cli::cli_abort(c(
      "{.arg version} must be a single non-empty character string.",
      "x" = "You supplied a {.cls {class(version)[1L]}} of length {length(version)}."
    ))
  }
  invisible(version)
}

#' Clear the spiR in-session download cache
#'
#' Removes all cached SPI data downloaded in the current R session.
#' Useful when you want to force a fresh download — for example, after
#' switching versions or when the remote data has been updated.
#'
#' @return `NULL`, invisibly.
#'
#' @examples
#' \dontrun{
#' spi_clear_cache()
#' }
#'
#' @export
spi_clear_cache <- function() {
  rm(list = ls(.spi_cache), envir = .spi_cache)
  invisible(NULL)
}

#' Download a file from the SPI GitHub repository
#'
#' Results are cached in-session by `(version, file_path)` key so that
#' repeated calls with identical arguments avoid redundant downloads.
#'
#' @param file_path Character. Path within the repository (e.g.
#'   `"03_output_data/SPI_data.csv"`).
#' @param version Character. Branch name. Default is `"master"`.
#'
#' @return A `data.table`.
#' @keywords internal
spi_download <- function(file_path, version = "master") {
  .spi_validate_version(version)

  cache_key <- paste0(version, "|", file_path)
  if (exists(cache_key, envir = .spi_cache, inherits = FALSE)) {
    return(.spi_cache[[cache_key]])
  }

  url <- paste(SPI_GITHUB_BASE, version, file_path, sep = "/")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  result <- tryCatch(
    utils::download.file(url, destfile = tmp, quiet = TRUE, mode = "wb"),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to download SPI data from GitHub.",
        "x" = "Caused by: {conditionMessage(e)}",
        "i" = "URL: {url}",
        "i" = "Version: {version}"
      ))
    }
  )

  if (result != 0L) {
    cli::cli_abort(c(
      "Download returned a non-zero exit status ({result}).",
      "i" = "URL: {url}",
      "i" = "Version: {version}"
    ))
  }

  dt <- tryCatch(
    fread(tmp, data.table = TRUE, encoding = "UTF-8"),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to parse the downloaded CSV file.",
        "x" = "Caused by: {conditionMessage(e)}",
        "i" = "URL: {url}"
      ))
    }
  )

  if (nrow(dt) == 0L) {
    cli::cli_warn(c(
      "Downloaded file contains no rows.",
      "i" = "URL: {url}",
      "i" = "Version: {version}"
    ))
  }

  .spi_cache[[cache_key]] <- dt
  return(dt)
}
