# Inventory cache system for the spiR package.
#
# Stores crawled GitHub file trees on disk (one RDS per version/branch)
# to avoid redundant GitHub API calls on every spi_inventory() call.
#
# Public API   : spi_update_inventory(), spi_clear_inventory()
# Internal     : .spi_inv_cache_dir(), .spi_inv_cache_path(),
#                .spi_inv_write_cache(), .spi_inv_read_cache(),
#                .spi_get_inventory()
# Path enrich  : .spi_enrich_tree()  — lives in this file
# Tree crawler : .spi_crawl_tree()   — lives in spi-github.R

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Increment when the cache list structure changes (forces re-crawl on load).
SPI_INVENTORY_CACHE_SCHEMA_VERSION <- 1L
# Age threshold (days) after which a cached tree expires and triggers a re-crawl.
SPI_INVENTORY_CACHE_TTL_DAYS       <- 30L

# ---------------------------------------------------------------------------
# Step 1: Path enrichment
# ---------------------------------------------------------------------------

#' Enrich a raw file-tree data.table with pillar, dimension, and category
#'
#' Takes a raw `data.table` (from the GitHub tree crawler) with columns
#' `path`, `type`, and `size`, and adds three metadata columns derived
#' from each file path:
#'
#' - `category`: `"raw"` for files under `01_raw_data/`, `"output"` for
#'   files under `03_output_data/`, and `"misc"` for everything else.
#' - `pillar`: integer 1--5 extracted from the first subfolder name under
#'   `01_raw_data/` (e.g. `4.1_SOCS` -> `4L`). `NA` for all other paths.
#' - `dimension`: character in `"P.D"` format from the same subfolder
#'   (e.g. `4.1_SOCS` -> `"4.1"`). `NA` when the subfolder encodes only a
#'   pillar (e.g. `3_DP`) or the path falls outside `01_raw_data/`.
#'
#' The function is pure: it copies the input before enriching, so the
#' original `data.table` is never modified.
#'
#' @param tree_dt A `data.table` with columns `path` (character),
#'   `type` (character), and `size` (integer).
#' @return A `data.table` with six columns: `path`, `type`, `size`,
#'   `category`, `pillar`, `dimension`.
#' @keywords internal
.spi_enrich_tree <- function(tree_dt) {
  if (!data.table::is.data.table(tree_dt))
    cli::cli_abort("{.arg tree_dt} must be a {.cls data.table}, not {.cls {class(tree_dt)[1L]}}.")
  if (!"path" %in% names(tree_dt))
    cli::cli_abort("{.arg tree_dt} must contain a {.field path} column.")
  if (!is.character(tree_dt[["path"]]))
    cli::cli_abort("Column {.field path} must be character, not {.cls {class(tree_dt$path)[1L]}}.")
  if (!"type" %in% names(tree_dt))
    cli::cli_abort("{.arg tree_dt} must contain a {.field type} column.")
  if (!"size" %in% names(tree_dt))
    cli::cli_abort("{.arg tree_dt} must contain a {.field size} column.")

  dt   <- data.table::copy(tree_dt)
  path <- dt[["path"]]

  na_paths <- sum(is.na(path))
  if (na_paths > 0L)
    cli::cli_warn("{na_paths} NA value{?s} in {.field path} treated as {.val misc}.")

  # --- category ------------------------------------------------------------
  # Derive from the top-level folder prefix.
  category <- rep("misc", length(path))
  category[startsWith(path, "01_raw_data/")]    <- "raw"
  category[startsWith(path, "03_output_data/")] <- "output"
  dt[, category := category]

  # --- pillar and dimension (raw paths only) -------------------------------
  # Extract the first subfolder immediately under 01_raw_data/, e.g.:
  #   "01_raw_data/4.1_SOCS/file.csv"         -> subdir = "4.1_SOCS"
  #   "01_raw_data/3_DP/2024/score.csv"        -> subdir = "3_DP"
  #   "01_raw_data/metadata/codes.csv"         -> subdir = "metadata"
  raw_idx <- which(category == "raw")
  subdir  <- rep(NA_character_, length(path))

  if (length(raw_idx) > 0L) {
    # Strip the "01_raw_data/" prefix, then take text before the next "/".
    after_prefix    <- substring(path[raw_idx], nchar("01_raw_data/") + 1L)
    subdir[raw_idx] <- sub("/.*$", "", after_prefix)
  }

  # Compute pil_hits first; dim_hits is a strict subset (P.D folders also
  # satisfy the leading-digit pattern), avoiding a redundant grep pass.
  pil_hits <- grep("^[0-9]+", subdir)
  dim_hits <- pil_hits[grepl("^[0-9]+\\.[0-9]+", subdir[pil_hits])]

  dim_vec <- rep(NA_character_, length(path))
  if (length(dim_hits) > 0L) {
    sub_dim           <- subdir[dim_hits]
    dim_vec[dim_hits] <- regmatches(sub_dim, regexpr("^[0-9]+\\.[0-9]+", sub_dim))
  }

  pil_vec <- rep(NA_integer_, length(path))
  if (length(pil_hits) > 0L) {
    sub_pil           <- subdir[pil_hits]
    pil_vec[pil_hits] <- as.integer(regmatches(sub_pil, regexpr("^[0-9]+", sub_pil)))
  }

  dt[, c("pillar", "dimension") := list(pil_vec, dim_vec)]

  return(dt)
}

# ---------------------------------------------------------------------------
# Step 2: Cache directory helpers
# ---------------------------------------------------------------------------

#' Resolve (and create if necessary) the spiR inventory cache directory
#'
#' Thin wrapper around [tools::R_user_dir()] so tests can redirect the cache
#' to a temporary directory via
#' `local_mocked_bindings(.spi_inv_cache_dir = function() tmp)`.
#' The resolved location is platform-dependent
#' (e.g. `~/.cache/spiR` on Unix, `%LOCALAPPDATA%/spiR` on Windows).
#'
#' @importFrom tools R_user_dir
#' @return Character scalar: absolute path to the cache directory.
#' @keywords internal
.spi_inv_cache_dir <- function() {
  dir <- tools::R_user_dir("spiR", "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  return(dir)
}

#' Build the absolute path to a version's inventory cache file
#'
#' Constructs the cache file path and validates that `version` does not
#' contain path separators (`/` or `\\`), protecting against path traversal
#' attacks. Thin wrapper so tests can exercise path construction in
#' isolation.
#'
#' @param version Character. Branch name (e.g. `"master"`, `"SPI2023"`).
#'   Must not contain `/` or `\\`.
#' @return Character scalar: path to `tree_{version}.rds` inside the cache
#'   directory.
#' @keywords internal
.spi_inv_cache_path <- function(version) {
  # Guard against path traversal: version must not contain path separators.
  if (grepl("[/\\\\]", version)) {
    cli::cli_abort(c(
      "{.arg version} must not contain path separators.",
      "x" = "Got: {.val {version}}"
    ))
  }
  file.path(.spi_inv_cache_dir(), paste0("tree_", version, ".rds"))
}

# ---------------------------------------------------------------------------
# Step 3: Cache read / write
# ---------------------------------------------------------------------------

#' Write an enriched inventory tree to the on-disk cache
#'
#' Validates that `tree_dt` is a non-empty `data.table` (delegating detailed
#' column checks to [.spi_enrich_tree()]), enriches it, then saves a named
#' list with `schema_version`, `timestamp`, and `tree` as an RDS file keyed
#' by the branch name.
#'
#' @param tree_dt A raw `data.table` from the GitHub tree crawler.
#' @param version Character. Branch name (determines the cache file name).
#' @return The enriched `data.table`, invisibly.
#' @keywords internal
.spi_inv_write_cache <- function(tree_dt, version) {
  if (!data.table::is.data.table(tree_dt)) {
    cli::cli_abort(
      "{.arg tree_dt} must be a {.cls data.table}, not {.cls {class(tree_dt)[1L]}}."
    )
  }
  if (nrow(tree_dt) == 0L) {
    cli::cli_abort(c(
      "Crawler returned an empty tree for version {.val {version}}.",
      "i" = "An empty tree is almost certainly a network or API error.",
      "i" = "Check your connection and try {.fn spi_update_inventory} again."
    ))
  }
  enriched  <- .spi_enrich_tree(tree_dt)
  cache_obj <- list(
    schema_version = SPI_INVENTORY_CACHE_SCHEMA_VERSION,
    timestamp      = Sys.time(),
    tree           = enriched
  )
  # compress = FALSE gives faster cold reads; file size is negligible at this scale.
  saveRDS(cache_obj, file = .spi_inv_cache_path(version), compress = FALSE)
  invisible(enriched)
}

#' Read and validate the on-disk inventory cache for a version
#'
#' Returns `NULL` (so the caller knows to re-crawl) in these situations:
#' \itemize{
#'   \item File does not exist (silent).
#'   \item File cannot be read (corrupt). A warning is emitted and the file
#'     is deleted.
#'   \item File has an unexpected list structure. A warning is emitted and
#'     the file is deleted.
#'   \item File has an incompatible `schema_version`. A warning is emitted
#'     and the file is deleted.
#'   \item Timestamp is not a valid `POSIXct` (e.g. tampered RDS). A warning
#'     is emitted and the file is deleted.
#'   \item File is older than 30 days, or the timestamp is in the future
#'     (clock skew). Silent expiry -- caller will re-crawl.
#'   \item Tree is not a `data.table` or is missing expected columns.
#'     A warning is emitted and the file is deleted.
#'   \item Tree `size` column is not numeric or integer. A warning is emitted
#'     and the file is deleted.
#'   \item Tree `path` column is not character. A warning is emitted and the
#'     file is deleted.
#'   \item Tree `type` column contains values other than `"blob"` or
#'     `"tree"`. A warning is emitted and the file is deleted.
#' }
#'
#' @param version Character. Branch name.
#' @return A `data.table` (the cached tree), or `NULL`.
#' @keywords internal
.spi_inv_read_cache <- function(version) {
  path <- .spi_inv_cache_path(version)

  if (!file.exists(path)) return(NULL)

  # --- Attempt to deserialise --------------------------------------------
  cache_obj <- tryCatch(
    readRDS(path),
    error = function(e) {
      cli::cli_warn(c(
        "Cached inventory for version {.val {version}} is corrupted and will be deleted.",
        "i" = "It will be re-fetched on the next call.",
        "x" = "Caused by: {conditionMessage(e)}"
      ))
      unlink(path)
      NULL
    }
  )
  if (is.null(cache_obj)) return(NULL)

  # --- Validate structure -------------------------------------------------
  expected_names <- c("schema_version", "timestamp", "tree")
  if (!is.list(cache_obj) || !all(expected_names %in% names(cache_obj))) {
    cli::cli_warn(c(
      "Cached inventory for version {.val {version}} has an unexpected structure and will be deleted.",
      "i" = "It will be re-fetched on the next call."
    ))
    unlink(path)
    return(NULL)
  }

  # --- Check schema version -----------------------------------------------
  if (!identical(cache_obj[["schema_version"]], SPI_INVENTORY_CACHE_SCHEMA_VERSION)) {
    cli::cli_warn(c(
      "Cached inventory for version {.val {version}} uses an incompatible schema (v{cache_obj$schema_version}) and will be deleted.",
      "i" = "Expected schema v{SPI_INVENTORY_CACHE_SCHEMA_VERSION}. It will be re-fetched on the next call."
    ))
    unlink(path)
    return(NULL)
  }

  # --- Validate timestamp type (guards against manual RDS edits) ----------
  if (!inherits(cache_obj[["timestamp"]], "POSIXct")) {
    cli::cli_warn(c(
      "Cached inventory for version {.val {version}} has an invalid timestamp and will be deleted.",
      "i" = "It will be re-fetched on the next call."
    ))
    unlink(path)
    return(NULL)
  }

  # --- Check TTL ----------------------------------------------------------
  age_days <- as.numeric(
    difftime(Sys.time(), cache_obj[["timestamp"]], units = "days")
  )
  if (age_days < 0 || age_days > SPI_INVENTORY_CACHE_TTL_DAYS) {
    return(NULL)  # Silent expiry (or clock skew) — caller will re-crawl
  }

  # --- Validate tree schema -----------------------------------------------
  tree          <- cache_obj[["tree"]]
  expected_cols <- c("path", "type", "size", "category", "pillar", "dimension")
  if (!data.table::is.data.table(tree) || !all(expected_cols %in% names(tree))) {
    cli::cli_warn(c(
      "Cached inventory for version {.val {version}} has an unexpected tree schema and will be deleted.",
      "i" = "It will be re-fetched on the next call."
    ))
    unlink(path)
    return(NULL)
  }

  # --- Validate tree column types -----------------------------------------
  if (!is.character(tree[["path"]])) {
    cli::cli_warn(c(
      "Cached inventory for version {.val {version}} has an invalid {.field path} column type and will be deleted.",
      "i" = "It will be re-fetched on the next call."
    ))
    unlink(path)
    return(NULL)
  }

  if (!is.integer(tree[["size"]]) && !is.numeric(tree[["size"]])) {
    cli::cli_warn(c(
      "Cached inventory for version {.val {version}} has an invalid {.field size} column type and will be deleted.",
      "i" = "It will be re-fetched on the next call."
    ))
    unlink(path)
    return(NULL)
  }

  valid_types <- tree[["type"]] %in% c("blob", "tree") | is.na(tree[["type"]])
  if (!all(valid_types)) {
    cli::cli_warn(c(
      "Cached inventory for version {.val {version}} contains unexpected {.field type} values and will be deleted.",
      "i" = "It will be re-fetched on the next call."
    ))
    unlink(path)
    return(NULL)
  }

  return(tree)
}

# ---------------------------------------------------------------------------
# Step 4: Inventory resolver
# ---------------------------------------------------------------------------

#' Internal: resolve the inventory tree for a given SPI version
#'
#' Returns the on-disk cached tree when valid and fresh. Falls back to
#' crawling the GitHub repository when the cache is absent, expired, or
#' corrupt. Errors clearly when neither cache nor network is available.
#'
#' This is the single entry point for all inventory consumers (e.g.
#' `spi_inventory()`, `spi_get_raw()` — planned features).
#'
#' @param version Character. Branch name. Default `"master"`.
#' @return A `data.table` with columns `path`, `type`, `size`, `category`,
#'   `pillar`, `dimension`.
#' @keywords internal
.spi_get_inventory <- function(version = "master") {
  .spi_validate_version(version)

  cached <- .spi_inv_read_cache(version)
  if (!is.null(cached)) return(invisible(cached))

  # Cache miss / expired — try to crawl the repository
  tree_dt <- tryCatch(
    .spi_crawl_tree(version),
    error = function(e) {
      # Chain the original condition so the true cause (network failure,
      # rate-limit, unimplemented stub, etc.) is visible in the traceback.
      cli::cli_abort(
        c(
          "No cached inventory found and the GitHub API could not be reached.",
          "i" = "Connect to the internet and try again, or call {.fn spi_update_inventory}."
        ),
        parent = e
      )
    }
  )

  result <- .spi_inv_write_cache(tree_dt, version)
  invisible(result)
}

# ---------------------------------------------------------------------------
# Step 5: Exported functions
# ---------------------------------------------------------------------------

#' Update the local SPI inventory cache
#'
#' Forces a fresh crawl of the
#' [World Bank SPI repository](https://github.com/worldbank/SPI) for the
#' specified version and writes the result to the on-disk inventory cache.
#' Useful when the remote repository has been updated and you want to
#' refresh before the automatic 30-day TTL expires.
#'
#' For reproducible workflows, call `spi_update_inventory()` explicitly
#' before distributing analysis code rather than relying on the automatic
#' 30-day TTL refresh of the internal cache.
#'
#' @param version Character. Branch name in the SPI repository. Defaults to
#'   `"master"` (latest stable). Use [spi_versions()] to list all available
#'   branches.
#'
#' @return The updated inventory `data.table`, invisibly. Each row
#'   represents one file in the SPI repository, with columns:
#'   \describe{
#'     \item{`path`}{Relative file path within the repository.}
#'     \item{`type`}{GitHub object type: `"blob"` (file) or `"tree"`
#'       (directory).}
#'     \item{`size`}{File size in bytes (`NA` for directories).}
#'     \item{`category`}{`"raw"`, `"output"`, or `"misc"`.}
#'     \item{`pillar`}{Integer 1–5, or `NA`.}
#'     \item{`dimension`}{Character `"P.D"` (e.g. `"4.1"`), or `NA`.}
#'   }
#'
#' @family spi-inventory-cache
#' @seealso [spi_clear_inventory()], [spi_versions()]
#'
#' @examples
#' \dontrun{
#' # Refresh the default version
#' spi_update_inventory()
#'
#' # Refresh a specific version
#' spi_update_inventory(version = "SPI2023")
#' }
#'
#' @section Development status:
#'   This function depends on the `github-tree-crawler` milestone. Calling
#'   it on the current development build will error with class
#'   `"spi_not_implemented"`.
#'
#' @importFrom cli cli_inform
#' @export
spi_update_inventory <- function(version = "master") {
  .spi_validate_version(version)

  tree_dt <- .spi_crawl_tree(version)
  result  <- .spi_inv_write_cache(tree_dt, version)
  cli::cli_inform("Inventory updated for version {.val {version}}.")
  invisible(result)
}

#' Clear the local SPI inventory cache
#'
#' Deletes on-disk inventory cache files written by [spi_update_inventory()]
#' or automatically by inventory functions. Does **not** affect the
#' in-session download cache — use [spi_clear_cache()] for that.
#'
#' @param version Character or `NULL`. A branch name deletes only that
#'   version's cache file. `NULL` (default) deletes all cached inventory
#'   files.
#'
#' @return `NULL`, invisibly.
#'
#' @family spi-inventory-cache
#' @seealso [spi_update_inventory()], [spi_clear_cache()]
#'
#' @examples
#' \dontrun{
#' # Delete the cache for one version
#' spi_clear_inventory("master")
#'
#' # Delete all inventory cache files
#' spi_clear_inventory()
#' }
#'
#' @export
spi_clear_inventory <- function(version = NULL) {
  if (!is.null(version)) {
    .spi_validate_version(version)
    path <- .spi_inv_cache_path(version)
    if (file.exists(path)) {
      unlink(path)
      cli::cli_inform("Inventory cache cleared for version {.val {version}}.")
    } else {
      cli::cli_inform("No inventory cache found for version {.val {version}}.")
    }
  } else {
    # Use .spi_inv_cache_dir() so that tests can mock the cache location.
    # list.files() returns character(0) on a fresh directory, so no special
    # "dir doesn't exist" guard is needed.
    cache_dir <- .spi_inv_cache_dir()
    files     <- list.files(cache_dir, pattern = "^tree_.*\\.rds$", full.names = TRUE)
    if (length(files) == 0L) {
      cli::cli_inform("Inventory cache is already empty.")
    } else {
      unlink(files)
      cli::cli_inform("Inventory cache cleared ({length(files)} file{?s} deleted).")
    }
  }

  invisible(NULL)
}
