# Filtering helpers for spi_get() and spi_download().
# None of these functions are exported.

# ---------------------------------------------------------------------------
# Known World Bank aggregate/group ISO3C codes.
# These appear in SPI_databank_country_and_aggregates.csv but do NOT
# represent individual countries. The list is derived from the WB country
# classification and changes rarely.
# ---------------------------------------------------------------------------
SPI_AGGREGATE_CODES <- c(
  "AFE", "AFW", "ARB", "CEB", "CSS",
  "EAP", "EAR", "EAS", "ECA", "ECS", "EMU", "EUU",
  "FCS",
  "HIC", "HPC",
  "IBD", "IBT", "IDA", "IDB", "IDX",
  "LAC", "LCN", "LDC", "LIC", "LMC", "LMY", "LTE",
  "MEA", "MIC", "MNA",
  "NAC",
  "OED", "OSS",
  "PRE", "PSS", "PST",
  "SAS", "SSA", "SSF", "SST",
  "TEA", "TEC", "TLA", "TMN", "TSA", "TSS",
  "UMC",
  "WLD"
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Check whether an ISO3C code is a WB aggregate/group code
#'
#' @param code Character vector of ISO3C codes.
#' @return Logical vector, TRUE where the code is a known WB aggregate.
#' @keywords internal
is_aggregate_code <- function(code) {
  code %in% SPI_AGGREGATE_CODES
}

#' Identify identifier/metadata columns in a wide SPI data.table
#'
#' Columns that do NOT start with `SPI.D<digit>` or `RAW.D<digit>` are
#' treated as identifiers (e.g. `iso3c`, `date`, `country`, `SPI.INDEX`,
#' `SPI.DIM*`). These are always preserved during column filtering.
#'
#' @param dt A `data.table`.
#' @return Character vector of column names.
#' @keywords internal
identify_id_columns <- function(dt) {
  cols <- names(dt)
  # Keep columns that are NOT SPI indicator columns (SPI.D<n>) or RAW
  # indicator columns (RAW.D<n>). Aggregate index columns like SPI.INDEX,
  # SPI.INDEX.PIL1, SPI.DIM*, etc. don't match this pattern and are kept.
  cols[!grepl("^SPI\\.D[0-9]|^RAW\\.D[0-9]", cols)]
}

#' Filter wide SPI data.table columns by regex pattern (internal helper)
#'
#' Keeps identifier columns plus all indicator columns whose names match
#' `pattern`. Shared implementation for [filter_columns_by_pillar()] and
#' [filter_columns_by_dimension()].
#'
#' @param dt A `data.table` (wide format).
#' @param pattern Character scalar. A regex to match against column names.
#' @return A `data.table` with only the relevant columns.
#' @keywords internal
filter_columns_by_pattern <- function(dt, pattern) {
  cols         <- names(dt)
  is_id        <- !grepl("^SPI\\.D[0-9]|^RAW\\.D[0-9]", cols)
  is_indicator <- grepl(pattern, cols)
  dt[, cols[is_id | is_indicator], with = FALSE]
}

#' Filter wide SPI data.table columns by pillar
#'
#' Keeps identifier columns plus all indicator columns belonging to the
#' specified pillar (`SPI.D{pillar}.*` and `RAW.D{pillar}.*`).
#'
#' @param dt A `data.table` (wide format, e.g. from SPI_data.csv or
#'   SPI_index.csv).
#' @param pillar Integer 1–5, or `NULL` (no filtering).
#' @return A `data.table` with only the relevant columns.
#' @keywords internal
filter_columns_by_pillar <- function(dt, pillar) {
  if (is.null(pillar)) return(dt)
  pattern <- paste0("^SPI\\.D", pillar, "\\.|^RAW\\.D", pillar, "\\.")
  filter_columns_by_pattern(dt, pattern)
}

#' Filter wide SPI data.table columns by dimension
#'
#' Keeps identifier columns plus all indicator columns belonging to the
#' specified dimension (e.g. `"5.2"` matches `SPI.D5.2.*` and
#' `RAW.D5.2.*`).
#'
#' @param dt A `data.table` (wide format).
#' @param dimension Character string in `"P.D"` format (e.g. `"5.2"`),
#'   or `NULL` (no filtering).
#' @return A `data.table` with only the relevant columns.
#' @keywords internal
filter_columns_by_dimension <- function(dt, dimension) {
  if (is.null(dimension)) return(dt)
  dim_escaped <- gsub("\\.", "\\\\.", dimension)
  pattern <- paste0("^SPI\\.D", dim_escaped, "\\.|^RAW\\.D", dim_escaped, "\\.")
  filter_columns_by_pattern(dt, pattern)
}

#' Filter long aggregates data.table rows by pillar and/or dimension
#'
#' Filters the `source_id` column in the aggregates long-format data.table.
#' When both `pillar` and `dimension` are provided, `dimension` (more
#' specific) takes precedence.
#'
#' @param dt A `data.table` (long format, from
#'   `SPI_databank_country_and_aggregates.csv`).
#' @param pillar Integer 1–5, or `NULL`. Matches rows where `source_id`
#'   starts with `SPI.D{pillar}.` (individual indicators). Note: pillar-level
#'   summary rows (e.g. `SPI.INDEX.PIL1`) are **not** matched by this filter;
#'   to retrieve those, omit `pillar` and filter on `source_id` manually.
#' @param dimension Character string in `"P.D"` format, or `NULL`.
#' @return A filtered `data.table`.
#' @keywords internal
filter_rows_by_pillar_dimension <- function(dt, pillar, dimension) {
  if (!is.null(dimension)) {
    # dimension is more specific — use it and ignore pillar
    dim_escaped <- gsub("\\.", "\\\\.", dimension)
    pattern <- paste0("^SPI\\.D", dim_escaped, "\\.")
    keep <- grepl(pattern, dt[["source_id"]])
    return(dt[keep])
  }

  if (!is.null(pillar)) {
    pattern <- paste0("^SPI\\.D", pillar, "\\.")
    keep <- grepl(pattern, dt[["source_id"]])
    return(dt[keep])
  }

  dt
}