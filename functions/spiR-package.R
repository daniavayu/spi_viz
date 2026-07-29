#' spiR: Statistical Performance Indicators Data Access
#'
#' Access World Bank Statistical Performance Indicators (SPI) data from
#' GitHub. Provides [spi_get()], [spi_data()], [spi_index()], and
#' [spi_aggregates()] to retrieve and filter the core SPI output datasets.
#'
#' @importFrom cli cli_abort cli_warn
#' @importFrom data.table data.table as.data.table fread :=
#' @importFrom httr2 request req_headers req_error req_perform resp_body_json resp_status
#' @keywords internal
"_PACKAGE"

## Suppress R CMD check note about data.table's non-standard evaluation
## (.SD, :=, etc.) where column names appear as global variables.
utils::globalVariables(c(".", ".SD"))
