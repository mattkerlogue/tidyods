#' Read cells from an ODS sheet
#'
#' Import cells from an OpenDocument Spreadsheet (ODS) file. In the resulting
#' dataset each cell is its own row with columns providing information about
#' the cell's position, value types and containing formulas.
#'
#' @param path The ODS file
#' @param sheet The sheet name or index number, set to NA for all sheets
#' @param quick Whether to use the quick reading process
#' @param quiet Whether to silence console messages (recommended for bulk processing)
#'
#' @return
#' A tibble (data.frame) of cells from the ODS sheet(s).
#'
#' @details
#' The goal of `read_ods_cells()` is to extract the constituent value(s) and
#' other information about cells stored in an ODS file and to present that
#' in a "tidy" format that allows further programmatic manipulation. It is
#' modelled after the functionality of [tidyxl::tidy_xlsx()] which performs a
#' similar role for Microsoft Excel spreadsheets.
#'
#' There are between two to four presentations of a cell's value in the
#' resulting tibble:
#'  - the `base_value`, a character vector providing the "most raw" version of
#'    a cell's value (see below regarding variation when setting `quick=TRUE`);
#'  - the `cell_content`, a character vector providing the version of a cell's
#'    value as seen by the user of a spreadsheet application;
#'  - for float, currency percentage value types, a `numeric_value` with the raw
#'    value of the cell as a base R numeric vector, currency value types also
#'    have a `currency_symbol` providing the 3-character ISO currency symbol;
#'  - for date and time value types, a character vector with the raw value
#'    conforming to the relevant ISO standard.
#'
#' Processing the ODS XML is a memory intensive process, you can achieve
#' significant speed enhancements by setting the `quick` argument to FALSE.
#' This process will extract only a minimum of information about the cells,
#' namely: `sheet`, `row`, `col`, `value_type` and a `base_value`. The
#' `base_value` when using the `quick` argument will combine the raw value
#' stored for float and percentage value types with the `cell_content` (i.e
#' formatted character string) for all other value types.
#'
#' #' @details # Cell information
#' The resulting data.frame has 25 columns:
#'
#' - `sheet`: the sheet the cell is from
#' - `row`: the row number of the cell
#' - `col`: the column number of the cell
#' - `cell_type`: the type of cell, either `cell`, `empty`, `merge-hidden` or
#'   `merge-lead`
#' - `is_empty`: a logical vector indicating if the cell is empty
#' - `value_type`: the value type, either `boolean`, `currency`, `date`,
#'    `float`, `percentage`, `string` or `time`
#' - `cell_content`: the content as presented to users of a spreadsheet application
#' - `base_value`: the "base" value of the cell
#' - `numeric_value`: base values for numeric value types as a numeric vector
#' - `currency_symbol`: the currency symbol associated with currency value types
#' - `boolean_value`: base values for boolean value types as a logical vector
#' - `date_value`: base values for date value types (incl date-time) as a character vector
#' - `time_value`: base value for time value types as a character vector
#' - `has_formula`: a logical vector indicating if the cell content are calculated by a formula
#' - `formula`: the formula used to calculate the cell content
#' - `has_error`: a logical vector indicating if the cell formula results in an error
#' - `error_type`: a numeric vector indicating the type of error
#' - `has_annotation`: a logival vector indicating if the cell has a comment or annotation
#' - `annotation`: the text of any comment or annotation
#' - `is_merged`: a logical indicating if the cell is part of a merge group
#' - `merge_colspan`: for merge leader cells, the number of columns spanned
#' - `merge_rowspan`: for merge leader cells, the number of rows spanned
#' - `merge_shape`: for merge leader cells, the shape of the merge group, either `vetical`,
#'   `horizontal`, or `rectangle`
#' - `cell_style`: the style code of the cell
#' - `row_style`: the style code of the row
#'
#' ## Cell types and merge groups
#'
#' Cells are assigned one of four `cell_type` values:
#'
#' - a cell is `empty` if it has no content or no value type,
#' - a cell is a `merge-lead` if it is the top-left cell of a merge group,
#'   other cells in the merge group are assigned `merge-hidden`
#' - everything else is simply a `cell`
#'
#' Cells in a merge group will also have `is_merged` set to `TRUE`. The top-left
#' cell in a merge group has information about the number of columns
#' (`merge_colspan`) and rows (`merge_rowspan`) in the merge group as well as
#' an indication of shape (`vertical`, `horizontal` or `rectangle`). Merge
#' groups are not uniquely identified.
#'
#' The ODS specification allows for cells hidden by a merge to have content,
#' annecdotally this appears to be supported by ODS files created by
#' LibreOffice but not by ODS files created by Microsoft Excel.
#'
#' ## Value types
#'
#' The `value_type` of a cell is based on the
#' ["value-type" attribute](https://docs.oasis-open.org/office/OpenDocument/v1.3/os/part3-schema/OpenDocument-v1.3-os-part3-schema.html#__RefHeading__1417680_253892949)
#' defined by the OpenDocument Format specification. There are 7 value types:
#'
#' - `boolean`: equivalent to R's [base::logical()] data type
#' - `currency`: a numeric value, optionally with a currency symbol
#' - `date`: a date or date-time stored in ISO format
#' - `float`: a numeric value, the ODS format does not distinguish between
#'   different numeric types
#' - `percentage`: a numeric value, formatted for display as a percentage
#' - `string`: equivalent to R's [base::character()]
#' - `time`: a duration, stored in ISO format
#'
#' For currency value types optionally a currency symbol (e.g. GBP, EUR, USD)
#' can be set, this is provided separately from the `base_value` as
#' `currency_symbol`, the formatting of currency symbols is handled by style
#' information, the value as visible to a spreadsheet user is provided in the
#' `cell_content` column.
#'
#' ## Error types
#' The values of `error_type` are equivalent to those produced by
#' [Microsoft Excel](https://support.microsoft.com/en-us/office/error-type-function-10958677-7c8d-44f7-ae77-b9a9ee6eefaa)
#' and [LibreOffice](https://help.libreoffice.org/7.3/en-GB/text/scalc/01/func_error_type.html?&DbPAR=SHARED&System=MAC).
#'
#' @examples
#' example <- system.file("extdata", "basic_example.ods", package = "tidyods")
#' example_cells <- read_ods_cells(example, 1)
#' dplyr::glimpse(example_cells)
#' @export
read_ods_cells <- function(path, sheet = 1, quick = FALSE, quiet = FALSE) {

  if (missing(path)) {
    cli::cli_abort("{.arg path} is not defined")
  } else if (length(path) != 1) {
    cli::cli_abort("{.arg path} must be a character vector of length 1")
  } else if (!is.character(path)) {
    cli::cli_abort("{.arg path} must be a character vector of length 1")
  }

  if (length(sheet) != 1) {
    cli::cli_abort("{.arg sheet} must be a character or numeric vector of length 1")
  }

  if (!(is.character(sheet) | is.numeric(sheet) | is.na(sheet))) {
    cli::cli_abort("{.arg sheet} must be a character or numeric vector of length 1")
  }

  if (!quiet) cli::cli_progress_step("Extracting ODS XML")
  ods_xml <- extract_ods_xml(ods_file = path)
  ns <- xml2::xml_ns(ods_xml)

  if (is.na(sheet)) {
    sheet_path <- NA
  } else {
    sheet_path <- get_sheet_path(ods_xml, sheet, ns)
  }

  if (!quiet) cli::cli_progress_step(
    msg = "Extracting cell and row info...",
    msg_done = "Extracting cell and row info",
  )

  if (quick) {
    ods_cells <- extract_cells_quick(ods_xml, sheet_path, ns)
  } else {
    ods_cells <- extract_cells_full(ods_xml, sheet_path, ns)
  }

  return(ods_cells)

}

#' Read an ODS sheet to a rectangular dataset
#'
#' A wrapper around [`read_ods_cells()`] and one of the
#' [rectify functions][tidyods::simple_rectify()] provided in the package.
#'
#' @param path The ODS file
#' @param sheet The sheet within the ODS file
#' @param rectify The method to convert cells to two-dimensions, either "simple"
#'   or "smart (see details).
#' @param base_values Whether to use the base_value of a cell (TRUE, the default)
#'   or whether to provide the cell content as seen by a spreadsheet user.
#' @param quick Whether to use the quick reading process.
#' @param skip The number of rows to skip before attempting to rectify the cells.
#' @param col_headers Whether to use the first row (after any skipping) as the
#'   column header (`TRUE` is the default), alternatively a character vector of
#'   column names can be provided.
#' @param quiet Whether to silence console messages (recommended for bulk processing)
#'
#' @return
#' A tibble, presenting cells in a traditional two-dimension spreadsheet format.
#'
#' @details
#' When `rectify = "simple"` then the [simple_rectify()] function will be used
#' to coerce the cells to a sheet. You can instruct the rectifier to skip rows,
#' whether to use the first row as column headers (or provide your own), and
#' whether to use the underlying values or the formatted cell content for the
#' value of the output cell. If `quick = TRUE` this implies using the cell
#' content, thus the user setting for `base_values` is ignored and treated as if
#' set to `FALSE`.
#'
#' When `rectify = "smart"` then the [smart_rectify()] function will be used,
#' this will attempt to guess the location of the column headers as well as
#' coercing the columns using value type information extracted from the ODS.
#' Using the smart rectifier ignores any settings for `base_values`, `skip` and
#' `col_headers`. You cannot set `quick = TRUE` if you want to use the
#' smart rectifier.
#'
#' @examples
#' example <- system.file("extdata", "basic_example.ods", package = "tidyods")
#' read_ods_sheet(example, 1)
#' read_ods_sheet(example, 2, "smart")
#' @export
read_ods_sheet <- function(path, sheet = 1, rectify = c("simple", "smart"),
                           skip = 0, col_headers = TRUE, base_values = TRUE,
                           quick = FALSE, quiet = FALSE) {

  if (missing(path)) {
    cli::cli_abort("{.arg path} is not defined")
  } else if (length(path) != 1) {
    cli::cli_abort("{.arg path} must be a character vector of length 1")
  } else if (!is.character(path)) {
    cli::cli_abort("{.arg path} must be a character vector of length 1")
  }

  if (length(sheet) != 1) {
    cli::cli_abort("{.arg sheet} must be a character or numeric vector of length 1")
  } else if (!(is.character(sheet) | is.numeric(sheet))) {
    cli::cli_abort("{.arg sheet} must be a character or numeric vector of length 1")
  } else if (is.numeric(sheet)) {
    if (sheet < 1) {
      cli::cli_abort("If numeric {.arg sheet} must be greater than 0")
    }
  }

  rectify <- match.arg(rectify)

  if (rectify == "smart" & quick) {
    cli::cli_abort(
      c(x = "{.arg quick} cannot be set to TRUE if {.arg rectify} is set to {.val smart}")
    )
  }

  ods_cells <- read_ods_cells(path, sheet, quick = quick, quiet = quiet)

  if (!quiet) cli::cli_progress_step("Rectifying cells to sheet layout")
  if (rectify == "simple" & !quick) {
    ods_sheet <- simple_rectify(ods_cells, skip = skip,
                                col_headers = col_headers,
                                base_values = base_values)
  } else if (rectify == "simple" & quick) {
    ods_sheet <- quick_rectify(ods_cells)
  } else if (rectify == "smart") {
    ods_sheet <- smart_rectify(ods_cells)
  }

  return(ods_sheet)

}
