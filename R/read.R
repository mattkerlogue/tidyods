#' Read cells from an ODS sheet
#'
#' Import cells from an OpenDocument Spreadsheet (ODS) file. In the resulting
#' dataaset each cell is its own row with columns providing information about
#' the cell's postion, value types and containing formulas.
#'
#' @param path The ODS file
#' @param sheet The sheet within the ODS file
#' @param quick Whether to use the quick reading process
#'
#' @return
#' A tibble with the following information:
#'
#' - `row`: the row number of the cell
#' - `col`: the column number of the cell
#' - `cell_type`: the type of cell, either "cell", "empty" or "merged"
#' - `value_type`: the ODS value type (see details)
#' - `cell_formula`: the ODS formula specification
#' - `cell_content`: the value of the cell as represented to the user of a
#'   spreadsheet application (i.e. with text formatting applied)
#' - `base_value`: the underlying value of a cell (see details)
#' - `currency symbol`: the currency symbol for cells with a `value_type` of
#'   currency
#'
#' If using the `quick` argument you only `row`, `col` and `cell_content` are
#' returned.
#'
#' @details
#' Cells are assigned a `cell_type` of "cell" when they have content, "empty"
#' if they have no content, or "merged" if they are part of a set of merged cells.
#'
#' The `value_type` of a cell is based on the ["value-type" attribute](https://docs.oasis-open.org/office/OpenDocument/v1.3/os/part3-schema/OpenDocument-v1.3-os-part3-schema.html#__RefHeading__1417680_253892949)
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
#' For currency value types optionally a currency symbol (e.g. GBP, EUR, USD) can
#' be set, this is provided separate from the `base_value` as `currency_symbol`.
#'
#' `read_ods_cells` provides two representations of a cell's value. The
#' `cell_content` column provides the cell's value as presented to the user of
#' a traditional spreadsheet application (such as Microsoft Excel, Google Sheets
#' or LibreOffice Calc), i.e. after applying cell formatting rules. The
#' `base_value` column provides the cell's underlying value, i.e. without
#' formatting.
#'
#' Processing the ODS XML is a memory intensive process, you can achieve
#' significant speed enhancements by setting the `quick` argument to FALSE,
#' this will extract only the text content of the cell. Note this also ignores
#' repeated white space elements (i.e. `"A   B   C"` will be returned as
#' `"A B C"`).
#'
#' @export
read_ods_cells <- function(path, sheet, quick = FALSE) {

  if (missing(path)) {
    cli::cli_abort("{.arg path} is not defined")
  } else if (length(path) != 1) {
    cli::cli_abort("{.arg path} must be a character vector of length 1")
  } else if (!is.character(path)) {
    cli::cli_abort("{.arg path} must be a character vector of length 1")
  }

  if (missing(sheet)) {
    cli::cli_abort("{.arg sheet} is not defined")
  } else if (length(sheet) != 1) {
    cli::cli_abort("{.arg sheet} must be a character vector of length 1")
  } else if (!is.character(sheet)) {
    cli::cli_abort("{.arg sheet} must be a character vector of length 1")
  }

  tbl_xml <- get_tbl_xml(ods_file = path, sheet_name = sheet)

  ods_cells <- extract_table(tbl_xml, quick = quick)

  return(ods_cells)

}

#' Read an ODS sheet to a rectangular dataset
#'
#' @param path The ODS file
#' @param sheet The sheet within the ODS file
#' @param rectify The method to convert cells to two-dimensions, can only be "simple"
#' @param base_values Whether to use the base_value of a cell (TRUE, the default)
#'   or whether to provide the cell content as seen by a spreadsheet user.
#' @param quick Whether to use the quick reading process
#'
#' @return
#' A tibble, presenting cells in a traditional two-dimension spreadsheet format.
#'
#' @details
#' At present, `rectify` can only be set to "simple", this calls the
#' [simple_rectify()] method to convert the cells to a sheet.
#'
#' When set to `TRUE` (the default) the `base_values` argument then the
#' underlying cell values are used. When `FALSE` then cell content as seen by a
#' spreadsheet application user will be shown. See [read_ods_cells()] for further
#' details.
#'
#' Setting `quick - TRUE` will ignore the setting of `base_values`, as the quick
#' process only returns cell content as seen by a spreadsheet users (but
#' ignoring any repeated white space in the cell content).
#'
#' @export
read_ods_sheet <- function(path, sheet, rectify = "simple", base_values = TRUE,
                           quick = FALSE) {

  if (missing(path)) {
    cli::cli_abort("{.arg path} is not defined")
  } else if (length(path) != 1) {
    cli::cli_abort("{.arg path} must be a character vector of length 1")
  } else if (!is.character(path)) {
    cli::cli_abort("{.arg path} must be a character vector of length 1")
  }

  if (missing(sheet)) {
    cli::cli_abort("{.arg sheet} is not defined")
  } else if (length(sheet) != 1) {
    cli::cli_abort("{.arg sheet} must be a character vector of length 1")
  } else if (!is.character(sheet)) {
    cli::cli_abort("{.arg sheet} must be a character vector of length 1")
  }

  ods_cells <- read_ods_cells(path, sheet, quick = quick)

  if (rectify == "simple" & !quick) {
    ods_sheet <- simple_rectify(ods_cells, base_values)
  } else if (rectify == "simple" & quick) {
    ods_sheet <- simple_rectify(ods_cells, base_values = FALSE)
  }

}

#' List the sheets in an ODS file
#'
#' Get a list of sheets in the ODS file, either to explore the structure of a
#' file or to use as an input for iterating over a whole spreadsheet document.
#'
#' @param path The ODS file
#'
#' @return A character vector of sheet names
#' @export
ods_sheets <- function(path) {

  ods_xml <- extract_ods_xml(path)
  ods_ns <- xml2::xml_ns(ods_xml)

  names(ods_sheet_paths(ods_xml, ods_ns))

}
