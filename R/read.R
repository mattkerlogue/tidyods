#' Read cells from an ODS sheet
#'
#' Import cells from an OpenDocument Spreadsheet (ODS) file. In the resulting
#' dataaset each cell is its own row with columns providing information about
#' the cell's postion, value types and containing formulas.
#'
#' @param path The ODS file
#' @param sheet The sheet within the ODS file
#' @param quick Whether to use the quick reading process
#' @param whitespace Whether to process multiple whitespaces
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
#' @examples
#' example <- system.file("extdata", "basic_example.ods", package = "tidyods")
#' example_cells <- read_ods_cells(example, 1)
#' dplyr::glimpse(example_cells)
#'
#' @export
read_ods_cells <- function(path, sheet = 1, quick = FALSE, whitespace = FALSE) {

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
    cli::cli_abort("{.arg sheet} must be a character or numeric vector of length 1")
  } else if (!(is.character(sheet) | is.numeric(sheet))) {
    cli::cli_abort("{.arg sheet} must be a character or numeric vector of length 1")
  }

  ods_xml <- extract_ods_xml(ods_file = path)

  sheets <- ods_sheet_paths(ods_xml)

  sheet_path <- NULL

  if (is.character(sheet)) {
    if (!(sheet %in% names(sheets))) {
      cli::cli_abort(
        c(
          "Invalid {.arg sheet} provided.",
          x = "{.val {sheet}} does not exist in {.file {ods_file}}",
          i = "Use {.fun ods_sheets} to get a list of sheets for this file."
        )
      )
    } else {
      sheet_path <- sheets[sheet]
    }
  } else if (is.numeric(sheet)) {
    if (sheet > length(sheets)) {
      cli::cli_abort(
        c(
          "Sheet number out of range.",
          i = "There are only {length(sheets)} sheets in {.file {ods_file}}"
        )
      )
    } else if (sheet > 0 ) {
      sheet_path <- sheets[sheet]
    }
  }

  if (is.null(sheet_path)) {
    tbl_xml <- ods_xml
  } else {
    tbl_xml <- xml2::xml_find_all(ods_xml, sheet_path)
  }

  ods_cells <- extract_table(tbl_xml, quick = quick, whitespace = whitespace)

  return(ods_cells)

}

#' Read an ODS sheet to a rectangular dataset
#'
#' A wrapper around [`read_ods_cells()`] and [`simple_rectify()`] to extract
#' cells from a sheet and return a
#'
#' @param path The ODS file
#' @param sheet The sheet within the ODS file
#' @param rectify The method to convert cells to two-dimensions, can only be "simple"
#' @param base_values Whether to use the base_value of a cell (TRUE, the default)
#'   or whether to provide the cell content as seen by a spreadsheet user.
#' @param quick Whether to use the quick reading process
#' @param whitespace Whether to process multiple whitespaces
#'
#' @return
#' A tibble, presenting cells in a traditional two-dimension spreadsheet format.
#'
#' @details
#' At present, `rectify` can only be set to "simple", this calls the
#' [simple_rectify()] method to convert the cells to a sheet.
#'
#' When `base_values = TRUE` (the default) the underlying cell values are used
#' for non-string value types. When `FALSE` then cell content as seen by a
#' spreadsheet application user will be shown.
#'
#' Setting `quick = TRUE` will ignore the setting of `base_values`, as the quick
#' process only returns cell content as seen by a spreadsheet users. When
#' `whitespace = FALSE` (the default), multiple whitespaces in cell content
#' are ignored. See [read_ods_cells()] for further details.
#'
#' @examples
#' example <- system.file("extdata", "basic_example.ods", package = "tidyods")
#' read_ods_sheet(example, 1)
#'
#' @export
read_ods_sheet <- function(path, sheet, rectify = "simple", base_values = TRUE,
                           quick = FALSE, whitespace = FALSE) {

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
    cli::cli_abort("{.arg sheet} must be a character or numeric vector of length 1")
  } else if (!(is.character(sheet) | is.numeric(sheet))) {
    cli::cli_abort("{.arg sheet} must be a character or numeric vector of length 1")
  } else if (is.numeric(sheet)) {
    if (sheet < 1) {
      cli::cli_abort("If numeric {.arg sheet} must be greater than 0")
    }
  }

  ods_cells <- read_ods_cells(path, sheet, quick = quick,
                              whitespace = whitespace)

  if (rectify == "simple" & !quick) {
    ods_sheet <- simple_rectify(ods_cells, base_values)
  } else if (rectify == "simple" & quick) {
    ods_sheet <- simple_rectify(ods_cells, base_values = FALSE)
  }

  return(ods_sheet)

}

#' List the sheets in an ODS file
#'
#' Get a list of sheets in the ODS file, either to explore the structure of a
#' file or to use as an input for iterating over a whole spreadsheet document.
#'
#' @param path The ODS file
#'
#' @return A character vector of sheet names
#'
#' @examples
#' example <- system.file("extdata", "basic_example.ods", package = "tidyods")
#' ods_sheets(example)
#'
#' @export
ods_sheets <- function(path) {

  ods_xml <- extract_ods_xml(path)

  ns <- xml2::xml_ns(ods_xml)

  sheets <- xml2::xml_attr(xml2::xml_find_all(ods_xml, "//table:table", ns = ns),
                           "table:name", ns = ns)

  return(sheets)

}
