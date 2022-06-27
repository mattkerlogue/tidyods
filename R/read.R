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
#' - `sheet`: the sheet of the cell
#' - `row`: the row number of the cell
#' - `col`: the column number of the cell
#' - `cell_type`: the type of cell, either "cell", "empty" or "merged"
#' - `value_type`: the ODS value type (see details)
#' - `is_empty`: if the cell has no content (can apply to cell and merged types)
#' - `cell_content`: the value of the cell as represented to the user of a
#'   spreadsheet application (i.e. with text formatting applied)
#' - `base_value`: the underlying value of a cell (see details)
#' - `numeric_value`: the `base_value` as a numeric vector for numeric types
#' - `logical_value`: the `base_value` as a logical vector for boolean types
#' - `currency_symbol`: the currency symbol for cells with a `value_type` of
#'   currency
#' - `has_formula`: whether the cell has a formula
#' - `cell_formula`: the ODS formula specification
#' - `has_error`: whether the cell has an error
#' - `error_type`: the error type
#'
#' If using the `quick` argument then only `row`, `col` and `cell_content` are
#' returned.
#'
#' @details
#' Cells are assigned a `cell_type` of "merged" if they are covered by a cell
#' merge, "blank" if they have no value type or cell content, otherwise they are
#' a "cell". Use `is_empty` to determine if the cell is blank, as the ODS
#' specification allows for the retention of content in cells hidden by a merge.
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
#' For currency value types optionally a currency symbol (e.g. GBP, EUR, USD)
#' can be set, this is provided separately from the `base_value` as
#' `currency_symbol`.
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
#' The values of `error_type` are equivalent to those produced by
#' [Microsoft Excel](https://support.microsoft.com/en-us/office/error-type-function-10958677-7c8d-44f7-ae77-b9a9ee6eefaa)
#' and [LibreOffice](https://help.libreoffice.org/7.3/en-GB/text/scalc/01/func_error_type.html?&DbPAR=SHARED&System=MAC).
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

  if (length(sheet) != 1) {
    cli::cli_abort("{.arg sheet} must be a character or numeric vector of length 1")
  } else if (!(is.character(sheet) | is.numeric(sheet))) {
    cli::cli_abort("{.arg sheet} must be a character or numeric vector of length 1")
  }

  cli::cli_progress_step("Extracting XML")
  ods_xml <- extract_ods_xml(ods_file = path)

  cli::cli_progress_step("Getting sheet")
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

  ns <- xml2::xml_ns(tbl_xml)

  cli::cli_progress_step("Extracting cell and row info")
  row_tbl <- extract_rows(tbl_xml, ns, quick = quick)
  cell_tbl <- extract_cells(tbl_xml, ns, quick = quick)

  cli::cli_progress_step("Generating output table")
  ods_cells <- generate_cell_output(row_tbl, cell_tbl, sheets,
                                    quick = quick)

  cli::cli_progress_cleanup()
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
#' @param whitespace Whether to process multiple whitespaces.
#' @param skip The number of rows to skip before attempting to rectify the cells.
#' @param col_headers Whether to use the first row (after any skipping) as the
#'   column header (`TRUE` is the default), alternatively a character vector of
#'   column names can be provided.
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
#' #' read_ods_sheet(example, 2, "smart")
#'
#' @export
read_ods_sheet <- function(path, sheet = 1, rectify = c("simple", "smart"),
                           skip = 0, col_headers = TRUE, base_values = TRUE,
                           quick = FALSE, whitespace = FALSE) {

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

  ods_cells <- read_ods_cells(path, sheet, quick = quick,
                              whitespace = whitespace)

  cli::cli_progress_step("Rectifying cells to sheet layout")
  if (rectify == "simple" & !quick) {
    ods_sheet <- simple_rectify(ods_cells, skip = skip,
                                col_headers = col_headers,
                                base_values = base_values)
  } else if (rectify == "simple" & quick) {
    ods_sheet <- simple_rectify(ods_cells, skip = skip,
                                col_headers = col_headers,
                                base_values = FALSE)
  } else if (rectify == "smart") {
    ods_sheet <- smart_rectify(ods_cells)
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
