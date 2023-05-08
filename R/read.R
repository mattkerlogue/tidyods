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
#' More details on the types of information extracted by `read_ods_cells`
#' can be found the vignette, `vignette("read_cells", package = "tidyods")`.
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

  if(!interactive()) {
    quiet <- TRUE
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

  if(!interactive()) {
    quiet <- TRUE
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
