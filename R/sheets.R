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
#' @export
ods_sheets <- function(path) {

  ods_xml <- extract_ods_xml(path)

  sheets <- ods_sheet_paths(ods_xml, .names_only = TRUE)

  return(sheets)

}

ods_sheet_paths <- function(ods_xml, ns = NULL, .names_only = FALSE) {

  if (is.null(ns)) {
    ns <- xml2::xml_ns(ods_xml)
  }

  sheet_nodes <- xml2::xml_find_all(ods_xml, "//table:table", ns)

  sheet_names <- xml2::xml_attr(sheet_nodes, "table:name", ns = ns)

  if (.names_only) {

    return(sheet_names)

  } else {

    sheet_paths <- xml2::xml_path(sheet_nodes)
    names(sheet_paths) <- sheet_names

    return(sheet_paths)

  }

}

get_sheet_path <- function(ods_xml, sheet, ns = NULL) {

  if (length(sheet) != 1) {
    cli::cli_abort("{.arg sheet} must a character or numeric vector of length 1")
  }

  sheet_paths <- ods_sheet_paths(ods_xml, ns)

  if (is.character(sheet)) {
    if (!(sheet %in% names(sheet_paths))) {
      cli::cli_abort("{.val {sheet}} is not a valid sheet name")
    }
  } else if (is.numeric(sheet)) {
    if (sheet < 0 || sheet > length(sheet_paths)) {
      cli::cli_abort("{.val {sheet}} is not a valid sheet index")
    }
  } else {
    cli::cli_abort("{.arg sheet} must be the name of a sheet or index number")
  }

  return(sheet_paths[sheet])

}
