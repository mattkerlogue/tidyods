# this is largely based on previous row-by-row approach used in process.R

read_large_ods <- function(path, sheet = 1) {

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
  ns <- xml2::xml_ns(ods_xml)

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
    cli::cli_abort(
      c(
        "You can only read a single sheet from a large file"
      )
    )
  }

  tbl_xml <- xml2::xml_find_first(x = ods_xml, xpath = sheet_path, ns = ns)

  cell_content <- extract_rows_large(tbl_xml, ns = ns)

  return(cell_content)

}

extract_rows_large <- function(tbl_nodes, ns) {

  row_nodes <- xml2::xml_find_all(
    x = tbl_xml,
    xpath = "descendant::table:table-row",
    ns = ns
  )

  pbid <- cli::cli_progress_bar("Processing rows", total = length(row_nodes))

  row_content <- purrr::map(row_nodes, extract_cells_large, ns = ns, pbid = pbid)

  return(row_content)

}

extract_cells_large <- function(row_node, ns, pbid) {

  cell_nodes <- xml2::xml_find_all(
    x = row_node,
    xpath = "descendant::table:table-cell | descendant::table:covered-table-cell",
    ns = ns
  )

  cell_paths <- xml2::xml_path(cell_nodes)
  cell_text <- xml2::xml_text(cell_nodes)

  cli::cli_progress_update(id = pbid)

  return(stats::setNames(cell_text, cell_paths))

}
