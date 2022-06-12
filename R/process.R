# Internal functions for processing ODS files

# unzip and ods file and get content xml
unzip_ods_xml <- function(ods_file) {

  temp_dir <- tempdir()

  # if remote file location download the file to temp_dir
  if (grepl("^((http|ftp)s?|sftp)://", ods_file)) {
    cli::cli_progress_message("Reading remote file")
    utils::download.file(
      url = ods_file,
      destfile = file.path(temp_dir, basename(ods_file)),
      quiet = TRUE
    )
    ods_file <- file.path(temp_dir, basename(ods_file))
  } else if (!file.exists(ods_file)) {
    cli::cli_abort("{.file {ods_file}} does not exist")
  }

  if (tolower(tools::file_ext(ods_file) != "ods")) {
    cli::cli_abort("{.file {ods_file}} does not have a valid extension, .ods")
  }

  cli::cli_progress_message("Unzipping ODS file")
  utils::unzip(ods_file, files = "content.xml", exdir = temp_dir,
               overwrite = TRUE)
  return(file.path(temp_dir, "content.xml"))

}

# read the extracted XML file
extract_ods_xml <- function(ods_file) {
  xml_file <- unzip_ods_xml(ods_file)
  if (!file.exists(xml_file)){
    cli::cli_abort("Error in unzip procedure")
  }
  cli::cli_progress_message("Reading XML file")
  xml2::read_xml(xml_file)
}

# get xpaths of sheets within the XML
ods_sheet_paths <- function(ods_xml, ods_ns) {

  # get nodes
  tbl_nodes <- xml2::xml_find_all(
    x = ods_xml,
    xpath = "/office:document-content/office:body/office:spreadsheet/table:table",
    ns = ods_ns
  )

  # get xpaths and name with sheet name
  tbl_paths <- xml2::xml_path(tbl_nodes)
  names(tbl_paths) <- xml2::xml_attr(
    x = tbl_nodes,
    attr = "table:name",
    ns = ods_ns
  )

  return(tbl_paths)

}

# get the XML node of a sheet
get_tbl_xml <- function(ods_file, sheet_name, .rows_only = TRUE) {

  cli::cli_progress_message("Getting ODS sheet")

  # get the XML
  ods_xml <- extract_ods_xml(ods_file)
  ods_ns <- xml2::xml_ns(ods_xml)

  # get sheet paths
  ods_sheets <- ods_sheet_paths(ods_xml, ods_ns)

  # evaluate sheet argument
  if (length(sheet_name) != 1) {
    cli::cli_abort(
      c(
        "{.arg sheet_name} must be of length 1",
        x = "You supplied a vector of length {.val {length(sheet_name)}}"
      )
    )
  } else if (!is.character(sheet_name)) {
    cli::cli_abort(
      c(
        "{.arg sheet_name} must be a character vector",
        x = "You supplied a {.cls {class(sheet_name)}} vector"
      )
    )
  } else if (!(sheet_name %in% names(ods_sheets))) {
    cli::cli_abort(
      c(
        "Invalid {.arg sheet_name} provided.",
        x = "{.val {sheet_name}} does not exist in {.file {ods_file}}",
        i = "Use {.fun ods_sheets} to get a list of sheets for this file."
      )
    )
  }

  # get the xpath for the sheet
  tbl_path <- ods_sheets[sheet_name]

  # find the sheet node
  tbl_node <- xml2::xml_find_all(
    x = ods_xml,
    xpath = tbl_path,
    ns = ods_ns
  )

  # default is to extract only the table row objects
  if (.rows_only) {
    tbl_node <- xml2::xml_find_all(
      x = tbl_node,
      xpath = "table:table-row",
      ns = ods_ns
    )
  }

  return(tbl_node)

}

# turn the table node into a tibble
extract_table <- function(tbl_xml) {

  # set progress bar id
  pbid <- cli::cli_progress_bar(
    "Processing rows",
    total = length(tbl_xml),
    format_done =
      "{prettyNum(cli::pb_total, ',')} rows processed in {cli::pb_elapsed}",
    clear = FALSE
  )

  # process rows
  row_data <- tibble::tibble(
    base_row = seq_along(tbl_xml),
    row_repeats = as.numeric(xml2::xml_attr(tbl_xml, "number-rows-repeated")),
    row_nodes = list(tbl_xml),
    row_content = purrr::map(tbl_xml, extract_row, .pb_id = pbid)
  )

  # if row is not repeated set repeat weight to 1
  row_data$row_repeats[is.na(row_data$row_repeats)] <- 1

  # if final row is blank set its repeat to 0
  # MS Excel has a habit of including the final row as blank
  # and a large replication weight
  if (xml2::xml_text(tbl_xml[length(tbl_xml)]) == "") {
    row_data$row_repeats[length(tbl_xml)] <- 0
  }

  # create output tibble
  ods_cells <- row_data %>%
    dplyr::select(base_row, row_repeats, row_content) %>%
    tidyr::unnest(row_content) %>%
    dplyr::mutate(
      cell_repeats = dplyr::case_when(
        cell_children == 0 ~ 1,
        is.na(cell_repeats) ~ 1,
        TRUE ~ cell_repeats
      )
    ) %>%
    tidyr::uncount(
      weights = cell_repeats,
      .id = "col_iteration",
      .remove = FALSE
    ) %>%
    tidyr::uncount(
      weights = row_repeats,
      .id = "row_iteration",
      .remove = FALSE
    ) %>%
    dplyr::arrange(base_row, row_iteration, base_col, col_iteration) %>%
    dplyr::group_by(base_row, row_iteration, .add = FALSE) %>%
    dplyr::mutate(row = dplyr::cur_group_id()) %>%
    dplyr::group_by(base_col, col_iteration, .add = FALSE) %>%
    dplyr::mutate(col = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cell_type = dplyr::case_when(
        cell_element == "covered-table-cell" ~ "merged",
        is.na(value_type) ~ "empty",
        TRUE ~ "cell"
      ),
      base_value = dplyr::case_when(
        is.na(value_type) ~ NA_character_,
        value_type == "boolean" ~ value_bool,
        value_type == "currency" ~ value_numeric,
        value_type == "date" ~ value_date,
        value_type == "float" ~ value_numeric,
        value_type == "percentage" ~ value_numeric,
        value_type == "string" & !is.na(value_string) ~ value_string,
        value_type == "string" & is.na(value_string) ~ cell_content,
        value_type == "time" ~ value_time,
        TRUE ~ cell_content
      ),
      value_numeric = as.numeric(value_numeric),
      value_bool = as.logical(value_bool),
    ) %>%
    dplyr::select(
      row, col, cell_type, value_type, cell_formula, cell_content, base_value,
      currency_symbol = value_currency)

  return(ods_cells)

}

# extract cells from a row
extract_row <- function(row_node, .pb_id = NULL) {

  # get cell nodes
  cell_nodes <- xml2::xml_children(row_node)

  # process cells
  row_cells <- tibble::tibble(
    base_col = seq_along(cell_nodes),
    cell_element = xml2::xml_name(cell_nodes),
    cell_repeats = as.numeric(
      xml2::xml_attr(cell_nodes, "number-columns-repeated")
    ),
    cell_children = xml2::xml_length(cell_nodes),
    cell_formula = xml2::xml_attr(cell_nodes, "formula"),
    cell_content = xml2::xml_text(cell_nodes),
    value_type = xml2::xml_attr(cell_nodes, "value-type"),
    value_numeric = xml2::xml_attr(cell_nodes, "value"),
    value_bool = xml2::xml_attr(cell_nodes, "boolean-value"),
    value_date = xml2::xml_attr(cell_nodes, "date-value"),
    value_time = xml2::xml_attr(cell_nodes, "time-value"),
    value_string = xml2::xml_attr(cell_nodes, "string-value"),
    value_currency = xml2::xml_attr(cell_nodes, "currency")
  )

  # detect replicated whitespace
  multispace_paths <- xml2::xml_path(
    xml2::xml_find_all(
      cell_nodes,
      "child::*/child::text:s/ancestor::table:table-cell")
  )

  # replicate whitespace
  if (length(multispace_paths) > 0) {
    multispace_cells <- as.numeric(
      gsub(".*/table:table-cell\\[(\\d+)\\]$",
           "\\1",
           multispace_paths)
    )

    for (cell in multispace_cells) {
      cell_contents <- xml2::xml_contents(xml2::xml_child(cell_nodes[cell]))
      cell_text <- character()
      for (el in cell_contents) {
        if (xml2::xml_name(el) == "s") {
          s_reps <- as.numeric(xml2::xml_attr(el, "c"))
          s_reps <- ifelse(is.na(s_reps), 1, s_reps)
          cell_text <- c(cell_text, rep(" ", s_reps))
        } else {
          cell_text <- c(cell_text, xml2::xml_text(el))
        }
      }
      cell_text <- paste0(cell_text, collapse = "")
      row_cells$cell_content[cell] <- cell_text
    }

  }

  # update the progress bar
  if (!is.null(.pb_id)){
    cli::cli_progress_update(id = .pb_id)
  }

  return(row_cells)

}
