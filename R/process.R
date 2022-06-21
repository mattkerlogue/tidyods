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
ods_sheet_paths <- function(ods_xml) {

  # get nodes
  tbl_nodes <- xml2::xml_find_all(
    x = ods_xml,
    xpath = "/office:document-content/office:body/office:spreadsheet/table:table"
  )

  # get xpaths and name with sheet name
  tbl_paths <- xml2::xml_path(tbl_nodes)

  names(tbl_paths) <- xml2::xml_attr(
    x = tbl_nodes,
    attr = "table:name",
    ns = xml2::xml_ns(tbl_nodes)
  )

  return(tbl_paths)

}

# get the XML node of a sheet
get_tbl_xml <- function(ods_file, sheet, .rows_only = TRUE) {

  cli::cli_progress_message("Getting ODS sheet")

  # get the XML
  ods_xml <- extract_ods_xml(ods_file)

  # get sheet paths
  ods_sheets <- ods_sheet_paths(ods_xml)

  # evaluate sheet argument
  if (length(sheet) != 1) {
    cli::cli_abort(
      c(
        "{.arg sheet} must be of length 1",
        x = "You supplied a vector of length {.val {length(sheet)}}"
      )
    )
  } else if (!(is.character(sheet) | is.numeric(sheet))) {
    cli::cli_abort(
      c(
        "{.arg sheet} must be a character vector",
        x = "You supplied a {.cls {class(sheet)}} vector"
      )
    )
  } else if (is.character(sheet) & !(sheet %in% names(ods_sheets))) {
    cli::cli_abort(
      c(
        "Invalid {.arg sheet} provided.",
        x = "{.val {sheet}} does not exist in {.file {ods_file}}",
        i = "Use {.fun ods_sheets} to get a list of sheets for this file."
      )
    )
  }

  # get the xpath for the sheet
  tbl_path <- ods_sheets[sheet]

  # find the sheet node
  tbl_node <- xml2::xml_find_all(
    x = ods_xml,
    xpath = tbl_path
  )

  # default is to extract only the table row objects
  if (.rows_only) {
    tbl_node <- xml2::xml_find_all(
      x = tbl_node,
      xpath = "table:table-row"
    )
  }

  return(tbl_node)

}

# turn the table node into a tibble
extract_table <- function(tbl_xml, quick = FALSE, whitespace = FALSE) {

  cli::cli_progress_message("Processing rows...")

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
    row_content = purrr::map(tbl_xml, extract_row, quick = quick, .pb_id = pbid)
  )

  # if row is not repeated set repeat weight to 1
  row_data$row_repeats[is.na(row_data$row_repeats)] <- 1

  # if final row is blank set its repeat to 0
  # MS Excel has a habit of including the final row as blank
  # and a large replication weight
  if (xml2::xml_text(tbl_xml[length(tbl_xml)]) == "") {
    row_data$row_repeats[length(tbl_xml)] <- 0
  }

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
    dplyr::ungroup()

  # create output tibble
  if (quick) {
    ods_cells_out <- ods_cells %>%
      dplyr::select(row, col, cell_content)
  } else {
    table_template <- tibble::tibble(
      "base_row" = integer(),
      "row_repeats" = numeric(),
      "base_col" = integer(),
      "cell_element" = character(),
      "cell_content" = character(),
      "cell_repeats" = numeric(),
      "cell_children" = integer(),
      "office:value-type" = character(),
      "calcext:value-type" = character(),
      "table:style-name" = character(),
      "office:boolean-value" = character(),
      "office:currency" = character(),
      "office:value" = character(),
      "office:date-value" = character(),
      "office:string-value" = character(),
      "office:time-value" = character(),
      "table:formula" = character(),
      "comment" = character(),
      "error" = character(),
      "col_iteration" = integer(),
      "row_iteration" = integer(),
      "row" = integer(),
      "col" = integer()
    )

    ods_cells_out <- table_template %>%
      dplyr::bind_rows(ods_cells) %>%
      dplyr::rename_with(gsub, pattern = ":|-", replacement = "_") %>%
      dplyr::mutate(
        cell_type = dplyr::case_when(
          cell_element == "covered-table-cell" ~ "merged",
          is.na(office_value_type) ~ "empty",
          TRUE ~ "cell"
        ),
        has_formula = !is.na(table_formula),
        base_value = dplyr::case_when(
          is.na(office_value_type) ~ NA_character_,
          office_value_type == "boolean" ~ office_boolean_value,
          office_value_type == "currency" ~ office_value,
          office_value_type == "date" ~ office_date_value,
          office_value_type == "float" ~ office_value,
          office_value_type == "percentage" ~ office_value,
          office_value_type == "string" & !is.na(office_string_value) ~
            office_string_value,
          office_value_type == "string" & is.na(office_string_value) ~ cell_content,
          office_value_type == "time" ~ office_time_value,
          TRUE ~ cell_content
        ),
        numeric_value = as.numeric(office_value),
        logical_value = as.logical(office_boolean_value),
        error = dplyr::case_when(
          cell_content == "#NUM!" ~ "503",
          cell_content == "#VALUE!" ~ "519",
          cell_content == "#NULL!" ~ "521",
          cell_content == "#REF!" ~ "524",
          cell_content == "#NAME?" ~ "525",
          cell_content == "#DIV/0" ~ "532",
          grepl("^Err:\\d{3}$", cell_content) ~ gsub("Err:", "", cell_content),
          TRUE ~ NA_character_
        ),
        error = as.numeric(error)
      ) %>%
      dplyr::select(
        row, col,
        cell_type,
        value_type = office_value_type,
        cell_content,
        base_value,
        numeric_value,
        logical_value,
        currency_symbol = office_currency,
        has_formula,
        cell_formula = table_formula,
        comment,
        error
      )
  }

  return(ods_cells_out)

}

# extract cells from a row
extract_row <- function(row_node, quick = FALSE, whitespace = FALSE, .pb_id = NULL) {

  # get cell nodes
  cell_nodes <- xml2::xml_children(row_node)

  # process cells
  if (quick) {

    row_cells <- tibble::tibble(
      base_col = seq_along(cell_nodes),
      cell_element = xml2::xml_name(cell_nodes),
      cell_content = xml2::xml_text(cell_nodes),
      cell_repeats = as.numeric(
        xml2::xml_attr(cell_nodes, "number-columns-repeated")
      ),
      cell_children = xml2::xml_length(cell_nodes)
    )

  } else {

    row_cells <- tibble::tibble(
      base_col = seq_along(cell_nodes),
      cell_element = xml2::xml_name(cell_nodes),
      cell_content = purrr::map_chr(cell_nodes, get_text, whitespace = whitespace),
      cell_repeats = as.numeric(
        xml2::xml_attr(cell_nodes, "number-columns-repeated")
      ),
      cell_children = xml2::xml_length(cell_nodes),
      comment = purrr::map_chr(cell_nodes, get_comment)
    )

    cell_attrs <- xml2::xml_attrs(cell_nodes, xml2::xml_ns(cell_nodes))

    attrs_df <- tidyr::unnest_wider(tibble::enframe(cell_attrs), value)

    row_cells <- dplyr::full_join(
      row_cells,
      attrs_df,
      by = c("base_col" = "name")
    )

  }

  # update the progress bar
  if (!is.null(.pb_id)){
    cli::cli_progress_update(id = .pb_id)
  }

  return(row_cells)

}

get_text <- function(cell_node, whitespace = FALSE) {

  # get paragraphs
  text_elements <- xml2::xml_find_all(cell_node, "text:p")

  if (whitespace) {

    cell_text <- c()

    for (el in text_elements) {
      el_content <- xml2::xml_contents(el)
      el_name <- xml2::xml_name(el_content)
      el_text <- xml2::xml_text(el_content)
      el_reps <- xml2::xml_attr(el_content, "c")
      el_reps[is.na(el_reps)] <- 1
      el_text[el_name == "s"] <- " "

      cell_text <- c(
        cell_text,
        paste0(rep(el_text, el_reps), collapse = "")
      )
    }

    cell_text <- paste0(cell_text, collapse = "\n")

  } else {
    cell_text <- paste0(xml2::xml_text(text_elements), collapse = "\n")
  }

  return(cell_text)

}

get_comment <- function(cell_node) {

  annotation_node <- xml2::xml_find_all(cell_node, "office:annotation")

  if (length(annotation_node) > 0) {
    comment_text <- get_text(annotation_node, whitespace = TRUE)
  } else {
    comment_text <- NA_character_
  }

  return(comment_text)

}
