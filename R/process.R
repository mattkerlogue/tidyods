# Internal functions for processing ODS files

# unzip and ods file and get content xml
unzip_ods_xml <- function(ods_file) {

  temp_dir <- tempdir()

  # if remote file location download the file to temp_dir
  if (grepl("^((http|ftp)s?|sftp)://", ods_file)) {
    cli::cli_progress_step("Reading remote file")
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

  return(xml2::read_xml(xml_file))

}

# get xpaths of sheets within the XML
ods_sheet_paths <- function(ods_xml, short = FALSE, flip = FALSE) {

  # get nodes
  tbl_nodes <- xml2::xml_find_all(
    x = ods_xml,
    xpath = "//table:table"
  )

  # get xpaths and name with sheet name
  tbl_paths <- xml2::xml_path(tbl_nodes)

  tbl_names <- xml2::xml_attr(
    x = tbl_nodes,
    attr = "table:name",
    ns = xml2::xml_ns(tbl_nodes)
  )

  if (short) {
    tbl_paths <- gsub("^.*(table:table\\[\\d+\\]?)\\.*$", "\\1", tbl_paths)
  }

  if (flip) {
    out <- stats::setNames(tbl_names, tbl_paths)
  } else {
    out <- stats::setNames(tbl_paths, tbl_names)
  }

  return(out)

}

extract_cells <- function(tbl_xml, ns, quick = FALSE) {

  cell_nodes <- xml2::xml_find_all(
    tbl_xml,
    "descendant::table:table-cell | descendant::table:covered-table-cell"
  )

  cell_paths <- xml2::xml_path(cell_nodes)

  cell_ref <- tibble::tibble(
    cell_id = seq_along(cell_paths),
    cell_path = cell_paths,
    row_path = gsub("/table:table-cell.*$|/table:covered-table-cell.*$",
                    "", cell_paths),
    cell_el = xml2::xml_name(cell_nodes, ns)
  )

  cell_text_nodes <- xml2::xml_find_all(
    tbl_xml,
    "descendant::table:table-cell/text:p | descendant::table:covered-table-cell/text:p"
    )

  cell_text <- tibble::tibble(
    content_text = xml2::xml_text(cell_text_nodes),
    content_paths = xml2::xml_path(cell_text_nodes),
    cell_path = gsub("/text:p.*$", "", content_paths)
  ) %>%
    dplyr::group_by(cell_path) %>%
    dplyr::arrange(content_paths) %>%
    dplyr::summarise(cell_content = paste0(content_text, collapse = "\n"))

  if (!quick) {

    cell_attrs <- xml2::xml_attrs(cell_nodes, ns = ns)
    cell_attrs_df <- tidyr::unnest_wider(
      tibble::enframe(cell_attrs, name = "cell_id"),
      value
    ) %>%
      dplyr::rename_with(gsub, pattern = ":|-", replacement = "_")

    if (!("table_number_columns_repeated" %in% names(cell_attrs_df))) {
      cell_attrs_df <- dplyr::mutate(cell_attrs_df,
                                    table_number_columns_repeated = NA_character_)
    }

  } else {

    cell_attrs_df <- tibble::tibble(
      cell_id = seq_along(cell_paths),
      office_value_type = xml2::xml_attr(
        cell_nodes, "office:value-type", ns = ns
      ),
      table_number_columns_repeated = xml2::xml_attr(
        cell_nodes, "table:number-columns-repeated", ns = ns
      )
    )

  }

  cell_tbl <- cell_ref %>%
    dplyr::full_join(cell_text, by = c("cell_path")) %>%
    dplyr::left_join(cell_attrs_df, by = c("cell_id"))

  return(cell_tbl)

}

extract_rows <- function(tbl_xml, ns, quick = FALSE) {

  row_nodes <- xml2::xml_find_all(tbl_xml, "descendant::table:table-row")
  row_paths <- xml2::xml_path(row_nodes)

  if (!quick) {
    row_attrs <- xml2::xml_attrs(row_nodes, ns = ns)

    row_attrs_df <- tidyr::unnest_wider(
      tibble::enframe(row_attrs, name = "row_id"),
      value
    ) %>%
      dplyr::rename_with(gsub, pattern = ":|-", replacement = "_")

    if (!("table_number_rows_repeated" %in% names(row_attrs_df))) {
      row_attrs_df <- dplyr::mutate(row_attrs_df,
                                    table_number_rows_repeated = NA_character_)
    }

  } else {

    row_attrs_df <- tibble::tibble(
      row_id = seq_along(row_nodes),
      table_number_rows_repeated = xml2::xml_attr(
        row_nodes, "table:number-rows-repeated", ns = ns
      )
    )

  }

  row_tbl <- tibble::tibble(
    row_id = seq_along(row_nodes),
    row_path = row_paths,
    row_children = xml2::xml_length(row_nodes)
  ) %>%
    dplyr::left_join(row_attrs_df, by = "row_id")

  return(row_tbl)

}

generate_cell_output <- function(row_tbl, cell_tbl, sheets, quick = FALSE) {

  combined_tbl <- dplyr::left_join(
    cell_tbl, row_tbl,
    by = c("row_path"),
    suffix = c("_cell", "_row")
  )

  sheets <- purrr::set_names(x = names(sheets), nm = sheets)

  cell_locations <- combined_tbl %>%
    dplyr::select(
      row_id, cell_id, table_number_rows_repeated, table_number_columns_repeated,
      row_children, cell_content, office_value_type, cell_path
    ) %>%
    dplyr::mutate(
      sheet = gsub("^(.*table:table\\[\\d+\\]?)\\/.*$", "\\1", cell_path),
      sheet = sheets[sheet]
    ) %>%
    dplyr::arrange(sheet, row_id, cell_id) %>%
    dplyr::group_by(sheet, row_id) %>%
    dplyr::mutate(
      base_col = dplyr::row_number(),
      last_col = base_col == max(base_col)
    ) %>%
    dplyr::group_by(sheet, .add = FALSE) %>%
    dplyr::mutate(
      base_row = row_id - min(row_id) + 1,
      last_row = row_id == max(row_id)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      col_repeats = dplyr::case_when(
        last_col & !is.na(table_number_columns_repeated) &
          is.na(office_value_type) & is.na(cell_content) ~ 0,
        is.na(table_number_columns_repeated) ~ 1,
        TRUE ~ as.numeric(table_number_columns_repeated)
      ),
      row_repeats = dplyr::case_when(
        last_row & row_children == 1 & !is.na(table_number_rows_repeated) &
          is.na(office_value_type) & is.na(cell_content) ~ 0,
        is.na(table_number_rows_repeated) ~ 1,
        TRUE ~ as.numeric(table_number_rows_repeated)
      )
    ) %>%
    tidyr::uncount(row_repeats, .remove = FALSE, .id = "row_iteration") %>%
    tidyr::uncount(col_repeats, .remove = FALSE, .id = "col_iteration") %>%
    dplyr::arrange(sheet, base_row, row_iteration, base_col, col_iteration) %>%
    dplyr::group_by(base_row, row_iteration, .add = FALSE) %>%
    dplyr::mutate(row = dplyr::cur_group_id(),
                  col = dplyr::row_number()) %>%
    dplyr::ungroup()

  if (quick) {

    ods_cells <- cell_locations %>%
      dplyr::select(sheet, row, col, cell_content)

  } else {

    template_table <- tibble::tibble(
      sheet = character(),
      row = integer(),
      col = integer(),
      cell_id = integer(),
      cell_path = character(),
      row_id = integer(),
      row_path = character(),
      row_children = integer(),
      table_style_name_row = character(),
      table_number_rows_repeated = character(),
      table_number_columns_repeated = character(),
      cell_el = character(),
      cell_content = character(),
      office_value_type = character(),
      calcext_value_type = character(),
      table_style_name_cell = character(),
      office_boolean_value = character(),
      office_currency = character(),
      office_value = character(),
      office_date_value = character(),
      office_time_value = character(),
      table_formula = character(),
      office_string_value = character(),
      table_number_columns_spanned = character(),
      table_number_rows_spanned = character()
    )

    ods_cells <-  cell_locations %>%
      dplyr::select(sheet, row, col, cell_id) %>%
      dplyr::left_join(combined_tbl, by = "cell_id") %>%
      dplyr::bind_rows(template_table) %>%
      dplyr::mutate(
        cell_type = dplyr::case_when(
          cell_el == "table:covered-table-cell" ~ "merged",
          is.na(office_value_type) ~ "blank",
          is.na(cell_content) ~ "blank",
          TRUE ~ "cell"
        ),
        is_empty = is.na(cell_content),
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
        error_type = dplyr::case_when(
          cell_content == "#NULL!" ~ "1",
          cell_content == "#DIV/0!" ~ "2",
          cell_content == "#VALUE!" ~ "3",
          cell_content == "#REF!" ~ "4",
          cell_content == "#NAME?" ~ "5",
          cell_content == "#NUM!" ~ "6",
          cell_content == "#N/A" ~ "7",
          grepl("^Err:\\d{3}$", cell_content) ~ "7",
          TRUE ~ NA_character_
        ),
        error_type = as.numeric(error_type),
        has_error = !is.na(error_type),
        base_value = dplyr::if_else(has_error, cell_content, base_value)
      ) %>%
      dplyr::select(
        sheet, row, col,
        cell_type,
        value_type = office_value_type,
        is_empty,
        cell_content,
        base_value,
        numeric_value,
        logical_value,
        currency_symbol = office_currency,
        has_formula,
        cell_formula = table_formula,
        has_error,
        error_type
      )

  }

  return(ods_cells)

}
