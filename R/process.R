# Functions for processing ODS XML

# extract components relating to rows
row_components <- function(sheet_xml, ns) {

  row_xml <- xml2::xml_find_all(sheet_xml, "descendant::table:table-row")
  row_tbl <- tibble::tibble(
    row_path = xml2::xml_path(row_xml),
    row_repeats = suppressWarnings(as.numeric(xml2::xml_attr(row_xml, "table:number-rows-repeated", ns))),
    row_style = xml2::xml_attr(row_xml, "table:style-name", ns)
  )

  return(row_tbl)

}

# extract components relating to columns
col_components <- function(sheet, ns) {

  col_xml <- xml2::xml_find_all(sheet_xml, "descendant::table:table-column")
  col_tbl <- tibble::tibble(
    col_path = xml2::xml_path(row_xml),
    col_style = xml2::xml_attr(row_xml, "table:style-name", ns)
  )

  return(row_tbl)

}

# extract minimal information about ODS cells
cell_components_basic <- function(sheet_xml, ns, quick) {

  cell_xml <- xml2::xml_find_all(
    sheet_xml,
    "descendant::table:table-cell | descendant::table:covered-table-cell"
  )

  cell_paths <- xml2::xml_path(cell_xml)

  cell_tbl <- tibble::tibble(
    cell_path = cell_paths,
    row_path = gsub("(.*table:table-row(\\[\\d+\\])?).*", "\\1", cell_paths),
    sheet_path = gsub("(.*table:table(\\[\\d+\\])?)\\/.*", "\\1", cell_paths),
    col_repeats = suppressWarnings(as.numeric(xml2::xml_attr(cell_xml, "table:number-columns-repeated", ns))),
    base_row = suppressWarnings(as.numeric(gsub(".*table:table-row\\[(\\d+)\\].*", "\\1", cell_paths))),
    base_col = suppressWarnings(as.numeric(gsub(".*table:table-cell\\[(\\d+)\\].*", "\\1", cell_paths))),
    office_value_type = xml2::xml_attr(cell_xml, "office:value-type", ns),
    cell_content = xml2::xml_text(cell_xml),
    cell_numeric = suppressWarnings(as.numeric(xml2::xml_attr(cell_xml, "office:value", ns)))
  ) |>
    dplyr::mutate(
      base_value = dplyr::case_when(
        is.na(office_value_type) ~ NA_character_,
        cell_content == "" ~ NA_character_,
        office_value_type == "currency" ~ cell_content,
        !is.na(cell_numeric) ~ as.character(cell_numeric),
        TRUE ~ cell_content
      )
    )

  return(cell_tbl)

}

# extract full cell information
cell_components_extended <- function(sheet_xml, ns) {

  cell_xml <- xml2::xml_find_all(
    sheet_xml,
    "descendant::table:table-cell | descendant::table:covered-table-cell"
  )

  cell_paths <- xml2::xml_path(cell_xml)

  cell_attrs <- xml2::xml_attrs(cell_xml, ns)

  cell_content_tbl <- extract_text(cell_xml, ns)

  cell_attrs_tbl <- extract_attributes(cell_xml, ns)

  cell_annotations_tbl <- extract_annotations(cell_xml, ns)

  cell_tbl_0 <- tibble::tibble(
    cell_path = cell_paths,
    row_path = gsub("(.*table:table-row(\\[\\d+\\])?).*", "\\1", cell_paths),
    sheet_path = gsub("(.*table:table(\\[\\d+\\])?)\\/.*", "\\1", cell_paths),
    base_row = suppressWarnings(
      as.numeric(gsub(".*table:table-row\\[(\\d+)\\].*", "\\1", cell_paths))
    ),
    base_col = suppressWarnings(
      as.numeric(gsub(".*table:table-cell\\[(\\d+)\\].*", "\\1", cell_paths))
    ),
    cell_el = xml2::xml_name(cell_xml),
  ) |>
    dplyr::left_join(
      cell_content_tbl, by = "cell_path"
    ) |>
    dplyr::left_join(
      cell_attrs_tbl, by = "cell_path"
    ) |>
    dplyr::left_join(
      cell_annotations_tbl, by = "cell_path"
    )

  cell_tbl <- tidyods_cell_scaffold() |>
    dplyr::bind_rows(cell_tbl_0) |>
    dplyr::mutate(
      col_repeats = suppressWarnings(as.numeric(table_number_columns_repeated)),
      .after = base_col
    ) |>
    dplyr::select(-table_number_columns_repeated)

  return(cell_tbl)

}

extract_attributes <- function(cell_xml, ns) {

  cell_attrs <- xml2::xml_attrs(cell_xml, ns)

  cell_attrs_tbl <- tibble::tibble(
    cell_path = xml2::xml_path(cell_xml),
    cell_attrs = cell_attrs
  ) |>
    tidyr::unnest_wider(cell_attrs)

  names(cell_attrs_tbl) <- gsub("-|:", "_", names(cell_attrs_tbl))

  return(cell_attrs_tbl)

}

extract_annotations <- function(cell_xml, ns) {

  annotation_xml <- xml2::xml_find_all(
    cell_xml,
    "office:annotation/text:p"
  )

  if (length(annotation_xml) == 0) {

    annotation_tbl <- tibble::tibble(
      cell_path = character(),
      cell_annotation = character()
    )

    return(annotation_tbl)

  }

  annotation_fragments <- xml2::xml_contents(annotation_xml)

  annotation_tbl <- tibble::tibble(
    id = seq_len(length(annotation_fragments)),
    xml_path = xml2::xml_path(annotation_fragments),
    el = xml2::xml_name(annotation_fragments, ns),
    text = xml2::xml_text(annotation_fragments),
    rep = suppressWarnings(
      as.numeric(xml2::xml_attr(annotation_fragments, "text:c", ns))
      )
  ) |>
    dplyr::mutate(
      rep = tidyr::replace_na(rep, 1),
      text = dplyr::if_else(
        el == "text:s",
        paste0(rep(" ", 4), collapse = ""),
        text
      ),
      text_path = gsub("(.*office:annotation/text:p(\\[\\d+\\])?).*", "\\1", xml_path)
    ) |>
    dplyr::arrange(text_path, id) |>
    dplyr::reframe(
      cell_annotation = paste0(text, collapse = ""), .by = "text_path"
    ) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      cell_path = gsub("(.*table-cell(\\[\\d+\\])?).*", "\\1", text_path)
    ) |>
    dplyr::reframe(
      cell_annotation = paste0(cell_annotation, collapse = "\n"), .by = "cell_path"
    )

  return(annotation_tbl)

}

extract_text <- function(cell_xml, ns) {

  cell_children <- xml2::xml_length(cell_xml)

  # shortcut if cells have singular content
  if (sum(cell_children > 1) == 0) {

    text_tbl <- tibble::tibble(
      cell_path = xml2::xml_path(cell_xml),
      cell_content = xml2::xml_text(cell_xml)
    )

    return(text_tbl)

  }

  cell_components <- xml2::xml_contents(cell_xml)
  component_el <- xml2::xml_name(cell_components, ns)
  text_components <- cell_components[component_el == "text:p"]

  text_fragments <- xml2::xml_contents(text_components)

  text_tbl <- tibble::tibble(
    id = seq_len(length(text_fragments)),
    xml_path = xml2::xml_path(text_fragments),
    el = xml2::xml_name(text_fragments, ns),
    text = xml2::xml_text(text_fragments),
    rep = suppressWarnings(
      as.numeric(xml2::xml_attr(text_fragments, "text:c", ns))
    )
  ) |>
    dplyr::mutate(
      rep = tidyr::replace_na(rep, 1),
      text = dplyr::if_else(
        el == "text:s",
        paste0(rep(" ", 4), collapse = ""),
        text
      ),
      text_path = gsub("(.*table-cell(\\[\\d+\\])?\\/text:p(\\[\\d+\\])?).*", "\\1", xml_path)
    ) |>
    dplyr::arrange(text_path, id) |>
    dplyr::reframe(
      cell_content = paste0(text, collapse = ""), .by = "text_path"
    ) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      cell_path = gsub("(.*table-cell(\\[\\d+\\])?).*", "\\1", text_path)
    ) |>
    dplyr::reframe(
      cell_content = paste0(cell_content, collapse = "\n"), .by = "cell_path"
    )

  return(text_tbl)

}


# merge cell and row components, replicate repeated rows/cols
combine_cells_rows <- function(cell_tbl, row_tbl) {

  full_tbl <- cell_tbl |>
    dplyr::left_join(
      row_tbl, by = "row_path"
    ) |>
    dplyr::mutate(
      across(c(base_row, base_col, col_repeats, row_repeats), ~tidyr::replace_na(.x, 1)),
      keep = dplyr::case_when(
        is.na(office_value_type) ~ FALSE,
        is.na(cell_content) & base_col == max(base_col) & col_repeats > 1 ~ FALSE,
        is.na(cell_content) & base_row == max(base_row) & col_repeats > 1 ~ FALSE,
        TRUE ~ TRUE
      )
    ) |>
    dplyr::filter(
      keep
    ) |>
    tidyr::uncount(row_repeats, .remove = FALSE, .id = "row_iteration") |>
    tidyr::uncount(col_repeats, .remove = FALSE, .id = "col_iteration") |>
    dplyr::arrange(sheet_path, base_row, row_repeats, base_col, col_repeats) |>
    dplyr::group_by(sheet_path, base_row, row_repeats, .add = FALSE) |>
    dplyr::mutate(row = dplyr::cur_group_id(),
                  col = dplyr::row_number(),
                  .before = 1L) |>
    dplyr::ungroup()

  return(full_tbl)

}

# extract basic cell info
extract_cells_quick <- function(ods_xml, sheet_path, ns = NULL) {

  if (is.null(ns)) {
    ns <- xml2::xml_ns(ods_xml)
  }

  if (is.na(sheet_path)) {
    sheet_xml <- ods_xml
    sheet_paths <- ods_sheet_paths(ods_xml, ns)
    sheet_tbl <- tibble::tibble(
      sheet = names(sheet_paths),
      sheet_path = sheet_paths
    )
  } else {
    sheet_xml <- ods_xml |>
      xml2::xml_find_first(sheet_path)
    sheet_name <- xml2::xml_attr(sheet_xml, "table:name", ns)
  }

  row_tbl <- row_components(sheet_xml, ns)
  cell_tbl <- cell_components_basic(sheet_xml, ns)

  if (is.na(sheet_path)) {
    full_tbl <- combine_cells_rows(cell_tbl, row_tbl) |>
      dplyr::left_join(sheet_tbl, by = "sheet_path")
  } else {
    full_tbl <- combine_cells_rows(cell_tbl, row_tbl) |>
      dplyr::mutate(sheet = sheet_name)
  }

  out_tbl <- full_tbl |>
    dplyr::select(sheet, row, col, value_type = office_value_type, base_value)

  return(out_tbl)

}

extract_cells_full <- function(ods_xml, sheet_path, ns = NULL) {

  if (is.null(ns)) {
    ns <- xml2::xml_ns(ods_xml)
  }

  if (is.na(sheet_path)) {
    sheet_xml <- ods_xml
    sheet_paths <- ods_sheet_paths(ods_xml, ns)
    sheet_tbl <- tibble::tibble(
      sheet = names(sheet_paths),
      sheet_path = sheet_paths
    )
  } else {
    sheet_xml <- ods_xml |>
      xml2::xml_find_first(sheet_path)
    sheet_name <- xml2::xml_attr(sheet_xml, "table:name", ns)
  }

  row_tbl <- row_components(sheet_xml, ns)
  cell_tbl <- cell_components_extended(sheet_xml, ns)

  if (is.na(sheet_path)) {
    full_tbl <- combine_cells_rows(cell_tbl, row_tbl) |>
      dplyr::left_join(sheet_tbl, by = "sheet_path")
  } else {
    full_tbl <- combine_cells_rows(cell_tbl, row_tbl) |>
      dplyr::mutate(sheet = sheet_name)
  }

  out_scaffold <- tidyods_out_scaffold()

  out_tbl0 <- full_tbl |>
    dplyr::mutate(
      cell_type = dplyr::case_when(
        cell_el == "covered-table-cell" ~ "merge-hidden",
        !is.na(table_number_columns_spanned) | !is.na(table_number_rows_spanned) ~
          "merge-lead",
        is.na(office_value_type) ~ "empty",
        is.na(cell_content) ~ "empty",
        cell_content == "" ~ "empty",
        TRUE ~ "cell"
      ),
      is_empty = cell_type == "empty",
      is_merged = grepl("merge", cell_type),
      has_formula = !is.na(table_formula),
      error_type = dplyr::case_when(
        cell_content == "#NULL!" ~ 1L,
        cell_content == "#DIV/0!" ~ 2L,
        cell_content == "#VALUE!" ~ 3L,
        cell_content == "#REF!" ~ 4L,
        cell_content == "#NAME?" ~ 5L,
        cell_content == "#NUM!" ~ 6L,
        cell_content == "#N/A" ~ 7L,
        grepl("^Err:\\d{3}$", cell_content) ~ 7L,
        TRUE ~ NA_integer_
      ),
      has_error = !is.na(error_type),
      base_value = dplyr::case_when(
        is_empty ~ NA_character_,
        has_error ~ cell_content,
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
      numeric_value = suppressWarnings(as.numeric(office_value)),
      boolean_value = suppressWarnings(as.logical(office_boolean_value)),
      has_annotation = !is.na(cell_annotation),
      merge_rowspan = suppressWarnings(as.numeric(table_number_rows_spanned)),
      merge_colspan = suppressWarnings(as.numeric(table_number_columns_spanned)),
      merge_shape = dplyr::case_when(
        merge_colspan == 1 & merge_rowspan > 1 ~ "vertical",
        merge_rowspan == 1 & merge_colspan > 1 ~ "horizontal",
        merge_rowspan > 1 & merge_colspan > 1  ~ "rectangle",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::rename(
      value_type = office_value_type,
      date_value = office_date_value,
      time_value = office_time_value,
      currency_symbol = office_currency,
      formula = table_formula,
      annotation = cell_annotation,
      cell_style = table_style_name
    ) |>
    dplyr::select(
      tidyselect::any_of(names(out_scaffold))
    )

  out_tbl <- out_scaffold |>
    dplyr::bind_rows(out_tbl0)

  return(out_tbl)

}


## scaffolds for tbl construction

# cell components scaffold
tidyods_cell_scaffold <- function() {
  tibble::tibble(
    cell_path = character(),
    row_path = character(),
    sheet_path = character(),
    base_row = numeric(),
    base_col = numeric(),
    cell_el = character(),
    cell_content = character(),
    office_value_type = character(),
    calcext_value_type = character(),
    table_style_name = character(),
    office_boolean_value = character(),
    office_currency = character(),
    office_value = character(),
    office_date_value = character(),
    office_time_value = character(),
    office_string_value = character(),
    table_formula = character(),
    table_number_columns_repeated = character(),
    table_number_matrix_columns_spanned = character(),
    table_number_matrix_rows_spanned = character(),
    table_number_columns_spanned = character(),
    table_number_rows_spanned = character(),
    cell_annotation = character()
  )
}

# template for output
tidyods_out_scaffold <- function() {
  tibble::tibble(
    sheet = character(),
    row = numeric(),
    col = numeric(),
    cell_type = character(),
    is_empty = logical(),
    value_type = character(),
    cell_content = character(),
    base_value = character(),
    numeric_value = numeric(),
    currency_symbol = character(),
    boolean_value = logical(),
    date_value = character(),
    time_value = character(),
    has_formula = logical(),
    formula = character(),
    has_error = logical(),
    error_type = numeric(),
    has_annotation = logical(),
    annotation = character(),
    is_merged = logical(),
    merge_colspan = numeric(),
    merge_rowspan = numeric(),
    merge_shape = character(),
    cell_style = character(),
    row_style = character()
  )
}
