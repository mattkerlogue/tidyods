extract_cells_quick <- function(ods_xml, sheet_path, ns = NULL) {

  if (is.null(ns)) {
    ns <- xml2::xml_ns(ods_xml)
  }

  sheet_xml <- sheet_xml_nodes(ods_xml, sheet_path, ns)
  sheet_tbl <- sheet_components(sheet_xml, ns)

  row_tbl <- row_components(sheet_xml, ns)
  cell_tbl <- cell_components_basic(sheet_xml, ns)

  full_tbl <- combine_cells_rows(cell_tbl, row_tbl) |>
    dplyr::left_join(sheet_tbl, by = "sheet_path")

  out_tbl <- full_tbl |>
    dplyr::select(sheet, row, col, value_type = office_value_type, base_value)

  return(out_tbl)

}

extract_cells_full <- function(ods_xml, sheet_path, ns = NULL) {

  if (is.null(ns)) {
    ns <- xml2::xml_ns(ods_xml)
  }

  sheet_xml <- sheet_xml_nodes(ods_xml, sheet_path, ns)
  sheet_tbl <- sheet_components(sheet_xml, ns)

  row_tbl <- row_components(sheet_xml, ns)
  cell_tbl <- cell_components_extended(sheet_xml, ns)

  full_tbl <- combine_cells_rows(cell_tbl, row_tbl) |>
    dplyr::left_join(sheet_tbl, by = "sheet_path")

  out_scaffold <- tidyods_out_scaffold()

  out_tbl0 <- full_tbl |>
    dplyr::mutate(
      cell_type = dplyr::case_when(
        cell_el == "table:covered-table-cell" ~ "merge-hidden",
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
      boolean_value = dplyr::case_when(
        grepl("of:=FALSE()", table_formula) ~ FALSE,
        grepl("of:=TRUE()", table_formula) ~ TRUE,
        TRUE ~ boolean_value
      ),
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
