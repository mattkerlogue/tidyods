extract_cells_quick <- function(ods_xml, sheet_path, ns = NULL) {

  if (is.null(ns)) {
    ns <- xml2::xml_ns(ods_xml)
  }

  sheet_xml <- sheet_xml_nodes(ods_xml, sheet_path, ns)
  sheet_tbl <- sheet_components(sheet_xml, ns)

  row_tbl <- row_components(sheet_xml, ns)
  cell_tbl <- cell_components_basic(sheet_xml, ns)

  full_tbl <- combine_components(cell_tbl, row_tbl) |>
    dplyr::left_join(sheet_tbl, by = "sheet_path")

  out_tbl <- full_tbl |>
    dplyr::select(
      .data$sheet, .data$address, .data$row, .data$col,
      value_type = .data$office_value_type, .data$base_value
  )

  return(out_tbl)

}

extract_cells_full <- function(ods_xml, sheet_path, ns = NULL) {

  if (is.null(ns)) {
    ns <- xml2::xml_ns(ods_xml)
  }

  sheet_xml <- sheet_xml_nodes(ods_xml, sheet_path, ns)
  sheet_tbl <- sheet_components(sheet_xml, ns)

  row_tbl <- row_components(sheet_xml, ns)
  col_tbl <- col_components(sheet_xml, ns)
  cell_tbl <- cell_components_extended(sheet_xml, ns)

  full_tbl <- combine_components(cell_tbl, row_tbl, col_tbl) |>
    dplyr::left_join(sheet_tbl, by = "sheet_path")

  out_scaffold <- tidyods_out_scaffold()

  out_tbl0 <- full_tbl |>
    dplyr::mutate(
      cell_type = dplyr::case_when(
        .data$cell_el == "table:covered-table-cell" ~ "merge-hidden",
        !is.na(.data$table_number_columns_spanned) |
          !is.na(.data$table_number_rows_spanned) ~ "merge-lead",
        is.na(.data$office_value_type) ~ "empty",
        is.na(.data$cell_content) ~ "empty",
        .data$cell_content == "" ~ "empty",
        TRUE ~ "cell"
      ),
      is_empty = .data$cell_type == "empty",
      is_merged = grepl("merge", .data$cell_type),
      has_formula = !is.na(.data$table_formula),
      error_type = dplyr::case_when(
        .data$cell_content == "#NULL!" ~ 1L,
        .data$cell_content == "Err:511" ~ 1L,
        .data$cell_content == "#DIV/0!" ~ 2L,
        .data$cell_content == "#VALUE!" ~ 3L,
        .data$cell_content == "#REF!" ~ 4L,
        .data$cell_content == "#NAME?" ~ 5L,
        .data$cell_content == "#NUM!" ~ 6L,
        .data$cell_content == "#N/A" ~ 7L,
        grepl("^Err:\\d{3}$", .data$cell_content) ~
          suppressWarnings(as.numeric(gsub("Err:", "", .data$cell_content))),
        TRUE ~ NA_integer_
      ),
      has_error = !is.na(.data$error_type),
      base_value = dplyr::case_when(
        .data$is_empty ~ NA_character_,
        .data$has_error ~ .data$cell_content,
        .data$office_value_type == "boolean" ~ .data$office_boolean_value,
        .data$office_value_type == "currency" ~ .data$office_value,
        .data$office_value_type == "date" ~ .data$office_date_value,
        .data$office_value_type == "float" ~ .data$office_value,
        .data$office_value_type == "percentage" ~ .data$office_value,
        .data$office_value_type == "string" & !
          is.na(.data$office_string_value) ~ .data$office_string_value,
        .data$office_value_type == "string" &
          is.na(.data$office_string_value) ~ .data$cell_content,
        .data$office_value_type == "time" ~ .data$office_time_value,
        TRUE ~ .data$cell_content
      ),
      numeric_value = suppressWarnings(as.numeric(.data$office_value)),
      boolean_value = suppressWarnings(as.logical(.data$office_boolean_value)),
      boolean_value = dplyr::case_when(
        grepl("of:=FALSE()", .data$table_formula) ~ FALSE,
        grepl("of:=TRUE()", .data$table_formula) ~ TRUE,
        TRUE ~ .data$boolean_value
      ),
      has_annotation = !is.na(.data$cell_annotation),
      merge_rowspan = suppressWarnings(
        as.numeric(.data$table_number_rows_spanned)
        ),
      merge_colspan = suppressWarnings(
        as.numeric(.data$table_number_columns_spanned)
      ),
      merge_shape = dplyr::case_when(
        .data$merge_colspan == 1 & .data$merge_rowspan > 1 ~ "vertical",
        .data$merge_rowspan == 1 & .data$merge_colspan > 1 ~ "horizontal",
        .data$merge_rowspan > 1  & .data$merge_colspan > 1  ~ "rectangular",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::rename(
      value_type = .data$office_value_type,
      date_value = .data$office_date_value,
      time_value = .data$office_time_value,
      currency_symbol = .data$office_currency,
      formula = .data$table_formula,
      annotation = .data$cell_annotation,
      cell_style = .data$table_style_name
    ) |>
    dplyr::select(
      tidyselect::any_of(names(out_scaffold))
    )

  out_tbl <- out_scaffold |>
    dplyr::bind_rows(out_tbl0)

  return(out_tbl)

}
