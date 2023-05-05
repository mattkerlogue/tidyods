## scaffolds for df construction

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
    table_number_columns_repeated = numeric(),
    table_number_matrix_columns_spanned = character(),
    table_number_matrix_rows_spanned = character(),
    table_number_columns_spanned = character(),
    table_number_rows_spanned = character(),
    cell_annotation = character()
  )
}

# output scaffold
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
