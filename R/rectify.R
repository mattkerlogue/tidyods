#' Convert ODS cells to a two-dimensional dataset
#'
#' There are two functions to "rectify" a set of ODS cells extracted by
#' [read_ods_cells()] back into a traditional two-dimensional spreadsheet
#' rectangle.
#'
#' @param ods_cells A set of cells from `read_ods_cells()`.
#' @param skip The number of rows to skip before extracting the sheet.
#' @param col_headers Whether to use the first row (after any skipping) as the
#'   column header (`TRUE` is the default), alternatively a character vector of
#'   column names can be provided.
#' @param values_from The column from `ods_cells` to output values from.
#'
#' @details
#' `simple_rectify()` will perform a basic reshaping of the dataset, by default
#' it will use the `base_value` extracted by `read_ods_cells()` but this can
#' be changed to a different column by setting the `values_from` argument. The
#' rectifier will also by default try to use the first row for column headers,
#' alternatively you can provide your own column names or set
#' `col_headers = FALSE` to get generic column names of `x1`, `x2`, etc.
#'
#' `smart_rectify()` performs a more complex reshaping of the dataset, by
#' guessing the location of column headers and using the `value_type`
#' information generated by [read_ods_cells()] to determine whether columns can
#' be coerced to a non-string data type (either numeric, logical, date or time).
#'
#'
#' @return
#' A tibble representing the original spreadsheet format.
#'
#' @examples
#' example <- system.file("extdata", "basic_example.ods", package = "tidyods")
#' example_cells <- read_ods_cells(example, 1)
#' simple_rectify(example_cells)
#' smart_rectify(example_cells)
#' @export
#' @rdname rectify
simple_rectify <- function(ods_cells, skip = 0, col_headers = TRUE,
                           values_from = "base_value") {

  if (length(unique(ods_cells$sheet)) > 1) {
    cli::cli_abort(c(x = "More than one sheet is present in {.arg ods_cells}"))
  }

  distinct_rc <- dplyr::distinct(ods_cells, row, col)

  if (nrow(distinct_rc) != nrow(ods_cells)) {
    cli::cli_abort(c(x = "There are duplicate rows/columns in {.arg ods_cells}"))
  }

  if (!(values_from %in% names(ods_cells))) {
    cli::cli_abort(c(
      x = "{.val {values_from}} does not exist in {.arg ods_cells}"
    ))
  }

  rect_cells <- quick_rectify(ods_cells, values_from)

  if (skip > 0) {
    rect_cells <- rect_cells[skip:nrow(rect_cells), ]
  }

  input_headers <- character()

  if (is.character(col_headers)) {
    input_headers <- col_headers
    col_headers <- TRUE
  }

  if (col_headers) {

    header_problem <- FALSE
    header_problem_msg <- character()

    if (length(input_headers) > 0) {

      if (length(input_headers) != ncol(rect_cells)) {
        header_problem <- TRUE
        header_problem_msg <- c(
          header_problem_msg,
          "Number of headers provided does not match number of columns"
        )
      }

      headers <- input_headers

    } else {

      header_row <- skip + 1
      headers <- ods_cells[["base_value"]][ods_cells[["row"]] == header_row]

    }

    if (sum(is.na(headers)) > 0) {
      header_problem <- TRUE
      header_problem_msg <- c(header_problem_msg,
                              "Column headers contain missing values")
    }

    if (sum(grepl("[^A-z0-9._ -]", headers)) > 0) {
      header_problem <- TRUE
      header_problem_msg <- c(header_problem_msg,
                              "Invalid characters detected in column headers")
    }

    headers <- gsub("[ -]", "_", headers)

    if (length(unique(headers)) != length(headers)) {
      header_problem <- TRUE
      header_problem_msg <- c(header_problem_msg,
                              "Duplicate column headers")
    }

    if (sum(grepl("^[^A-z]", headers)) > 0) {
      header_problem <- TRUE
      header_problem_msg <- c(header_problem_msg,
                              "Column headers can only start with a letter")
    }

    if (header_problem) {
      cli::cli({
        cli::cli_alert_warning("Issues detected with column headers")
        cli::cli_ul(header_problem_msg)
        cli::cli_alert_info(paste(
          "Cells have been rectified with default column headers",
          "(e.g. {.val {c(\"x1\", \"x2\")}})"
        ))
        cli::cli_alert_info(paste(
          "Using {.fun janitor::row_to_names} followed by",
          "{.fun janitor::clean_names} might resolve this"
        ))
      })
      return(rect_cells)
    }

    if (length(input_headers) == 0) {
      rect_cells <- rect_cells[2:nrow(rect_cells), ]
    }

    names(rect_cells) <- headers

  }

  return(rect_cells)

}

quick_rectify <- function(ods_cells, values_from = "base_value") {

  tidyr::crossing(
    row = seq_len(max(ods_cells$row)),
    col = seq_len(max(ods_cells$col)),
  ) |>
    dplyr::left_join(ods_cells, by = c("row", "col")) |>
    dplyr::mutate(col = paste0("x", col)) |>
    dplyr::select(.data$row, .data$col, {{values_from }}) |>
    tidyr::pivot_wider(names_from = col,
                       values_from = {{ values_from }}) |>
    dplyr::select(-.data$row)

}


#' @export
#' @rdname rectify
smart_rectify <- function(ods_cells) {

  guess_header <- ods_cells |>
    dplyr::group_by(row) |>
    dplyr::summarise(empty = sum(is_empty)) |>
    dplyr::filter(empty == min(empty)) |>
    dplyr::filter(row == min(row))

  header_full <- guess_header$empty == 0
  header_early <- guess_header$row < 10

  if (header_full & header_early) {
    header_row <- guess_header$row
  } else {
    cli::cli_abort(
      c(
        x = "Unable to guess header row",
        i = "Use {.fun simple_rectify} to coerce {.arg ods_cells} to a sheet"
      )
    )
  }

  col_value_types <- ods_cells |>
    dplyr::filter(row > header_row) |>
    dplyr::mutate(
      value_type = dplyr::if_else(
        value_type == "percentage" | value_type == "currency",
        "float",
        value_type)
    ) |>
    dplyr::group_by(col) |>
    dplyr::count(value_type) |>
    dplyr::mutate(prop = n/sum(n)) |>
    tidyr::drop_na(value_type)

  col_string_info <- ods_cells |>
    dplyr::filter(row > header_row & value_type == "string") |>
    dplyr::group_by(col) |>
    dplyr::summarise(
      string_vals = dplyr::n(),
      unique_strings = length(unique(cell_content)),
      shorthand = sum(cell_content %in% shorthand),
      na_vals = sum(cell_content %in% na_values),
      errors = sum(!is.na(error_type)),
      .groups = "keep"
    ) |>
    dplyr::mutate(
      strings_all_markers = sum(shorthand, na_vals, errors) == string_vals,
      strings_all_errors = errors == string_vals
    ) |>
    dplyr::ungroup()

  drop_string <- col_string_info |>
    dplyr::filter(strings_all_markers & !strings_all_errors) |>
    dplyr::pull(col)

  dropped_values <- ods_cells |>
    dplyr::filter(col %in% drop_string &
                    value_type == "string" &
                    row > header_row) |>
    dplyr::count(col, cell_content)

  col_mutlitype <- col_value_types |>
    dplyr::filter(!(value_type == "string" & col %in% drop_string)) |>
    dplyr::count(col) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(col)

  col_type_out <- col_value_types |>
    dplyr::filter(!(value_type == "string" & col %in% drop_string))

  ods_sheet <- simple_rectify(ods_cells, skip = header_row - 1)

  for (i in seq_along(names(ods_sheet))) {

    if (i %in% col_mutlitype) {
      col_type <- "string"
    } else {
      col_type <- col_type_out$value_type[col_type_out$col == i]
    }

    if (length(col_type) == 0) {
      col_type <- "string"
    }

    suppressWarnings(if (col_type == "boolean") {
      ods_sheet[[i]] <- readr::parse_logical(ods_sheet[[i]])
    } else if (col_type == "float") {
      ods_sheet[[i]] <- readr::parse_number(ods_sheet[[i]])
    } else if (col_type == "date") {
      ods_sheet[[i]] <- readr::parse_datetime(ods_sheet[[i]])
    } else if (col_type == "time") {
      ods_sheet[[i]] <- readr::parse_time(
        gsub("^PT|S$", "", gsub("H|M", ":", ods_sheet[[i]]))
      )
    })

  }

  if (nrow(dropped_values) > 0) {
    attr(ods_sheet, "dropped_values") <- dropped_values
    cli::cli_alert_warning(
      "Values dropped from {length(drop_string)} col{?s}, see attribute {.code dropped_values} for details"
    )
  }

  return(ods_sheet)

}

# common shorthand codes
shorthand <- c(
  "b", "c", "e", "er", "f", "low", "p", "r", "u", "w", "x", "z", "ns", "s",
  "ss", "sss",
  "[b]", "[c]", "[e]", "[er]", "[f]", "[low]", "[p]", "[r]", "[u]", "[w]",
  "[x]", "[z]", "[ns]", "[s]", "[ss]", "[sss]",
  "B", "C", "E", "ER", "F", "LOW", "P", "R", "U", "W", "X", "Z", "NS", "S",
  "SS", "SSS",
  "[B]", "[C]", "[E]", "[ER]", "[F]", "[LOW]", "[P]", "[R]", "[U]", "[W]",
  "[X]", "[Z]", "[NS]", "[S]", "[SS]", "[SSS]",
  ".", "..", "...", "....", ":", "-", "--", "---", "\u2013", "\u2014", "~",
  "*", "**", "***", "****", "\u2020", "\u2021", "\u00a7", "\u2016", "\u00b6",
  "\u203b", "#", "\u0394", "\u25ca"
)

# common na values
na_values <- c(
  "NA", "N/A", "NULL", "na", "n/a", "null"
)
