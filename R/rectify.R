#' Convert ODS cells to a two-dimensional dataset
#'
#' `simple_rectify` converts a set of cells  extracted by [read_ods_cells()]
#' into a traditional rectangular two-dimensional structure.
#'
#' @param ods_cells A set of cells from an ODS sheet
#' @param base_values Whether to use the base_value of a cell (TRUE, the default)
#'   or whether to provide the cell content as seen by a spreadsheet user.
#'
#' @details
#' When set to `TRUE` (the default) the `base_values` argument then the
#' underlying cell values are used. When `FALSE` then cell content as seen by a
#' spreadsheet application user will be shown. See [read_ods_cells()] for further
#' details.
#'
#' Column headers are of the format "X#", i.e. "X" and the column number. You
#' can use another function such as [janitor::row_to_names()] to replace these
#' with values from a row within the dataset.
#'
#' @return
#' A tibble representing the original spreadsheet format.
#'
#' @examples
#' example <- system.file("extdata", "basic_example.ods", package = "tidyods")
#' example_cells <- read_ods_cells(example, 1)
#' simple_rectify(example_cells, base_values = TRUE)
#'
#' @export
simple_rectify <- function(ods_cells, base_values = TRUE) {

  if ("sheet" %in% names(ods_cells)) {
    if(length(unique(ods_cells[["sheet"]])) > 1) {
      cli::cli_abort(
        c(
          x = "More than one sheet present in the cells"
        )
      )
    }
  } else {
    distinct_rc <- dplyr::distinct(ods_cells, row, col)
    if (nrow(distinct_rc) != nrow(ods_cells)) {
      cli::cli_abort(
        c(
          x = "Row/column combinations are duplicated"
        )
      )
    }
  }

  all_cell_positions <- tidyr::crossing(
    row = 1:max(ods_cells$row),
    col = 1:max(ods_cells$col)
  )

  if (base_values) {
    simple_content <- ods_cells %>%
      dplyr::select(row, col, cell_value = base_value)
  } else  {
    simple_content <- ods_cells %>%
      dplyr::select(row, col, cell_content) %>%
      dplyr::mutate(
        cell_value = dplyr::if_else(cell_content == "",
                                    NA_character_,
                                    cell_content)
      ) %>%
      dplyr::select(-cell_content)
  }

  ods_sheet <- all_cell_positions %>%
    dplyr::left_join(simple_content, by = c("row", "col")) %>%
    dplyr::mutate(col = paste0("X", col)) %>%
    tidyr::pivot_wider(names_from = col, values_from = cell_value) %>%
    dplyr::select(-row)

  # handle Excel blank final column
  if (sum(is.na(ods_sheet[[ncol(ods_sheet)]])) == nrow(ods_sheet)) {
    ods_sheet <- ods_sheet[, 1:(ncol(ods_sheet) - 1)]
  }

  return(ods_sheet)

}
