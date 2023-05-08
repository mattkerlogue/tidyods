# low-level component processing

sheet_components <- function(sheet_xml, ns) {

  sheet_tbl <- tibble::tibble(
    sheet = xml2::xml_attr(sheet_xml, "table:name", ns),
    sheet_path = xml2::xml_path(sheet_xml)
  )

  return(sheet_tbl)

}

row_components <- function(sheet_xml, ns) {

  row_xml <- xml2::xml_find_all(sheet_xml, "descendant::table:table-row", ns)
  row_tbl <- tibble::tibble(
    row_path = xml2::xml_path(row_xml),
    row_repeats = suppressWarnings(
      as.numeric(xml2::xml_attr(row_xml, "table:number-rows-repeated", ns))
    ),
    row_style = xml2::xml_attr(row_xml, "table:style-name", ns)
  )

  return(row_tbl)

}

col_components <- function(sheet_xml, ns) {

  col_xml <- xml2::xml_find_all(sheet_xml, "descendant::table:table-column", ns)
  col_tbl_0 <- tibble::tibble(
    base_col = seq_along(col_xml),
    col_path = xml2::xml_path(col_xml),
    col_style = xml2::xml_attr(col_xml, "table:style-name", ns),
    col_repeats = suppressWarnings(
      as.numeric(xml2::xml_attr(col_xml, "table:number-columns-repeated", ns))
    ),
    col_default_cell_style = xml2::xml_attr(
      col_xml, "table:default-cell-style-name", ns
    )
  )

  col_tbl_0$col_repeats[is.na(col_tbl_0$col_repeats)] <- 1
  col_tbl_0$sheet_path <- gsub(
    "(.*table:table(\\[\\d+\\])?)\\/.*", "\\1", col_tbl_0$col_path
  )

  if (max(col_tbl_0$col_repeats) > 1) {
    col_tbl <- col_tbl_0 |>
      tidyr::uncount(.data$col_repeats, .remove = FALSE, .id = "col_iteration") |>
      dplyr::arrange(.data$sheet_path, .data$base_col, .data$col_iteration) |>
        dplyr::group_by(.data$sheet_path, .add = FALSE) |>
        dplyr::mutate(
          col = dplyr::row_number(),
          .before = 1L
        ) |>
        dplyr::ungroup()
  } else {
    col_tbl <- col_tbl_0
    col_tbl$col <- col_tbl_0$base_col
  }

  col_tbl <- col_tbl |>
    dplyr::select(.data$sheet_path, .data$col, .data$col_style,
                  .data$col_default_cell_style)

  return(col_tbl)

}

cell_components_basic <- function(sheet_xml, ns) {

  cell_xml <- cell_xml_nodes(sheet_xml, ns)

  cell_paths <- xml2::xml_path(cell_xml)

  cell_text <- extract_text(cell_xml, ns)

  cell_tbl <- tibble::tibble(
    cell_id = seq_along(cell_xml),
    cell_path = cell_paths,
    cell_el = xml2::xml_name(cell_xml, ns),
    row_path = gsub("(.*table:table-row(\\[\\d+\\])?).*", "\\1", cell_paths),
    sheet_path = gsub("(.*table:table(\\[\\d+\\])?)\\/.*", "\\1", cell_paths),
    base_row = suppressWarnings(
      as.numeric(gsub(".*table:table-row\\[(\\d+)\\].*", "\\1", cell_paths))
    ),
    col_repeats = suppressWarnings(
      as.numeric(xml2::xml_attr(cell_xml, "table:number-columns-repeated", ns))
    ),
    office_value_type = xml2::xml_attr(cell_xml, "office:value-type", ns),
    cell_numeric = xml2::xml_attr(cell_xml, "office:value", ns)
  ) |>
    dplyr::left_join(cell_text, by = "cell_path") |>
    dplyr::mutate(
      cell_content = dplyr::if_else(
        .data$cell_content == "", NA_character_, .data$cell_content
      ),
      base_value = dplyr::case_when(
        is.na(.data$office_value_type) ~ NA_character_,
        .data$office_value_type == "currency" ~ .data$cell_content,
        grepl("^#", .data$cell_content) ~ .data$cell_content,
        !is.na(.data$cell_numeric) ~ .data$cell_numeric,
        TRUE ~ .data$cell_content
      )
    ) |>
    dplyr::group_by(.data$sheet_path, .data$row_path) |>
    dplyr::mutate(base_col = dplyr::row_number(), .after = "base_row")

  return(cell_tbl)

}

cell_components_extended <- function(sheet_xml, ns) {

  cell_xml <- cell_xml_nodes(sheet_xml, ns)

  cell_paths <- xml2::xml_path(cell_xml)

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
    cell_el = xml2::xml_name(cell_xml, ns),
  ) |>
    dplyr::group_by(.data$sheet_path, .data$row_path) |>
    dplyr::mutate(base_col = dplyr::row_number(), .after = "base_row") |>
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
    dplyr::bind_rows(cell_tbl_0)

  names(cell_tbl)[names(cell_tbl) == "table_number_columns_repeated"] <- "col_repeats"

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

  if ("table_number_columns_repeated" %in% names(cell_attrs_tbl)) {
    cell_attrs_tbl[["table_number_columns_repeated"]] <- suppressWarnings(
      as.numeric(cell_attrs_tbl$table_number_columns_repeated)
      )
  }

  return(cell_attrs_tbl)

}

extract_annotations <- function(cell_xml, ns) {

  annotation_xml <- xml2::xml_find_all(
    cell_xml,
    "office:annotation/text:p",
    ns
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
    n_rep = suppressWarnings(
      as.numeric(xml2::xml_attr(annotation_fragments, "text:c", ns))
      )
  ) |>
    dplyr::mutate(
      n_rep = tidyr::replace_na(.data$n_rep, 1),
      text = dplyr::if_else(.data$el == "text:s", " ", .data$text),
      text_path = gsub(
        "(.*office:annotation/text:p(\\[\\d+\\])?).*", "\\1", .data$xml_path
      )
    ) |>
    dplyr::arrange(.data$text_path, .data$id) |>
    dplyr::reframe(
      cell_annotation = paste0(rep(.data$text, .data$n_rep), collapse = ""),
      .by = "text_path"
    ) |>
    dplyr::mutate(
      cell_path = gsub("(.*table-cell(\\[\\d+\\])?).*", "\\1", .data$text_path)
    ) |>
    dplyr::reframe(
      cell_annotation = paste0(.data$cell_annotation, collapse = "\n"),
      .by = "cell_path"
    )

  return(annotation_tbl)

}

extract_text <- function(cell_xml, ns) {

  cell_children <- xml2::xml_children(cell_xml)
  child_el <- xml2::xml_name(cell_children)
  text_xml <- cell_children[child_el == "p"]
  text_fragments <- xml2::xml_contents(text_xml)

  text_tbl <- tibble::tibble(
    id = seq_len(length(text_fragments)),
    xml_path = xml2::xml_path(text_fragments),
    el = xml2::xml_name(text_fragments),
    text = xml2::xml_text(text_fragments),
    n_rep = suppressWarnings(
      as.numeric(xml2::xml_attr(text_fragments, "c"))
    )
  ) |>
    dplyr::mutate(
      n_rep = tidyr::replace_na(.data$n_rep, 1),
      text = dplyr::if_else(.data$el == "s", " ", .data$text),
      text_path = gsub(
        "(.*table-cell(\\[\\d+\\])?\\/text:p(\\[\\d+\\])?).*", "\\1",
        .data$xml_path
      )
    ) |>
    dplyr::arrange(.data$text_path, .data$id) |>
    dplyr::reframe(
      cell_content = paste0(rep(.data$text, .data$n_rep), collapse = ""),
      .by = "text_path"
    ) |>
    dplyr::mutate(
      cell_path = gsub("(.*table-cell(\\[\\d+\\])?).*", "\\1", .data$text_path)
    ) |>
    dplyr::reframe(
      cell_content = paste0(.data$cell_content, collapse = "\n"),
      .by = "cell_path"
    )

  return(text_tbl)

}

combine_components <- function(cell_tbl, row_tbl, col_tbl = NULL) {

  init_tbl <- cell_tbl |>
    dplyr::left_join(row_tbl, by = "row_path")

  init_tbl$base_row[is.na(init_tbl$base_row)] <- 1
  init_tbl$base_col[is.na(init_tbl$base_col)] <- 1
  init_tbl$col_repeats[is.na(init_tbl$col_repeats)] <- 1
  init_tbl$row_repeats[is.na(init_tbl$row_repeats)] <- 1

  base_col_n <- table(init_tbl$row_path)

  init_tbl$keep <- dplyr::case_when(
    init_tbl$cell_el == "table:covered-table-cell" ~ TRUE,
    init_tbl$base_col < base_col_n[init_tbl$row_path] ~ TRUE,
    is.na(init_tbl$cell_content) & is.na(init_tbl$office_value_type) &
      init_tbl$col_repeats > 1 ~ FALSE,
    is.na(init_tbl$cell_content) & is.na(init_tbl$office_value_type) &
      init_tbl$col_repeats > 1 ~ FALSE,
    TRUE ~ TRUE
  )

  full_tbl_0 <- init_tbl[init_tbl[["keep"]], ] |>
    tidyr::uncount(.data$row_repeats, .remove = FALSE, .id = "row_iteration") |>
    tidyr::uncount(.data$col_repeats, .remove = FALSE, .id = "col_iteration") |>
    dplyr::arrange(
      .data$sheet_path,
      .data$base_row, .data$row_iteration,
      .data$base_col, .data$col_iteration
    ) |>
    dplyr::group_by(
      .data$sheet_path, .data$base_row, .data$row_iteration, .add = FALSE
    ) |>
    dplyr::mutate(
      row = dplyr::cur_group_id(),
      col = dplyr::row_number(),
      .before = 1L
    ) |>
    dplyr::ungroup()

  # correct row numbers when multiple sheets are being extracted
  if (length(unique(full_tbl_0$sheet_path)) > 1) {

    sheet_nrows <- full_tbl_0 |>
      dplyr::distinct(.data$sheet_path, .data$row_path) |>
      dplyr::count(.data$sheet_path) |>
      dplyr::mutate(
        row_correction = dplyr::lag(cumsum(.data$n), default = 0)
      )

    full_tbl <- full_tbl_0 |>
      dplyr::left_join(sheet_nrows, by = "sheet_path") |>
      dplyr::mutate(
        row = row - .data$row_correction
      ) |>
      dplyr::select(-.data$row_correction)

  } else {
    full_tbl <- full_tbl_0
  }

  if (!is.null(col_tbl)) {
    full_tbl <- full_tbl |>
      dplyr::left_join(col_tbl, by = c("sheet_path", "col"))
  }

  full_tbl$address = cellranger::R1C1_to_A1(
    paste0("R", full_tbl$row, "C", full_tbl$col),
    strict = FALSE
  )

  return(full_tbl)

}


