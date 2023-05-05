sheet_xml_nodes <- function(ods_xml, sheet_path, ns) {

  if (is.na(sheet_path)) {
    sheet_xml <- xml2::xml_find_all(
      ods_xml,
      "descendant::table:table",
      ns
    )
  } else {
    sheet_xml <- ods_xml |>
      xml2::xml_find_first(sheet_path)
  }

  return(sheet_xml)
}

cell_xml_nodes <- function(sheet_xml, ns) {
  xml2::xml_find_all(
    sheet_xml,
    "descendant::table:table-cell | descendant::table:covered-table-cell",
    ns
  )
}
