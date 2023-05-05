example_file <- system.file("extdata", "basic_example.ods", package = "tidyods")
example_ods <- extract_ods_xml(example_file)
example_ns <- xml2::xml_ns(example_ods)

excel_file <- system.file("extdata", "basic_example_excel.ods", package = "tidyods")
excel_ods <- extract_ods_xml(excel_file)
excel_ns <- xml2::xml_ns(excel_ods)

sheet_paths <- c(
  "/office:document-content/office:body/office:spreadsheet/table:table[1]",
  "/office:document-content/office:body/office:spreadsheet/table:table[2]"
)
names(sheet_paths) <- c("penguins", "types")


test_that("Sheet names", {
  expect_equal(ods_sheets(example_file), c("penguins", "types"))
  expect_equal(ods_sheets(excel_file), c("penguins", "types"))
})

test_that("Sheet paths (LibreOffice)", {
  expect_equal(ods_sheet_paths(example_ods, example_ns), sheet_paths)
  expect_equal(get_sheet_path(example_ods, 1, example_ns), sheet_paths[1])
  expect_equal(get_sheet_path(example_ods, 2, example_ns), sheet_paths[2])
  expect_equal(get_sheet_path(example_ods, "penguins", example_ns), sheet_paths[1])
  expect_equal(get_sheet_path(example_ods, "types", example_ns), sheet_paths[2])

  expect_error(get_sheet_path(example_ods, ns = example_ns))
  expect_error(
    get_sheet_path(example_ods, 1:2, example_ns),
    regexp = "`sheet` must a character or numeric vector of length 1"
  )
  expect_error(
    get_sheet_path(example_ods, "hello", example_ns),
    regexp = "not a valid sheet name"
  )
  expect_error(
    get_sheet_path(example_ods, TRUE, example_ns),
    regexp = "`sheet` must be the name of a sheet or index number"
  )
})

test_that("Sheet paths (Excel)", {
  expect_equal(ods_sheet_paths(excel_ods, excel_ns), sheet_paths)
  expect_equal(get_sheet_path(excel_ods, 1, excel_ns), sheet_paths[1])
  expect_equal(get_sheet_path(excel_ods, 2, excel_ns), sheet_paths[2])
  expect_equal(get_sheet_path(excel_ods, "penguins", excel_ns), sheet_paths[1])
  expect_equal(get_sheet_path(excel_ods, "types", excel_ns), sheet_paths[2])

  expect_error(get_sheet_path(excel_ods, ns = excel_ods))
  expect_error(
    get_sheet_path(excel_ods, 1:2, excel_ns),
    regexp = "`sheet` must a character or numeric vector of length 1"
  )
  expect_error(
    get_sheet_path(excel_ods, "hello", excel_ns),
    regexp = "not a valid sheet name"
  )
  expect_error(
    get_sheet_path(excel_ods, TRUE, excel_ns),
    regexp = "`sheet` must be the name of a sheet or index number"
  )
})
