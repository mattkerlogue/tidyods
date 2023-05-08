# Performance examples
# ====================
# These examples and their output are included in README.Rmd as code blocks
# in the Performance section. They are not run as code within the Rmd as they
# take significant time, instead run separately and copy to the README.Rmd file.

rstudioapi::restartSession()
devtools::load_all()

example_file <- system.file("extdata", "basic_example.ods", package = "tidyods")

bench::mark(
  "cells_quick" =
    read_ods_cells(example_file, 2, quick = TRUE, quiet = TRUE),
  "cells_slow" =
    read_ods_cells(example_file, 2, quiet = TRUE),
  "sheet_quick" =
    read_ods_sheet(example_file, 2, quick = TRUE, quiet = TRUE),
  "sheet_slow" =
    read_ods_sheet(example_file, 2, col_headers = FALSE, quiet = TRUE),
  "readODS" =
    readODS::read_ods(example_file, 2),
  check = FALSE, filter_gc = FALSE, iterations = 20
) |>
  dplyr::transmute(expression, min, median, mean = total_time/n_itr, n_itr)

postcodes_file <- system.file("extdata", "civil-service-postcodes-2021.ods",
                              package = "tidyods")

bench::mark(
  "cells_quick" =
    read_ods_cells(postcodes_file, 2, quick = TRUE, quiet = TRUE),
  "cells_slow" =
    read_ods_cells(postcodes_file, 2, quiet = TRUE),
  "sheet_quick" =
    read_ods_sheet(postcodes_file, 2, quick = TRUE, quiet = TRUE),
  "sheet_slow" =
    read_ods_sheet(postcodes_file, 2, col_headers = FALSE, quiet = TRUE),
  "readODS" =
    readODS::read_ods(postcodes_file, 2),
  check = FALSE, filter_gc = FALSE, iterations = 5
) |>
  dplyr::transmute(expression, min, median, mean = total_time/n_itr, n_itr)

