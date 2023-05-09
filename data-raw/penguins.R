# data about penguins
# ====================
# data from {palmerpenguins} is used to create example ODS files
# this code creates csv files contained in the data-raw folder

# basic_example -----------------------------------------------------------
# this code is for the "penguins" sheet in the basic_example.ods file and
# its Excel and Google variants

penguins_example <- palmerpenguins::penguins |>
  tidyr::drop_na() |>
  dplyr::group_by(species, female = sex == "female") |>
  dplyr::summarise(
    dplyr::across(c(bill_length_mm, body_mass_g), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

readr::write_csv(penguins_example, "data-raw/penguins-example.csv")

# smart_rectify examples --------------------------------------------------
# this code creates csv files contained in the penguin_statistics.ods file
# which is an approximation of a file designed to meet the UK government's
# releasing statistics in spreadsheets guidance
# https://analysisfunction.civilservice.gov.uk/policy-store/releasing-statistics-in-spreadsheets/

penguins_table1 <- palmerpenguins::penguins |>
  dplyr::select(-year) |>
  dplyr::group_by(species, sex) |>
  dplyr::add_count() |>
  dplyr::summarise(
    across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    sex = stringr::str_to_title(sex),
    across(where(is.numeric), ~dplyr::if_else(.data$n < 10, "[c]", as.character(.x))),
    sex = forcats::fct_na_value_to_level(sex, level = "Missing [Note 3]")
  ) |>
  dplyr::select(-n)

readr::write_csv(penguins_table1, "data-raw/penguins-table1.csv")

penguin_isotopes <- palmerpenguins::penguins_raw |>
  dplyr::select(
    island = Island, sex = Sex,
    delta15 = `Delta 15 N (o/oo)`, delta13 = `Delta 13 C (o/oo)`
  ) |>
  dplyr::mutate(
    sex = forcats::fct_na_value_to_level(stringr::str_to_title(sex), "Missing [Note 3]")
  )

penguin_isotope_samples <- penguin_isotopes |>
  dplyr::group_by(island, sex) |>
  dplyr::summarise(
    n_d15 = sum(!is.na(delta15)), n_d13 = sum(!is.na(delta13)),
    .groups = "drop"
  )

penguins_table2 <- penguin_isotopes |>
  dplyr::group_by(island, sex) |>
  dplyr::summarise(
    across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::left_join(penguin_isotope_samples, by = c("island", "sex")) |>
  dplyr::mutate(
    across(c(delta15, delta13), scales::number_format(0.01)),
    delta15 = dplyr::case_when(
      n_d15 == 0 ~ "[w]",
      n_d15 < 10 ~ "[c]",
      n_d15 < 50 ~ paste0(delta15, " [e]"),
      TRUE ~ as.character(delta15)
    ),
    delta13 = dplyr::case_when(
      n_d13 == 0 ~ "[w]",
      n_d13 < 10 ~ "[c]",
      n_d13 < 50 ~ paste0(delta13, " [e]"),
      TRUE ~ as.character(delta13)
    )
  ) |>
  dplyr::select(-n_d15, -n_d13)

readr::write_csv(penguins_table2, "data-raw/penguins-table2.csv")
