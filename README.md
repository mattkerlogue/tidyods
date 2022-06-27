
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyods <img src="man/figures/tidyods_hex.png" align="right" alt="tidyods package logo" width="120" />

<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

`{tidyods}` is an R package to import cells from an OpenDocument
Spreadsheet (ODS) file, and provide information about the cell’s
postion, value types and formulas, and provide methods to “rectify”
cells back to a 2-dimensional data.frame (specifically as a tidyverse
tibble).

## Installation

You can install the development version of `{tidyods}` like so:

``` r
remotes::install_github("mattkerlogue/tidyods")
```

## Usage

The `{tidyods}` package exports the following functions for users:

-   `read_ods_cells(path, sheet, quick = FALSE)`: extract the cells from
    a sheet in an ODS file
-   `read_ods_sheet(path, sheet, rectify = "simple", base_values = TRUE, quick = FALSE)`:
    extract cells from an ODS sheet and return as a rectangular dataset,
    like a spreadsheet
-   `ods_sheets(path)`: list the sheets in an ODS fie
-   `simple_rectify(path)`: “rectify” a set of cells into a rectangular
    dataset, like a spreadsheet

The package includes an example ODS with two sheets: `penguins` and
`types`.

``` r
library(tidyods)

example_file <- system.file("extdata", "basic_example.ods", package = "tidyods")

ods_sheets(example_file)
#> [1] "penguins" "types"
```

The `penguins` sheet is a simple 6 rows by 4 columns sheet that stores
the output of the following code:

``` r
palmerpenguins::penguins |>
  tidyr::drop_na() |>
  dplyr::group_by(species, female = sex == "female") |>
  dplyr::summarise(
    dplyr::across(c(bill_length_mm, body_mass_g), mean, na.rm = TRUE),
    .groups = "drop"
  )
#> # A tibble: 6 × 4
#>   species   female bill_length_mm body_mass_g
#>   <fct>     <lgl>           <dbl>       <dbl>
#> 1 Adelie    FALSE            40.4       4043.
#> 2 Adelie    TRUE             37.3       3369.
#> 3 Chinstrap FALSE            51.1       3939.
#> 4 Chinstrap TRUE             46.6       3527.
#> 5 Gentoo    FALSE            49.5       5485.
#> 6 Gentoo    TRUE             45.6       4680.
```

``` r
penguin_sheet <- read_ods_sheet(example_file, "penguins", quick = TRUE)
#> ℹ Extracting XML
#> ✔ Extracting XML [12ms]
#> 
#> ℹ Getting sheet
#> ✔ Getting sheet [7ms]
#> 
#> ℹ Extracting cell and row info
#> ✔ Extracting cell and row info [22ms]
#> 
#> ℹ Generating output table
#> ✔ Generating output table [25ms]
#> 
#> ℹ Rectifying cells to sheet layout
#> ✔ Rectifying cells to sheet layout [15ms]
#> 

penguin_sheet
#> # A tibble: 6 × 4
#>   species   female bill_length_mm body_mass_g
#>   <chr>     <chr>  <chr>          <chr>      
#> 1 Adelie    FALSE  40.4           4043       
#> 2 Adelie    TRUE   37.3           3369       
#> 3 Chinstrap FALSE  51.1           3939       
#> 4 Chinstrap TRUE   46.6           3527       
#> 5 Gentoo    FALSE  49.5           5485       
#> 6 Gentoo    TRUE   45.6           4680
```

While the `types` sheet shows examples of the different ODS data types:

``` r
types_cells <- read_ods_cells(example_file, "types")
#> ℹ Extracting XML
#> ✔ Extracting XML [4ms]
#> 
#> ℹ Getting sheet
#> ✔ Getting sheet [6ms]
#> 
#> ℹ Extracting cell and row info
#> ✔ Extracting cell and row info [28ms]
#> 
#> ℹ Generating output table
#> ✔ Generating output table [30ms]
#> 

types_cells |> 
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::glimpse()
#> Rows: 110
#> Columns: 15
#> Groups: col [10]
#> $ sheet           <chr> "types", "types", "types", "types", "types", "types", …
#> $ row             <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ col             <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8,…
#> $ cell_type       <chr> "cell", "cell", "cell", "cell", "cell", "cell", "cell"…
#> $ value_type      <chr> "string", "boolean", "currency", "date", "time", "date…
#> $ is_empty        <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ cell_content    <chr> "Cell", "TRUE", "£1.20", "15/06/22", "13:24:56", "15/0…
#> $ base_value      <chr> "Cell", "true", "1.2", "2022-06-15", "PT13H24M56S", "2…
#> $ numeric_value   <dbl> NA, NA, 1.2000, NA, NA, NA, 12034.5679, 0.5467, 6579.2…
#> $ logical_value   <lgl> NA, TRUE, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, N…
#> $ currency_symbol <chr> NA, NA, "GBP", NA, NA, NA, NA, NA, NA, NA, NA, NA, "GB…
#> $ has_formula     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ cell_formula    <chr> NA, NA, NA, NA, NA, NA, NA, NA, "of:=[.G2]*[.H2]", "of…
#> $ has_error       <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ error_type      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, 7, NA, NA, NA, NA,…

types_cells |>
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::slice_head(n = 2) |>
  dplyr::select(-cell_type)
#> # A tibble: 20 × 14
#> # Groups:   col [10]
#>    sheet   row   col value_type is_empty cell_content   base_value numeric_value
#>    <chr> <int> <int> <chr>      <lgl>    <chr>          <chr>              <dbl>
#>  1 types     2     1 string     FALSE    Cell           Cell              NA    
#>  2 types     3     1 string     FALSE    Cell with com… Cell with…        NA    
#>  3 types     2     2 boolean    FALSE    TRUE           true              NA    
#>  4 types     3     2 boolean    FALSE    FALSE          false             NA    
#>  5 types     2     3 currency   FALSE    £1.20          1.2                1.2  
#>  6 types     3     3 currency   FALSE    £1.20          1.2                1.2  
#>  7 types     2     4 date       FALSE    15/06/22       2022-06-15        NA    
#>  8 types     3     4 date       FALSE    06/15/22       2022-06-15        NA    
#>  9 types     2     5 time       FALSE    13:24:56       PT13H24M5…        NA    
#> 10 types     3     5 time       FALSE    13:24          PT13H24M5…        NA    
#> 11 types     2     6 date       FALSE    15/06/2022 13… 2022-06-1…        NA    
#> 12 types     3     6 date       FALSE    15/06/22 13:24 2022-06-1…        NA    
#> 13 types     2     7 float      FALSE    12035          12034.567…     12035.   
#> 14 types     3     7 float      FALSE    12034.57       12034.567…     12035.   
#> 15 types     2     8 float      FALSE    0.5467         0.5467             0.547
#> 16 types     3     8 percentage FALSE    55%            0.5467             0.547
#> 17 types     2     9 float      FALSE    6579.3         6579.2982…      6579.   
#> 18 types     3     9 float      FALSE    6579.3         6579.2982…      6579.   
#> 19 types     2    10 string     FALSE    #N/A           #N/A              NA    
#> 20 types     3    10 string     FALSE    #DIV/0!        #DIV/0!           NA    
#> # … with 6 more variables: logical_value <lgl>, currency_symbol <chr>,
#> #   has_formula <lgl>, cell_formula <chr>, has_error <lgl>, error_type <dbl>
```

## Performance

An ODS file is a zipped collection of XML files and associated files.
`{tidyods}`, like the `{readODS}` package, uses the
[xml2](https://xml2.r-lib.org) package to process this file. There are
three likely sources of performance issues: downloading of remote files,
unzipping the ODS file, processing rows.

With default settings, performance of `{tidyods}` functions is
marginally slower for small files than `{readODS}`, but this is unlikely
to be noticeable to users. When working with larger files performnace is
comparable.

Performance can be improved by setting `quick = TRUE` in
`read_ods_cells()` and `read_ods_sheet()`, this will extract only a
simple text representation of the cell, i.e. it will not provide
information on cell or value types, formulas or the underlying base
value for numbers, dates or times.

``` r
types_cells_quick <- read_ods_cells(example_file, "types", quick = TRUE)
#> ℹ Extracting XML
#> ✔ Extracting XML [4ms]
#> 
#> ℹ Getting sheet
#> ✔ Getting sheet [6ms]
#> 
#> ℹ Extracting cell and row info
#> ✔ Extracting cell and row info [19ms]
#> 
#> ℹ Generating output table
#> ✔ Generating output table [23ms]
#> 

types_cells_quick |>
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::glimpse()
#> Rows: 110
#> Columns: 4
#> Groups: col [10]
#> $ sheet        <chr> "types", "types", "types", "types", "types", "types", "ty…
#> $ row          <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ col          <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9,…
#> $ cell_content <chr> "Cell", "TRUE", "£1.20", "15/06/22", "13:24:56", "15/06/2…
```

``` r
test_file <- system.file("extdata", "basic_example.ods", package = "tidyods")

microbenchmark::microbenchmark(
  tidy_quick = read_ods_cells(test_file, 1, quick = TRUE),
  tidy_slow = read_ods_cells(test_file, 1),
  tidy_sheet_quick = read_ods_sheet(test_file, 1, quick = TRUE),
  tidy_sheet_slow = read_ods_sheet(test_file, 1),
  read_ods = readODS::read_ods(test_file, 1),
  times = 100
)

#> Unit: milliseconds
#>              expr       min       lq     mean   median       uq       max neval
#>        tidy_quick 24.280200 25.44304 27.45693 26.25878 29.51594  33.89195   100
#>         tidy_slow 34.481574 36.26819 38.81044 37.82586 41.17425  47.92445   100
#>  tidy_sheet_quick 31.678445 33.50053 36.11012 34.95258 38.76044  44.62018   100
#>   tidy_sheet_slow 41.229846 42.25362 47.12949 46.38392 47.86963 125.16058   100
#>          read_ods  9.995062 10.46072 12.07059 10.58868 10.95301 118.04728   100
```

To test real-world performance we will use an ODS file published by the
UK Government on the [number of civil servants by
postcode](https://www.gov.uk/government/statistics/number-of-civil-servants-by-postcode-department-responsibility-level-and-leaving-cause-2021)
(5,544 rows by 11 columns). Analysis of read times indicates similar
performance to that achieved by `{readODS}`.

``` r
postcodes_file <- "~/Downloads/Civil-servants-by-postcode-department-
  responsibility-level-and-leaving-cause-2021.ods"

microbenchmark::microbenchmark(
  tidy_quick = read_ods_cells(postcodes_file, 2, quick = TRUE),
  tidy_slow = read_ods_cells(postcodes_file, 2),
  tidy_sheet_quick = read_ods_sheet(postcodes_file, 2, quick = TRUE),
  tidy_sheet_slow = read_ods_sheet(postcodes_file, 2),
  read_ods = readODS::read_ods(postcodes_file, 2),
  times = 10
)

#> Unit: seconds
#>              expr      min       lq     mean   median       uq      max neval
#>        tidy_quick 5.696553 5.706185 5.849200 5.808597 5.990277 6.058689    10
#>         tidy_slow 6.909598 7.037668 7.124450 7.146029 7.185813 7.287308    10
#>  tidy_sheet_quick 5.758918 5.824600 5.951630 5.926395 6.084767 6.182411    10
#>   tidy_sheet_slow 6.963679 7.049813 7.158550 7.091867 7.176565 7.626200    10
#>          read_ods 4.774232 4.893009 5.025191 5.065590 5.108057 5.236603    10
```

Due to the reliance on `{xml2}` very large files will perform poorly or
run into memory limitations. This is being investigated as part of the
package development.

Performance issues are tracked and discussed in [issue
\#3](https://github.com/mattkerlogue/tidyods/issues/3) in the package’s
GitHub repository.

## Related projects

The `{tidyODS}` package is heavily inspired by two existing packages:

-   The [`{readODS}`](https://github.com/chainsawriot/readODS) package
    provides functionality to read and write ODS files with R, with the
    resulting data.frame reflecting the way the data is viewed in a
    spreadsheet editor application (i.e a two-dimension table
    structure).
-   The [`{tidyxl}`](https://nacnudus.github.io/tidyxl/) package reads
    cells from Excel files and provides a data.frame where each cell is
    its own row and columns provide information about the cell’s
    location, value and value type, formula and formatting.
-   The [`{unpivotr}`](https://nacnudus.github.io/unpivotr/) package
    works with datasets of cells (such as those created by tidyxl).

## Philosophy

The `{readODS}` package is the only package on CRAN that handles the
reading of ODS files, whereas there are more than 20 packages (as at 12
June 2022) on CRAN that work with Excel files (of which `{tidyxl}` is
one). In some respects this is due to the wider usage of Excel files by
businesses, governments, academia and other organisations to publish and
share data. Various governments and international organisations are
starting to mandate the use of OpenDocument Format files for publishing
of their information.

The initial purpose of `{tidyods}` was to provide a second package to
the R ecosystem for reading ODS files, in part prompted when
encountering an error with `{readODS}` and discovering no alternative
pacakge was available. For example if you run into an error when using
the `{readxl}` package you could use easily try the`{openxlsx}` or
`{xlsx}` instead.

The conceptual code development lead to the creation of an output
dataset similar to that produced by the `{tidyxl}` package.

Thus, `{tidyods}` extracts cells from an ODS document in the same manner
as `{tidyxl}`, but includes functions to “rectify” the cells so as to
also provide an output similar to that of `{readODS}` (and many of those
that read Excel files). This is similar to the
[rectify](https://nacnudus.github.io/unpivotr/reference/rectify.html)
function in the `{unpivotr}` package.
