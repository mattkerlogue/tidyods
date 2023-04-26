
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyods <img src="man/figures/tidyods_hex.png" align="right" alt="tidyods package logo" width="120" />

<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

`{tidyods}` imports cells from an OpenDocument Spreadsheet (ODS) file,
and provides not just the cell’s value but also information about the
cell’s position, value types and formulas in a tidy format to allow
further programmatic analysis, investigation and manipulation. It also
provides methods to “rectify” cells back to a 2-dimensional data.frame.

## Installation

You can install the development version of `{tidyods}` like so:

``` r
remotes::install_github("mattkerlogue/tidyods")
```

## Usage

You can read in cells via the `read_ods_cells()` function. There is a
helper function `ods_sheets()` that can list the sheets in an ODS file.

The package includes an example ODS with two sheets: `penguins` and
`types`.

``` r
library(tidyods)

example_file <- system.file("extdata", "basic_example.ods", package = "tidyods")

ods_sheets(example_file)
#> [1] "penguins" "types"

read_ods_cells(example_file, sheet = "penguins", quiet = TRUE)
#> # A tibble: 28 × 25
#>    sheet      row   col cell_type is_empty value_type cell_content   base_value 
#>    <chr>    <dbl> <dbl> <chr>     <lgl>    <chr>      <chr>          <chr>      
#>  1 penguins     1     1 cell      FALSE    string     species        species    
#>  2 penguins     1     2 cell      FALSE    string     female         female     
#>  3 penguins     1     3 cell      FALSE    string     bill_length_mm bill_lengt…
#>  4 penguins     1     4 cell      FALSE    string     body_mass_g    body_mass_g
#>  5 penguins     2     1 cell      FALSE    string     Adelie         Adelie     
#>  6 penguins     2     2 cell      FALSE    boolean    FALSE          false      
#>  7 penguins     2     3 cell      FALSE    float      40.4           40.3904109…
#>  8 penguins     2     4 cell      FALSE    float      4043           4043.49315…
#>  9 penguins     3     1 cell      FALSE    string     Adelie         Adelie     
#> 10 penguins     3     2 cell      FALSE    boolean    TRUE           true       
#> # ℹ 18 more rows
#> # ℹ 17 more variables: numeric_value <dbl>, currency_symbol <chr>,
#> #   boolean_value <lgl>, date_value <chr>, time_value <chr>, has_formula <lgl>,
#> #   formula <chr>, has_error <lgl>, error_type <dbl>, has_annotation <lgl>,
#> #   annotation <chr>, is_merged <lgl>, merge_colspan <dbl>,
#> #   merge_rowspan <dbl>, merge_shape <chr>, cell_style <chr>, row_style <chr>
```

The `penguins` sheet is a simple 6 rows by 4 columns sheet that stores
the output of the following code:

``` r
palmerpenguins::penguins |>
  tidyr::drop_na() |>
  dplyr::group_by(species, female = sex == "female") |>
  dplyr::summarise(
    dplyr::across(c(bill_length_mm, body_mass_g), ~mean(.x, na.rm = TRUE)),
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

There are functions to “rectify” a `{tidyods}` data.frame back to a
traditional 2-dimensional array, the function `read_ods_sheet()`
combines `read_ods_cells()` and the rectify functions to easily import a
dataset.

``` r
penguin_sheet <- read_ods_sheet(example_file, "penguins", quick = TRUE, quiet = TRUE)

penguin_sheet
#> # A tibble: 4 × 8
#>   col   `1`            `2`              `3`              `4`   `5`   `6`   `7`  
#>   <chr> <chr>          <chr>            <chr>            <chr> <chr> <chr> <chr>
#> 1 x1    species        Adelie           Adelie           Chin… Chin… Gent… Gent…
#> 2 x2    female         FALSE            TRUE             FALSE TRUE  FALSE TRUE 
#> 3 x3    bill_length_mm 40.3904109589041 37.2575342465753 51.0… 46.5… 49.4… 45.5…
#> 4 x4    body_mass_g    4043.49315068493 3368.83561643836 3938… 3527… 5484… 4679…
```

The `types` sheet shows examples of the different ODS data types:

``` r
types_cells <- read_ods_cells(example_file, "types")

types_cells |> 
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::glimpse()
#> Rows: 100
#> Columns: 25
#> Groups: col [10]
#> $ sheet           <chr> "types", "types", "types", "types", "types", "types", …
#> $ row             <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ col             <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8,…
#> $ cell_type       <chr> "cell", "cell", "cell", "cell", "cell", "cell", "cell"…
#> $ is_empty        <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ value_type      <chr> "string", "boolean", "currency", "date", "time", "date…
#> $ cell_content    <chr> "Cell", "TRUE", "£1.20", "15/06/22", "13:24:56", "15/0…
#> $ base_value      <chr> "Cell", "true", "1.2", "2022-06-15", "PT13H24M56S", "2…
#> $ numeric_value   <dbl> NA, NA, 1.2000, NA, NA, NA, 12034.5679, 0.5467, 6579.2…
#> $ currency_symbol <chr> NA, NA, "GBP", NA, NA, NA, NA, NA, NA, NA, NA, NA, "GB…
#> $ boolean_value   <lgl> NA, TRUE, NA, NA, NA, NA, NA, NA, NA, NA, NA, FALSE, N…
#> $ date_value      <chr> NA, NA, NA, "2022-06-15", NA, "2022-06-15T13:24:56", N…
#> $ time_value      <chr> NA, NA, NA, NA, "PT13H24M56S", NA, NA, NA, NA, NA, NA,…
#> $ has_formula     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ formula         <chr> NA, NA, NA, NA, NA, NA, NA, NA, "of:=[.G2]*[.H2]", "of…
#> $ has_error       <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ error_type      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, 7, NA, NA, NA, NA,…
#> $ has_annotation  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ annotation      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Test comment"…
#> $ is_merged       <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ merge_colspan   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ merge_rowspan   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ merge_shape     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ cell_style      <chr> NA, NA, "ce4", "ce11", "ce21", "ce31", "ce62", NA, NA,…
#> $ row_style       <chr> "ro1", "ro1", "ro1", "ro1", "ro1", "ro1", "ro1", "ro1"…

types_cells |>
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::slice_head(n = 2) |>
  dplyr::select(-cell_type)
#> # A tibble: 20 × 24
#> # Groups:   col [10]
#>    sheet   row   col is_empty value_type cell_content   base_value numeric_value
#>    <chr> <dbl> <dbl> <lgl>    <chr>      <chr>          <chr>              <dbl>
#>  1 types     2     1 FALSE    string     Cell           Cell              NA    
#>  2 types     3     1 FALSE    string     2022-06-16T00… 2022-06-1…        NA    
#>  3 types     2     2 FALSE    boolean    TRUE           true              NA    
#>  4 types     3     2 FALSE    boolean    FALSE          false             NA    
#>  5 types     2     3 FALSE    currency   £1.20          1.2                1.2  
#>  6 types     3     3 FALSE    currency   £1.20          1.2                1.2  
#>  7 types     2     4 FALSE    date       15/06/22       2022-06-15        NA    
#>  8 types     3     4 FALSE    date       06/15/22       2022-06-15        NA    
#>  9 types     2     5 FALSE    time       13:24:56       PT13H24M5…        NA    
#> 10 types     3     5 FALSE    time       13:24          PT13H24M5…        NA    
#> 11 types     2     6 FALSE    date       15/06/2022 13… 2022-06-1…        NA    
#> 12 types     3     6 FALSE    date       15/06/22 13:24 2022-06-1…        NA    
#> 13 types     2     7 FALSE    float      12035          12034.567…     12035.   
#> 14 types     3     7 FALSE    float      12034.57       12034.567…     12035.   
#> 15 types     2     8 FALSE    float      0.5467         0.5467             0.547
#> 16 types     3     8 FALSE    percentage 55%            0.5467             0.547
#> 17 types     2     9 FALSE    float      6579.3         6579.2982…      6579.   
#> 18 types     3     9 FALSE    float      6579.3         6579.2982…      6579.   
#> 19 types     2    10 FALSE    string     #N/A           #N/A              NA    
#> 20 types     3    10 FALSE    string     #DIV/0!        #DIV/0!           NA    
#> # ℹ 16 more variables: currency_symbol <chr>, boolean_value <lgl>,
#> #   date_value <chr>, time_value <chr>, has_formula <lgl>, formula <chr>,
#> #   has_error <lgl>, error_type <dbl>, has_annotation <lgl>, annotation <chr>,
#> #   is_merged <lgl>, merge_colspan <dbl>, merge_rowspan <dbl>,
#> #   merge_shape <chr>, cell_style <chr>, row_style <chr>
```

## Performance

An ODS file is a zipped collection of XML files and associated files.
`{tidyods}`, like the `{readODS}` package, uses the
[`{xml2}`](https://xml2.r-lib.org) package to process this file. There
are three likely sources of performance issues: downloading of remote
files, unzipping the ODS file, processing rows.

With default settings, performance of `{tidyods}` functions is
comparable to `{readODS}` for small files, when working with larger
files performance is somewhat quicker than `{readODS}`.

Performance can be improved by setting `quick = TRUE` in
`read_ods_cells()` and `read_ods_sheet()`, this will extract only a
simple text representation of the cell, i.e. it will not provide
information on cell or value types, formulas or the underlying base
value for numbers, dates or times.

``` r
types_cells_quick <- read_ods_cells(example_file, "types", quick = TRUE)

types_cells_quick |>
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::glimpse()
#> Rows: 100
#> Columns: 5
#> Groups: col [10]
#> $ sheet      <chr> "types", "types", "types", "types", "types", "types", "type…
#> $ row        <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,…
#> $ col        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1…
#> $ value_type <chr> "string", "boolean", "currency", "date", "time", "date", "f…
#> $ base_value <chr> "Cell", "TRUE", "£1.20", "15/06/22", "13:24:56", "15/06/202…
```

``` r
test_file <- system.file("extdata", "basic_example.ods", package = "tidyods")

microbenchmark::microbenchmark(
+   tidy_quick = read_ods_cells(test_file, 1, quick = TRUE, quiet = TRUE),
+   tidy_slow = read_ods_cells(test_file, 1, quiet = TRUE),
+   tidy_sheet_quick = read_ods_sheet(test_file, 1, quick = TRUE, quiet = TRUE),
+   tidy_sheet_slow = read_ods_sheet(test_file, 1, col_headers = FALSE, quiet = TRUE),
+   read_ods = readODS::read_ods(test_file, 1),
+   times = 100
+ )

#> Unit: milliseconds
#>              expr      min       lq     mean   median       uq       max neval
#>        tidy_quick 15.27992 15.60860 16.31240 15.80013 16.07870  31.66500   100
#>         tidy_slow 26.96623 27.66971 30.18053 27.99699 28.71550 125.81871   100
#>  tidy_sheet_quick 19.86716 20.52970 22.55174 20.84087 21.70159  47.90370   100
#>   tidy_sheet_slow 32.29455 33.28417 35.45937 33.90800 34.99032  44.82710   100
#>          read_ods 21.84952 22.42372 23.44579 22.63387 23.04967  33.24858   100
```

To test real-world performance we will use an ODS file published by the
UK Government on the [number of civil servants by
postcode](https://www.gov.uk/government/statistics/number-of-civil-servants-by-postcode-department-responsibility-level-and-leaving-cause-2021),
a sheet of 5,544 rows by 11 columns.

``` r
postcodes_file <- system.file(
  "extdata",
  "Civil-servants-by-postcode-department-responsibility-level-and-leaving-cause-2021.ods",
  package = "tidyods"
)

microbenchmark::microbenchmark(
+   tidy_quick = read_ods_cells(postcodes_file, 2, quick = TRUE, quiet = TRUE),
+   tidy_slow = read_ods_cells(postcodes_file, 2, quiet = TRUE),
+   tidy_sheet_quick = read_ods_sheet(postcodes_file, 2, quick = TRUE, quiet = TRUE),
+   tidy_sheet_slow = read_ods_sheet(postcodes_file, 2, col_headers = FALSE, quiet = TRUE),
+   read_ods = readODS::read_ods(postcodes_file, 2),
+   times = 10
+ )

#> Unit: seconds
#>              expr       min        lq      mean    median        uq       max neval
#>        tidy_quick  4.055970  4.063083  4.146814  4.124268  4.202820  4.344831    10
#>         tidy_slow  5.844290  6.010520  6.071356  6.058574  6.183601  6.243177    10
#>  tidy_sheet_quick  4.112764  4.194695  4.325126  4.298507  4.438064  4.574808    10
#>   tidy_sheet_slow  6.037985  6.150493  6.217836  6.220751  6.315860  6.413734    10
#>          read_ods 10.505124 10.961019 11.263066 11.217981 11.605392 12.229583    10

```

The dependency on `{xml2}` is likely to cause the function to fail/crash
when working with exceptionally large files.

Performance issues are tracked and discussed in [issue
\#3](https://github.com/mattkerlogue/tidyods/issues/3) in the package’s
GitHub repository.

## Related projects

The `{tidyODS}` package is heavily inspired by three existing packages:

- The [`{readODS}`](https://github.com/chainsawriot/readODS) package
  provides functionality to read and write ODS files with R, with the
  resulting data.frame reflecting the way the data is viewed in a
  spreadsheet editor application (i.e a two-dimension table structure).
- The [`{tidyxl}`](https://nacnudus.github.io/tidyxl/) package reads
  cells from Excel files and provides a data.frame where each cell is
  its own row and columns provide information about the cell’s location,
  value and value type, formula and formatting.
- The [`{unpivotr}`](https://nacnudus.github.io/unpivotr/) package works
  with datasets of cells (such as those created by tidyxl).

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
package was available. This is not the same when working with Excel
spreadsheets, for example if you run into an error when using the
`{readxl}` package you could use easily try the `{openxlsx}` or `{xlsx}`
packages instead.

The initial conceptual development of code lead to the creation of an
output dataset similar to that produced by the `{tidyxl}` package. Thus,
`{tidyods}` extracts cells from an ODS document in the same manner as
`{tidyxl}`, but includes functions to “rectify” the cells so as to also
provide an output similar to that of `{readODS}` (and many of those that
read Excel files). This is similar to the
[rectify](https://nacnudus.github.io/unpivotr/reference/rectify.html)
function in the `{unpivotr}` package.

## Code of Conduct

Please note that the tidyods project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms. Please read the [contributing
guidelines](.github/CONTRIBUTING.md).
