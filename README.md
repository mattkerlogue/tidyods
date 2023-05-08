
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
#> [1] "penguins" "types"    "merges"

read_ods_cells(example_file, sheet = "penguins", quiet = TRUE)
#> # A tibble: 28 × 28
#>    sheet    address   row   col cell_type is_empty value_type cell_content  
#>    <chr>    <chr>   <dbl> <dbl> <chr>     <lgl>    <chr>      <chr>         
#>  1 penguins A1          1     1 cell      FALSE    string     species       
#>  2 penguins B1          1     2 cell      FALSE    string     female        
#>  3 penguins C1          1     3 cell      FALSE    string     bill_length_mm
#>  4 penguins D1          1     4 cell      FALSE    string     body_mass_g   
#>  5 penguins A2          2     1 cell      FALSE    string     Adelie        
#>  6 penguins B2          2     2 cell      FALSE    boolean    FALSE         
#>  7 penguins C2          2     3 cell      FALSE    float      40.4          
#>  8 penguins D2          2     4 cell      FALSE    float      4043          
#>  9 penguins A3          3     1 cell      FALSE    string     Adelie        
#> 10 penguins B3          3     2 cell      FALSE    boolean    TRUE          
#> # ℹ 18 more rows
#> # ℹ 20 more variables: base_value <chr>, numeric_value <dbl>,
#> #   currency_symbol <chr>, boolean_value <lgl>, date_value <chr>,
#> #   time_value <chr>, has_formula <lgl>, formula <chr>, has_error <lgl>,
#> #   error_type <dbl>, has_annotation <lgl>, annotation <chr>, is_merged <lgl>,
#> #   merge_colspan <dbl>, merge_rowspan <dbl>, merge_shape <chr>,
#> #   cell_style <chr>, row_style <chr>, col_style <chr>, …
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
#> # A tibble: 7 × 4
#>   x1        x2     x3               x4              
#>   <chr>     <chr>  <chr>            <chr>           
#> 1 species   female bill_length_mm   body_mass_g     
#> 2 Adelie    FALSE  40.3904109589041 4043.49315068493
#> 3 Adelie    TRUE   37.2575342465753 3368.83561643836
#> 4 Chinstrap FALSE  51.0941176470588 3938.97058823529
#> 5 Chinstrap TRUE   46.5735294117647 3527.20588235294
#> 6 Gentoo    FALSE  49.4737704918033 5484.83606557377
#> 7 Gentoo    TRUE   45.5637931034483 4679.74137931035
```

The `types` sheet shows examples of the different ODS data types:

``` r
types_cells <- read_ods_cells(example_file, "types", quiet = TRUE)

types_cells |> 
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::glimpse()
#> Rows: 150
#> Columns: 28
#> Groups: col [10]
#> $ sheet                  <chr> "types", "types", "types", "types", "types", "t…
#> $ address                <chr> "A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2",…
#> $ row                    <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3,…
#> $ col                    <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6…
#> $ cell_type              <chr> "cell", "cell", "cell", "cell", "cell", "cell",…
#> $ is_empty               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ value_type             <chr> "string", "boolean", "currency", "date", "time"…
#> $ cell_content           <chr> "Cell", "TRUE", "£1.20", "15/06/22", "13:24:56"…
#> $ base_value             <chr> "Cell", "true", "1.2", "2022-06-15", "PT13H24M5…
#> $ numeric_value          <dbl> NA, NA, 1.2000, NA, NA, NA, 12034.5679, 0.5467,…
#> $ currency_symbol        <chr> NA, NA, "GBP", NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ boolean_value          <lgl> NA, TRUE, NA, NA, NA, NA, NA, NA, NA, NA, NA, F…
#> $ date_value             <chr> NA, NA, NA, "2022-06-15", NA, "2022-06-15T13:24…
#> $ time_value             <chr> NA, NA, NA, NA, "PT13H24M56S", NA, NA, NA, NA, …
#> $ has_formula            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ formula                <chr> NA, NA, NA, NA, NA, NA, NA, NA, "of:=[.G2]*[.H2…
#> $ has_error              <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ error_type             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, 7, NA, NA, …
#> $ has_annotation         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ annotation             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Test c…
#> $ is_merged              <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ merge_colspan          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ merge_rowspan          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ merge_shape            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ cell_style             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ row_style              <chr> "ro1", "ro1", "ro1", "ro1", "ro1", "ro1", "ro1"…
#> $ col_style              <chr> "co6", "co7", "co8", "co9", "co10", "co11", "co…
#> $ col_default_cell_style <chr> "Default", "ce15", "ce4", "ce11", "ce21", "ce31…

types_cells |>
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::slice_head(n = 2) |>
  dplyr::select(-cell_type)
#> # A tibble: 20 × 27
#> # Groups:   col [10]
#>    sheet address   row   col is_empty value_type cell_content        base_value 
#>    <chr> <chr>   <dbl> <dbl> <lgl>    <chr>      <chr>               <chr>      
#>  1 types A2          2     1 FALSE    string     Cell                Cell       
#>  2 types A3          3     1 FALSE    string     Cell with comment   Cell with …
#>  3 types B2          2     2 FALSE    boolean    TRUE                true       
#>  4 types B3          3     2 FALSE    boolean    FALSE               false      
#>  5 types C2          2     3 FALSE    currency   £1.20               1.2        
#>  6 types C3          3     3 FALSE    currency   £1.20               1.2        
#>  7 types D2          2     4 FALSE    date       15/06/22            2022-06-15 
#>  8 types D3          3     4 FALSE    date       06/15/22            2022-06-15 
#>  9 types E2          2     5 FALSE    time       13:24:56            PT13H24M56S
#> 10 types E3          3     5 FALSE    time       13:24               PT13H24M56S
#> 11 types F2          2     6 FALSE    date       15/06/2022 13:24:56 2022-06-15…
#> 12 types F3          3     6 FALSE    date       15/06/22 13:24      2022-06-15…
#> 13 types G2          2     7 FALSE    float      12035               12034.56789
#> 14 types G3          3     7 FALSE    float      12034.57            12034.56789
#> 15 types H2          2     8 FALSE    float      0.5467              0.5467     
#> 16 types H3          3     8 FALSE    percentage 55%                 0.5467     
#> 17 types I2          2     9 FALSE    float      6579.3              6579.29826…
#> 18 types I3          3     9 FALSE    float      6579.3              6579.29826…
#> 19 types J2          2    10 FALSE    string     #N/A                #N/A       
#> 20 types J3          3    10 FALSE    string     #DIV/0!             #DIV/0!    
#> # ℹ 19 more variables: numeric_value <dbl>, currency_symbol <chr>,
#> #   boolean_value <lgl>, date_value <chr>, time_value <chr>, has_formula <lgl>,
#> #   formula <chr>, has_error <lgl>, error_type <dbl>, has_annotation <lgl>,
#> #   annotation <chr>, is_merged <lgl>, merge_colspan <dbl>,
#> #   merge_rowspan <dbl>, merge_shape <chr>, cell_style <chr>, row_style <chr>,
#> #   col_style <chr>, col_default_cell_style <chr>
```

## Performance

An ODS file is a zipped collection of XML files and associated files.
`{tidyods}`, like the `{readODS}` package, uses the
[`{xml2}`](https://xml2.r-lib.org) package to process this file.

The main aim of `{tidyods}` is to extract a large set of information
about cells, not just just their location and value, and while slower
than `{readODS}` for small and medium sized files users are unlikely to
see noticeable differences when using `read_ods_cells()`.

If your primary interest is just in cell location and cell values then
setting the argument `quick = TRUE` can result in a faster extraction
process as it provides only six columns: `sheet`, `address`, `row`,
`col`, `value_type` and `base_value`. This `quick` extraction process
varies in how the `base_value` column is constructed. When
`quick = TRUE` only float and percentage values are taken in their raw
numeric form, for all other types the `cell_content` is used. That means
that for currency, date and time value types the value as formatted for
spreadsheet application users is returned rather than the “raw” value
stored in the underlying XML file.

``` r
types_cells_quick <- read_ods_cells(example_file, "types",
                                    quick = TRUE, quiet = TRUE)

types_cells_quick |>
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::glimpse()
#> Rows: 150
#> Columns: 6
#> Groups: col [10]
#> $ sheet      <chr> "types", "types", "types", "types", "types", "types", "type…
#> $ address    <chr> "A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2", "I2", "J2",…
#> $ row        <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,…
#> $ col        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1…
#> $ value_type <chr> "string", "boolean", "currency", "date", "time", "date", "f…
#> $ base_value <chr> "Cell", "TRUE", "£1.20", "15/06/22", "13:24:56", "15/06/202…
```

Using the package’s example file we can see how setting `quick = TRUE`
results in performance that is nearly comaprable with that provided by
`{readODS}`.

``` r
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
  
#> # A tibble: 5 × 7
#>   expression       min   median     mean n_itr
#>   <bch:expr>  <bch:tm> <bch:tm> <bch:tm> <int>
#> 1 cells_quick   55.4ms   61.2ms   61.9ms    20
#> 2 cells_slow    83.3ms   90.5ms   91.5ms    20
#> 3 sheet_quick   62.8ms   69.7ms   72.4ms    20
#> 4 sheet_slow    93.3ms  101.1ms  102.9ms    20
#> 5 readODS       50.4ms   53.7ms   55.3ms    20
```

To test real-world performance we can use an ODS file published by the
UK Government on the [number of civil servants by
postcode](https://www.gov.uk/government/statistics/number-of-civil-servants-by-postcode-department-responsibility-level-and-leaving-cause-2021),
which contains a large sheet of 5,544 rows by 11 columns.

``` r
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

#> # A tibble: 5 × 7
#>   expression       min   median     mean n_itr
#>   <bch:expr>  <bch:tm> <bch:tm> <bch:tm> <int>
#> 1 cells_quick    16.3s    16.7s    16.7s     5
#> 2 cells_slow     21.7s      22s    22.8s     5
#> 3 sheet_quick    18.3s    19.9s    19.7s     5
#> 4 sheet_slow     20.7s    23.3s      23s     5
#> 5 readODS        13.7s    14.8s    14.6s     5
```

For this large sheet setting `quick = TRUE` delivers only a couple of
seconds slower extraction time compared to that provided by
`readODS::read_ods()`.

The dependency on `{xml2}` is likely to cause the function to fail/crash
when working with exceptionally large files. This is a limitation
inherited from the libxml2 C library that is used to power `{xml2}`,
which is
[documented](https://gitlab.gnome.org/GNOME/libxml2/-/wikis/Memory-management#general-memory-requirements)
as requiring available memory equal to around 4 times the size of the
file being processed. As the XML in an ODS is contained in a zip file
the system file size of an ODS file can easily hide the true
requirements of processing the file. Files are now checked for size and
operations cancelled if its estimated to exceed available memory.

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
