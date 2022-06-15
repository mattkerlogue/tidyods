
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyods

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
#> Unzipping ODS file
#> Reading XML file
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
#> Getting ODS sheet
#> Unzipping ODS file
#> Getting ODS sheetReading XML file
#> Getting ODS sheetProcessing rows...

penguin_sheet
#> # A tibble: 7 × 4
#>   X1        X2     X3             X4         
#>   <chr>     <chr>  <chr>          <chr>      
#> 1 species   female bill_length_mm body_mass_g
#> 2 Adelie    FALSE  40.4           4043       
#> 3 Adelie    TRUE   37.3           3369       
#> 4 Chinstrap FALSE  51.1           3939       
#> 5 Chinstrap TRUE   46.6           3527       
#> 6 Gentoo    FALSE  49.5           5485       
#> 7 Gentoo    TRUE   45.6           4680
```

While the `types` sheet shows examples of the different ODS data types:

``` r
types_cells <- read_ods_cells(example_file, "types")
#> Getting ODS sheet
#> Unzipping ODS file
#> Getting ODS sheetReading XML file
#> Getting ODS sheetProcessing rows...

types_cells |> 
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::glimpse()
#> Rows: 90
#> Columns: 8
#> Groups: col [9]
#> $ row             <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ col             <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, …
#> $ cell_type       <chr> "cell", "cell", "cell", "cell", "cell", "cell", "cell"…
#> $ value_type      <chr> "string", "boolean", "currency", "date", "time", "date…
#> $ cell_formula    <chr> NA, NA, NA, NA, NA, NA, NA, NA, "of:=[.G2]*[.H2]", NA,…
#> $ cell_content    <chr> "Cat", "TRUE", "£1.20", "15/06/22", "13:24:56", "15/06…
#> $ base_value      <chr> "Cat", "true", "1.2", "2022-06-15", "PT13H24M56S", "20…
#> $ currency_symbol <chr> NA, NA, "GBP", NA, NA, NA, NA, NA, NA, NA, NA, "GBP", …

types_cells |>
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::slice_head(n = 2) |>
  dplyr::select(-cell_type)
#> # A tibble: 18 × 7
#> # Groups:   col [9]
#>      row   col value_type cell_formula   cell_content base_value currency_symbol
#>    <int> <int> <chr>      <chr>          <chr>        <chr>      <chr>          
#>  1     2     1 string     <NA>           Cat          Cat        <NA>           
#>  2     3     1 string     <NA>           Dog          Dog        <NA>           
#>  3     2     2 boolean    <NA>           TRUE         true       <NA>           
#>  4     3     2 boolean    <NA>           FALSE        false      <NA>           
#>  5     2     3 currency   <NA>           £1.20        1.2        GBP            
#>  6     3     3 currency   <NA>           £1.20        1.2        GBP            
#>  7     2     4 date       <NA>           15/06/22     2022-06-15 <NA>           
#>  8     3     4 date       <NA>           06/15/22     2022-06-15 <NA>           
#>  9     2     5 time       <NA>           13:24:56     PT13H24M5… <NA>           
#> 10     3     5 time       <NA>           13:24        PT13H24M5… <NA>           
#> 11     2     6 date       <NA>           15/06/2022 … 2022-06-1… <NA>           
#> 12     3     6 date       <NA>           15/06/22 13… 2022-06-1… <NA>           
#> 13     2     7 float      <NA>           12035        12034.567… <NA>           
#> 14     3     7 float      <NA>           12034.57     12034.567… <NA>           
#> 15     2     8 float      <NA>           0.5467       0.5467     <NA>           
#> 16     3     8 percentage <NA>           55%          0.5467     <NA>           
#> 17     2     9 float      of:=[.G2]*[.H… 6579.3       6579.2982… <NA>           
#> 18     3     9 float      of:=[.G3]*[.H… 6579.3       6579.2982… <NA>
```

## Performance

An ODS file is a zipped collection of XML files and associated files.
`{tidyods}`, like the `{readODS}` package, uses the
[xml2](https://xml2.r-lib.org) package to process this file. There are
three likely sources of performance issues: downloading of remote files,
unzipping the ODS file, processing rows.

With default settings, performance of `{tidyods}` functions is slower
than `{readODS}`, however `{tidyods}` provides console messages and
progress bars to the user.

Performance can be improved by setting `quick = TRUE` in
`read_ods_cells()` and `read_ods_sheet()`, this will extract only a
simple text representation of the cell, i.e. it will not provide
information on cell or value types, formulas or the underlying base
value for numbers, dates or times. This method also ignores replicated
white space characters.

``` r
types_cells_quick <- read_ods_cells(example_file, "types", quick = TRUE)
#> Getting ODS sheet
#> Unzipping ODS file
#> Getting ODS sheetReading XML file
#> Getting ODS sheetProcessing rows...

types_cells_quick |>
  dplyr::filter(row > 1) |>
  dplyr::group_by(col) |>
  dplyr::glimpse()
#> Rows: 90
#> Columns: 3
#> Groups: col [9]
#> $ row          <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, …
#> $ col          <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, …
#> $ cell_content <chr> "Cat", "TRUE", "£1.20", "15/06/22", "13:24:56", "15/06/20…
```

To test performance we will use an ODS file published by the UK
Government on the [number of civil servants by
postcode](https://www.gov.uk/government/statistics/number-of-civil-servants-by-postcode-department-responsibility-level-and-leaving-cause-2021)
(5,544 rows by 11 columns). Analysis of read times indicates that using
the `quick = TRUE` setting for `read_ods_cells()` results in a read that
is 10 times faster than the full cell extraction method and around 1.5
times faster than reading the sheet with `{readODS}`.

``` r
microbenchmark::microbenchmark(
  "tidyods_slow" = tidyods::read_ods_cells(test_ods, "Staff_in_post", 
    quick = FALSE),
  "tidyods_quick" = tidyods::read_ods_cells(test_ods, "Staff_in_post", 
    quick = TRUE),
  "readODS" = readODS::read_ods(test_ods, "Staff_in_post"),
  times = 3
)

#> Unit: seconds
#>          expr     min      lq    mean  median      uq     max neval
#>  tidyods_slow 35.4833 35.7546 35.8626 36.0259 36.0523 36.0786     3
#> tidyods_quick  3.2470  3.2584  3.2935  3.2697  3.3167  3.3638     3
#>       readods  5.1141  5.1606  5.2050  5.2072  5.2505  5.2939     3
```

A cursory test of a huge file that is known to be slow/non-performative
with {readODS} shows promise.

Performance issues are tracked and discussed in [issue
\#3](https://github.com/mattkerlogue/tidyods/issues/3) in the package’s
GitHub repository.

However, if performance is a major determinant of your workflow
(i.e. you have very large files), its likely more sensible to open the
file in a spreadsheet application, save the file as a `.xslx` format
file and read using `{tidyxl}`.

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
