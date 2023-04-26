# Contributing to tidyods

This outlines how to propose a change to the `{tidyods}` R Package.

## Fixing typos
R help documentation, including the package's webiste, are created from [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html), you will need to edit these in the corresponding source `.R` file rather than `.Rd`

## Bigger changes
If you’ve found a bug, please file an issue that illustrates the bug, preferably with example code/example ODS file. If you want to add new functionality or propose improvements to the package code then please create an issue to discuss and explain your idea(s).

### Pull request process

* Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("mattkerlogue/tidyods", fork = TRUE)`.
* Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
* Create a Git branch for your pull request (PR), e.g. `new-feature` or `this-bug`.
* Make your changes, commit to git, and then create a PR, include `fixes #issue-number` in the title or description of the request.


### Code style

* New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
* Documentation is created using [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html).  
* We use [testthat](https://cran.r-project.org/package=testthat) for unit tests.

## Code of Conduct

Please note that the tidyods project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
