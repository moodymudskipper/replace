
<!-- README.md is generated from README.Rmd. Please edit that file -->

# replace

Choose a target, a replacement, files or folders, and whether you want
to replace standard variables, function calls, argument names, formal
names, or package names.

## Installation

You can install the development version of replace like so:

``` r
remotes::install_github("moodymudskipper/replace")
```

## Example

``` r
library(replace)
tmp <- tempfile(fileext = ".R")
code <- "
foo <- function(foo = foo) {
   foo(foo)
   foo::foo(foo = foo)
   lapply(foo, foo)
}
"
writeLines(code, tmp)

# by default we look at everything udner the R folder and replace variables and function calls
# below we do it every token type in succession

#tmp
replace_in_files("foo", "VAR", tmp, "var") # variable names
replace_in_files("foo", "FUN", tmp, "fun") # function calls
replace_in_files("foo", "ARG", tmp, "arg") # arguments
replace_in_files("foo", "FORMAL", tmp, "formal") # formals
replace_in_files("foo", "PACKAGE", tmp, "package") # packages
cat(readLines(tmp), sep = "\n")
#> 
#> VAR <- function(FORMAL = VAR) {
#>    FUN(VAR)
#>    PACKAGE::FUN(ARG = VAR)
#>    lapply(VAR, VAR)
#> }
```
