# Find paths to package files (internal function)

R packages may include arbitrary files such as drivers and templates
that are installed alongside the code itself. This function is a simple
wrapper to
[`base::system.file()`](https://rdrr.io/r/base/system.file.html) to find
paths for this package.

## Usage

``` r
inst(...)
```

## Arguments

- ...:

  character vectors, specifying subdirectory and file(s) within this
  package. The default, none, returns the root of the package. Wildcards
  are not supported.

## Value

Returns a character vector of file paths that matched `...` or an empty
string if none matched.

## Examples

``` r
# Where is the custom dictionary for this installation?
micromegas:::inst("WORDLIST")
#> [1] "/home/runner/work/_temp/Library/micromegas/WORDLIST"
```
