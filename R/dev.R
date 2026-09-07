#' Find paths to package files
#'
#' R packages may include arbitrary files such as drivers and templates that are
#' installed alongside the code itself. This function is a simple wrapper to
#' [base::system.file()] to find paths for this package.
#'
#' @param ... character vectors, specifying subdirectory and file(s) within this
#'   package. The default, none, returns the root of the package. Wildcards are
#'   not supported.
#'
#' @return Returns a character vector of file paths that matched `...` or an
#'   empty string if none matched.
#'
#' @examples
#' # Where is the custom dictionary for this installation?
#' micromegas:::inst("WORDLIST")
inst <- function(...) {
  system.file(..., package = "micromegas")
}

#' Open an R file and its corresponding test file
#'
#' An opinionated wrapper function that calls [usethis::use_r()] and
#' [usethis::use_test()].
#'
#' @param name file name, without extension or "test-" prefix
#'
#' @return Returns `TRUE` invisibly upon success.
dev <- function(name) {
  invisible(usethis::use_r(name) && usethis::use_test(name))
}

#' Run code quality checks
#'
#' This function runs a sequence of code quality checks. You are encouraged to
#' run this function as you develop, but at a minimum you should run it before
#' pushing code changes.
#'
#' @return Returns `TRUE` invisibly upon success.
#' @seealso [dev_check_helpers]
dev_check <- function() {
  y <- c()

  message("\nTidying code...")
  y <- c(y, dev_lint())

  message("\nRebuilding docs...")
  y <- c(y, dev_document())

  message("\nChecking spelling...")
  y <- c(y, dev_spell_check())

  message("\nRebuilding package site...")
  y <- c(y, dev_build_site())

  message("\nChecking package...")
  y <- c(y, devtools::check(
    document = FALSE,
    cran = TRUE,
    remote = FALSE,
    force_suggests = TRUE,
    args = "--force-multiarch"
  )$status == 0)

  return(invisible(all(y)))
}

#' Helper functions called by `dev_check()`
#'
#' Learn more about what [dev_check()] does.
#'
#' @details
#'
#' [dev_check()] is a convenience wrapper that runs the following checks in
#' sequence:
#'
#'  * [dev_lint()] tidies source code with [styler::style_pkg()].
#'  * [dev_document()] updates package documentation and vignettes with calls
#'    to [devtools::document()] and [usethis::use_tidy_description()].
#'  * [dev_spell_check()] checks spelling of package documentation and vignettes
#'    with calls to [spelling::spell_check_package()] and
#'    [spelling::update_wordlist()].
#'  * [dev_build_site()] builds the documentation site with a call to
#'    [pkgdown::build_site()], then loads a preview in the RStudio Viewer pane.
#'
#' ## Spell checking
#'
#' [dev_spell_check()] first checks package documentation and vignettes for
#' spelling errors. If no spelling errors are found, the custom dictionary is
#' re-sorted, deduplicated, and purged of unused words. If spelling errors are
#' found, a user confirmation is required before updating the custom dictionary;
#' otherwise an error is thrown.
#'
#' @seealso [spelling::wordlist]
#'
#' @references <https://pkgdown.r-lib.org>
#'
#' @return Each function returns `TRUE` invisibly upon success.
#'
#' @name dev_check_helpers
NULL

#' @rdname dev_check_helpers
dev_lint <- function() {
  styler::style_pkg() # defaults to tidyverse_style

  return(invisible(TRUE))
}

#' @rdname dev_check_helpers
dev_document <- function() {
  o <- capture.output(suppressMessages(
    devtools::document(
      roclets = c("rd", "collate", "namespace", "vignette"),
      quiet = TRUE
    )
  ))

  o <- capture.output(suppressMessages(
    usethis::use_tidy_description()
  ))

  return(invisible(TRUE))
}

#' @rdname dev_check_helpers
dev_spell_check <- function() {
  y <- spelling::spell_check_package()

  if (nrow(y) == 0L) {
    o <- capture.output(spelling::update_wordlist(confirm = FALSE))
    rm(o)
    return(invisible(TRUE))
  } else {
    warning(
      "Spelling errors found:",
      call. = FALSE, immediate. = TRUE, noBreaks. = TRUE
    )
    print(y)

    if (readline("Type 'yes' to update wordlist: ") == "yes") {
      spelling::update_wordlist(confirm = FALSE)
      return(invisible(TRUE))
    } else {
      stop("Please resolve spelling errors.")
    }
  }
}

#' @rdname dev_check_helpers
dev_build_site <- function() {
  pkgdown::build_site(preview = FALSE)

  if (interactive()) {
    d <- tryCatch(
      yaml::read_yaml("_pkgdown.yml")$destination,
      error = function(e) "docs"
    )

    if (is.null(d)) d <- "docs"

    as.character(d) |>
      dir_copy(file_temp()) |>
      path("index", ext = "html") |>
      rstudioapi::viewer()
  }

  return(invisible(TRUE))
}
