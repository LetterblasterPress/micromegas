#' Retrieve typesetting assets as character vectors
#'
#' @returns A character vector with the requested asset
#'
#' @examples
#' head(typesetting_metadata(), 2)
#'
#' head(typesetting_template(), 2)
#'
#' @name typesetting_assets
NULL

#' @rdname typesetting_assets
#' @export
typesetting_metadata <- function() {
  readLines(inst("typesetting_metadata.yml"))
}

#' @rdname typesetting_assets
#' @export
typesetting_template <- function() {
  readLines(inst("typesetting_template.latex"))
}
