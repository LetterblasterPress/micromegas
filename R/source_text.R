#' Retrieve source text as a character vector
#'
#' This package includes the source text for *Micromégas* in various formats,
#' and this function simply reads the desired text file.
#'
#' Two formats are provided:
#'
#' - The `md` format provides the source text with minimal Markdown and no
#'   extraneous markup. (see `vignette("source_text_md")`)
#' - The `md+tex` format extends the `md` format with LaTeX commands to refine
#'   the resulting typeset. (see `vignette("source_text_md_tex")`)
#'
#' @param format which format to return, `md` or `md+tex` (see Details)
#'
#' @returns A character vector of *Micromégas* text in the requested format.
#'
#' @export
#'
#' @examples
#' head(source_text(), 8)
#'
#' head(source_text("md+tex"), 13)
source_text <- function(format = c("md", "md+tex")) {
  format <- match.arg(format)
  file_name <- c(
    "md" = "md.md",
    "md+tex" = "md_tex.md"
  )[format]

  readLines(inst("source_text", file_name))
}

#' A table of source text regular expressions
#'
#' This table contains reverse engineered regular expressions to indicate if a
#' line is indented and to match chapter headings & other formatted blocks.
#'
#' @format A [tibble][tibble::tibble-package] of regular expressions and logical indicators
#' \describe{
#'   \item{chapter}{logical indicating if line matches a chapter heading}
#'   \item{epigraph}{logical indicating if line matches a chapter epigraph}
#'   \item{quotation}{logical indicating if line matches a quotation}
#'   \item{closing}{logical indicating if line matches the closing line}
#'   \item{first_line}{logical indicating if line matches the first line of a regular paragraph}
#'   \item{first_line_chapter}{logical indicating if line matches the first line of a chapter}
#'   \item{indented}{logical indicating if line is indented}
#'   \item{regex}{a character vector of regular expressions for each line}
#' }
#'
#' @seealso `vignette("source_text_regexes")`
#'
#' @examples
#' str(source_text_regexes)
"source_text_regexes"
