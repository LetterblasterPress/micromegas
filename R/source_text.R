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
