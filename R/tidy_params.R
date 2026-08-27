#' Tidy typesetting parameters
#'
#' This function prepares a data frame of typesetting parameters in a consistent
#' format that can be passed to downstream rendering functions. It accepts named
#' parameters, a list of parameters, or a data frame of parameters.
#'
#' @param ...
#'        accepts a list or a data frame of parameter values instead of passing
#'        named parameters individually
#' @param draft
#'        logical, passed to LaTeX document class options
#' @param `header-includes`
#'        character vector of additional LaTeX setup code
#' @param title_page
#'        logical indicating whether to typeset a title page or not
#' @param page_height,page_width
#'        page dimensions (inches)
#' @param text_height_lines
#'        textblock line count
#' @param text_width
#'        textblock width (inches)
#' @param t_mar,i_mar
#'        top & inner margins (inches)
#' @param leading
#'        baseline skip (points)
#' @param ws,ws_stretch,ws_shrink
#'        LaTeX word space parameters
#' @param pretolerance,tolerance,hfuzz,emergencystretch
#'        LaTeX typesetting parameters
#' @param hyphenpenalty,exhyphenpenalty,doublehyphendemerits,interlinepenalty,clubpenalty,widowpenalty,brokenpenalty
#'        LaTeX penalties
#' @param parindent
#'        paragraph indentation (points)
#' @param hyphenate_responded
#'        logical indicating whether to accept non-standard hyphenation of the
#'        word `re-spon-ded`
#' @param reflow_epi_3,reflow_epi_5
#'        logical indicating whether to break epigraphs for chapters 3 & 5
#' @param aristotle_space
#'        blank lines to add above & below Aristotle quotation
#' @param closing_space
#'        blank lines before closing environment
#' @param loose_headings
#'        logical indicating whether to set loose or compact chapter headings
#' @param ch1pre,ch2pre,ch3pre,ch4pre,ch5pre,ch6pre,ch7pre
#'        additional blank lines before each chapter
#' @param ch1post,ch2post,ch3post,ch4post,ch5post,ch6post,ch7post
#'        additional blank lines after each chapter epigraph
#'
#' @returns a [tibble][tibble::tibble-package] of parameters
#'
#' @export
#'
#' @examples
#' # can pass arguments directly
#' tidy_params(
#'   page_height = 10, page_width = 8,
#'   text_height_lines = 42, text_width = 5,
#'   t_mar = 0.5, i_mar = 0.5
#' )
#'
#' # or a data frame of parameters (extra columns ignored)
#' tidy_params(layout_candidates) |> str()
tidy_params <- function(
  ...,
  draft = TRUE,
  `header-includes` = "",
  title_page = !draft,
  page_height,
  page_width,
  text_height_lines,
  text_width,
  t_mar,
  i_mar,
  leading = 12,
  ws = 1.001327,
  ws_stretch = 0.552792,
  ws_shrink = 0.921620,
  pretolerance = 100,
  tolerance = 200,
  hfuzz = 0,
  emergencystretch = 0,
  hyphenpenalty = 50,
  exhyphenpenalty = 50,
  doublehyphendemerits = 1e4,
  interlinepenalty = 5,
  clubpenalty = 150,
  widowpenalty = 150,
  brokenpenalty = 100,
  parindent = 12,
  hyphenate_responded = FALSE,
  reflow_epi_3 = FALSE,
  reflow_epi_5 = FALSE,
  aristotle_space = 0,
  closing_space = 0,
  loose_headings = FALSE,
  ch1pre = 2,
  ch2pre = 1,
  ch3pre = 1,
  ch4pre = 1,
  ch5pre = 1,
  ch6pre = 1,
  ch7pre = 1,
  ch1post = 1,
  ch2post = 1,
  ch3post = 1,
  ch4post = 1,
  ch5post = 1,
  ch6post = 1,
  ch7post = 1
) {
  dots <- list(...)
  if (length(dots) == 1L && is.data.frame(dots[[1]])) {
    if (nrow(dots[[1]]) == 1L) {
      do.call(tidy_params, dots[[1]])
    } else {
      map_dfr(split(dots[[1]], seq(nrow(dots[[1]]))), tidy_params)
    }
  } else {
    tibble(
      draft,
      `header-includes`,
      title_page,
      page_height,
      page_width,
      text_height_lines,
      text_width,
      t_mar,
      i_mar,
      leading,
      ws,
      ws_stretch,
      ws_shrink,
      pretolerance,
      tolerance,
      hfuzz,
      emergencystretch,
      hyphenpenalty,
      exhyphenpenalty,
      doublehyphendemerits,
      interlinepenalty,
      clubpenalty,
      widowpenalty,
      brokenpenalty,
      parindent,
      hyphenate_responded,
      reflow_epi_3,
      reflow_epi_5,
      aristotle_space,
      closing_space,
      loose_headings,
      ch1pre,
      ch2pre,
      ch3pre,
      ch4pre,
      ch5pre,
      ch6pre,
      ch7pre,
      ch1post,
      ch2post,
      ch3post,
      ch4post,
      ch5post,
      ch6post,
      ch7post
    )
  }
}
