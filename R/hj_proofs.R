#' Typesetting parameters for optimal hyphenation & justification
#'
#' This table contains typesetting parameters and resulting word-space metrics
#' for proofs with the best hyphenation & justification. These proofs rendered
#' without error, are free of overfull or underfull lines, and have no more
#' than three consecutive lines that end with a hyphenated word.
#'
#' @format A [tibble][tibble::tibble-package]
#' \describe{
#'   \item{id}{typeset ID}
#'   \item{draft--ch7post}{typesetting parameters, see `tidy_params()`}
#'   \item{word_space_mean}{word space average}
#'   \item{word_space_min, word_space_max}{word space range}
#'   \item{word_space_p01--word_space_p99}{word space quantiles}
#'   \item{word_space_sd}{word space standard deviation}
#'   \item{word_space_lb, word_space_ub}{word space 95% confidence interval}
#'   \item{word_space_rmse}{word space root mean squared error}
#' }
#'
#' @seealso `vignette("hj_optimization")`
#'
#' @examples
#' str(hj_proofs)
#'
#' # Note that one layout candidate has no acceptable solution
#' hj_proofs |>
#'   tidy_params() |>
#'   dplyr::select(any_of(names(layout_candidates))) |>
#'   dplyr::anti_join(x = layout_candidates) |>
#'   dplyr::mutate(across(where(is.factor), as.character)) |>
#'   str()
"hj_proofs"
