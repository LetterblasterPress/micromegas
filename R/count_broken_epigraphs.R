count_broken_epigraphs <- function(x) {
  regexes <- micromegas::source_text_regexes
  regexes <- regexes[regexes$epigraph, ]$regex

  y <- expand_grid(
    x = proof_path(x) |>
      pdf_text() |>
      strsplit("\n") |>
      map(str_trim) |>
      map_chr(head, n = 1),
    re = regexes
  )

  sum(map2_lgl(y$re, y$x, grepl))
}
