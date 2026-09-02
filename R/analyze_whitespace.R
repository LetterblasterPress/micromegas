analyze_whitespace <- function(x, verbose = FALSE) {
  rds_path <- path_ext_set(proof_path(x), "rds")
  stopifnot(file_exists(rds_path))
  params <- tidy_params(readRDS(rds_path))

  y <- parse_pdf_text(x)
  y <- y[!y$text %in% "", ]

  y$tex_text <- y$text |>
    sub(
      pattern = "^On one\\b",
      replacement = "O\\\\hspace{11pt plus 1pt minus 2pt}{\\\\scshape{}n one}"
    ) |>
    sub(
      pattern = "\\bthe Mediterranean\\b",
      replacement = "\\\\emph{the Mediterranean}"
    ) |>
    sub(
      pattern = "pond we call the$",
      replacement = "pond we call \\\\emph{the}"
    ) |>
    sub(
      pattern = "^Mediterranean\b",
      replacement = "\\\\emph{Mediterranean}"
    ) |>
    sub(
      pattern = "we call the Mediter-$",
      replacement = "we call \\\\emph{the Mediter-}"
    ) |>
    sub(
      pattern = "^ranean\b",
      replacement = "\\\\emph{ranean}"
    ) |>
    sub(
      pattern = "\\bOcean\\b",
      replacement = "\\\\emph{Ocean}"
    ) |>
    sub(
      pattern = "\\bOn the Soul\\b",
      replacement = "\\\\emph{On the Soul}"
    ) |>
    sub(
      pattern = "Aristotle, \\bOn the$",
      replacement = "Aristotle, \\\\emph{On the}"
    ) |>
    sub(
      pattern = "^Soul\\b",
      replacement = "\\\\emph{Soul}"
    ) |>
    sub(
      pattern = "Aristotle, \\bOn$",
      replacement = "Aristotle, \\\\emph{On}"
    ) |>
    sub(
      pattern = "^the Soul\\b",
      replacement = "\\\\emph{the Soul}"
    ) |>
    sub(
      pattern = "\\bSumma\\b",
      replacement = "\\\\emph{Summa}"
    ) |>
    sub(
      pattern = "\\b([iI]{2})\\b",
      replacement = "\\\\allsc{II}"
    )

  y$tex_text <- if_else(
    !params$loose_headings & y$chapter,
    sprintf("\\allsc{%s}", y$tex_text),
    y$tex_text
  )

  y$tex_text <- if_else(
    y$epigraph | y$quotation,
    "{\\itshape\\addfontfeature{Ligatures=Discretionary} %s}" |>
      sprintf(y$tex_text),
    y$tex_text
  )

  y$tex_text <- if_else(
    y$closing,
    sprintf("\\allsc{%s}", y$tex_text),
    y$tex_text
  )

  # make another column of TeX text with spaces removed
  y$tex_text_nospace <- y$tex_text |>
    gsub(pattern = " ", replacement = "") |>
    sub(pattern = "11ptplus1ptminus2pt", replacement = "11pt plus 1pt minus 2pt")

  # typeset TeX text to determine natural width & width without spaces
  y$natural_width <- y$tex_text
  y$nospace_width <- y$tex_text_nospace
  y <- select(y, -any_of(starts_with("tex_text")))
  y <- mutate(y, across(ends_with("_width"), function(x) {
    this_src <- sprintf("\\setlength{\\sw}{\\widthof{%s}}\\the\\sw", x) |>
      paste(collapse = "\n\n")

    these_params <- params
    these_params$title_page <- FALSE
    these_params$page_width <- 1.5 * these_params$page_width
    these_params$page_height <- 1.5 * these_params$page_height
    these_params$`header-includes` <- paste(collapse = "\n", c(
      these_params$`header-includes`,
      "\\usepackage{calc}",
      "\\newlength{\\sw}"
    ))
    these_params <- tidy_params(these_params)

    this_proof <- suppressMessages(typeset(src = this_src, params = these_params))

    proof_path(this_proof$id) |>
      pdf_text() |>
      strsplit("\\s+") |>
      unlist() |>
      keep(grepl, pattern = "^\\d*\\.?\\d+pt$") |>
      sub(pattern = "pt", replacement = "") |>
      as.numeric()
  }))

  # deduce expected width
  y$expected_width <- 72.27 * params$text_width - y$indented * params$parindent
  y$expected_width[y$chapter | y$epigraph | y$quotation | y$closing] <- NA
  y$expected_width[y$last_line] <- NA
  y$expected_width <- coalesce(y$expected_width, y$natural_width)

  # estimate word space
  y$total_space_expected <- y$natural_width - y$nospace_width
  y$total_space_actual <- y$expected_width - y$nospace_width
  y$word_space_count <- map_int(strsplit(y$text, "\\s+"), length) - 1L
  y <- y[y$word_space_count > 0, ]
  y$word_space_expected <- y$total_space_expected / y$word_space_count
  y$word_space_actual <- y$total_space_actual / y$word_space_count

  if (!verbose) {
    y <- tibble(
      word_space_mean = mean(y$word_space_actual),
      word_space_sd = sd(y$word_space_actual),
      word_space_rmse = sqrt(weighted.mean(
        x = (y$word_space_actual - y$word_space_expected)^2,
        w = y$word_space_count
      )),
      word_space_min = min(y$word_space_actual),
      word_space_p01 = as.numeric(quantile(y$word_space_actual, probs = 0.01)),
      word_space_p05 = as.numeric(quantile(y$word_space_actual, probs = 0.05)),
      word_space_p95 = as.numeric(quantile(y$word_space_actual, probs = 0.95)),
      word_space_p99 = as.numeric(quantile(y$word_space_actual, probs = 0.99)),
      word_space_max = max(y$word_space_actual)
    )
    y$word_space_lb <- y$word_space_mean - qnorm(0.975) * y$word_space_sd /
      sqrt(nrow(y))
    y$word_space_ub <- y$word_space_mean + qnorm(0.975) * y$word_space_sd /
      sqrt(nrow(y))
  }

  return(y)
}
