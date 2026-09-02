parse_pdf_text <- function(x) {
  pdf_path <- proof_path(x)
  stopifnot(file_exists(pdf_path))

  rds_path <- path_ext_set(pdf_path, "rds")
  stopifnot(file_exists(rds_path))
  params <- tidy_params(readRDS(rds_path))

  # get text, tidy whitespace
  txt <- pdf_text(pdf_path) |>
    strsplit("\n") |>
    map(gsub, pattern = " +", replacement = " ") |>
    map(str_trim)

  # confirm there isn't any detritus from widows-and-orphans package
  stopifnot(!any(unlist(map(txt, grepl, pattern = "\\*"))))

  # drop title page if present
  if (params$title_page) {
    stopifnot(length(txt[[2]]) == 0L)
    txt <- tail(txt, -2)
  }

  # check page numbers, flatten
  txt <- txt |>
    imap_dfr(function(x, i) {
      stopifnot(identical(i, as.integer(tail(x, 1))))
      x <- head(x, -2)
      tibble(
        page = i,
        line = if_else(
          cumsum(grepl("^Chapter [IV]+$", x, ignore.case = TRUE)) > 0,
          seq(
            params$text_height_lines - length(x) + 1,
            params$text_height_lines
          ),
          seq_along(x)
        ),
        text = x
      )
    })

  # confirm all source regexes are matched as expected
  source_text_regexes <- micromegas::source_text_regexes
  regex_counts <- source_text_regexes |>
    mutate(
      match_count = source_text_regexes$regex |>
        map(grepl, x = txt$text) |>
        map_int(sum),
      match_count_expected = 1
    )
  regex_counts$match_count_expected <- regex_counts$match_count_expected +
    (
      params$reflow_epi_3 &
        grepl("Voyage of the two inhabitants", regex_counts$regex)
    ) +
    (
      params$reflow_epi_5 &
        grepl("Experiments and reasonings", regex_counts$regex)
    )
  stopifnot(all(regex_counts$match_count == regex_counts$match_count_expected))

  # use regexes to flag special lines/environments
  y <- tibble(
    txt,
    map_dfr(txt$text, function(x) {
      y <- tibble(text = x, source_text_regexes)
      y <- y[map2_lgl(y$regex, y$text, grepl), ]
      y <- select(y, where(is.logical))
      if (nrow(y) == 0L) {
        y <- as_tibble(map(y, ~FALSE))
      }
      return(y)
    })
  )
  stopifnot(identical(txt, select(y, names(txt))))

  # tidy text
  y$text <- if_else(y$chapter, toupper(y$text), y$text)
  y$text <- if_else(y$closing, str_to_sentence(y$text), y$text)
  y$text <- y$text |>
    gsub(pattern = "micromegas", replacement = "Micromegas") |>
    sub(pattern = "^O\\s*(n one|N ONE)\\b", replacement = "On one") |>
    gsub(pattern = "\\b([iI]{2})\\b", replacement = "II") |>
    gsub(pattern = " *([:;!?])", replacement = "\\1") |>
    str_trim()

  # flag the last line of each paragraph
  y$last_line <- (
    !y$chapter & !y$epigraph & !y$quotation & !y$closing & y$text != "" &
      (
        lead(y$first_line) %in% TRUE |
          lead(y$quotation) %in% TRUE |
          lead(y$text) %in% ""
      )
  )

  relocate(y, "text", .after = everything())
}
