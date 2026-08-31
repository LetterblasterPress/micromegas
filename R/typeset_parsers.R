parse_tex_log <- function(x) {
  x |>
    mutate(
      exceptions = map(log, function(x) {
        subset(
          x,
          grepl("warn|error", x, ignore.case = TRUE) &
            !grepl("Package: infwarerr .* Providing info/warning/error messages", x) &
            !grepl("Package hyperref Warning: Draft mode on.", x) &
            !grepl("The material used in the headers is too large", x) &
            !grepl("Package widows-and-orphans", x)
        )
      }),
      overfulls = map(log, function(x) {
        tibble(log = x, overfull = cumsum(grepl("Overfull", log))) |>
          filter(overfull > 0) |>
          slice(1:3, .by = overfull) |>
          summarise(
            .by = overfull,
            log = sprintf("`%s`", paste(log, collapse = "\\\\"))
          ) |>
          filter(!grepl(" \\\\vbox ", log)) |>
          transmute(
            amount = as.numeric(sub("^.+?([0-9\\.]+)pt .+?$", "\\1", log)),
            log = "`Overfull \\\\hbox \\([0-9\\.]+pt too wide\\) in paragraph at " |>
              sub("", log)
          )
      }),
      underfulls = map(log, function(x) {
        tibble(log = x, underfull = cumsum(grepl("Underfull", log))) |>
          filter(underfull > 0) |>
          slice(1:4, .by = underfull) |>
          summarise(
            .by = underfull,
            log = sprintf("`%s`", paste(log, collapse = "\\\\"))
          ) |>
          transmute(
            amount = as.numeric(sub("^.+?\\(badness ([0-9\\.]+)\\) .+?$", "\\1", log)),
            log
          )
      }),
      # widows_and_orphans = map(log, function(x) {
      #   # FIXME check for "Detecting widows and orphans" to confirm actually loaded
      #   subset(
      #     x,
      #     grepl("warn|error", x, ignore.case = TRUE) &
      #       grepl("Package widows-and-orphans", x)
      #   )
      # }),
      exception_count = map_int(exceptions, ~ coalesce(length(.), 0)),
      overfull_count = map_int(overfulls, ~ coalesce(nrow(.), 0)),
      overfull_total = map_dbl(overfulls, ~ coalesce(sum(.$amount), 0)),
      underfull_count = map_int(underfulls, ~ coalesce(nrow(.), 0)),
      underfull_total = map_dbl(underfulls, ~ coalesce(sum(.$amount), 0))
      # w_o_h_total = map_int(widows_and_orphans, ~ coalesce(length(.), 0)),
      # widow_orphan_counts = map(widows_and_orphans, function(i) {
      #   if (length(i) == 0L || all(i %in% c(NA, ""))) {
      #     return(tibble(
      #       orphan_widow_recto = 0,
      #       orphan_widow_verso = 0,
      #       widow_verso = 0,
      #       widow_recto = 0,
      #       orphan_recto = 0,
      #       orphan_verso = 0,
      #       hyphen_recto = 0,
      #       hyphen_verso = 0
      #     ))
      #   } else {
      #     # NOTE: package defines orphan at bottom of page, widow at top
      #     tibble(
      #       i,
      #       page = as.integer(sub("^.*page (\\d+).*$", "\\1", i)),
      #       recto = (page %% 2) == 1,
      #       verso = (page %% 2) == 0,
      #       orphan_widow = grepl(" and", i),
      #       orphan = !orphan_widow & grepl("Orphan", i),
      #       widow = grepl("Widow", i),
      #       hyphen = grepl("Hyphen", i)
      #     ) |>
      #       summarise(
      #         orphan_widow_recto = 2 * sum(orphan_widow & recto),
      #         orphan_widow_verso = 2 * sum(orphan_widow & verso),
      #         widow_verso = sum(widow & verso),
      #         widow_recto = sum(widow & recto),
      #         orphan_recto = sum(orphan & recto),
      #         orphan_verso = sum(orphan & verso),
      #         hyphen_recto = sum(hyphen & recto),
      #         hyphen_verso = sum(hyphen & verso)
      #       )
      #   }
      # })
    ) # |>
  # unnest(widow_orphan_counts) |>
  # mutate(
  #   w_o_h_total = (
  #     orphan_widow_recto +
  #       orphan_widow_verso +
  #       widow_verso +
  #       widow_recto +
  #       orphan_recto +
  #       orphan_verso +
  #       hyphen_recto +
  #       hyphen_verso
  #   )
  # )
}

estimate_type_usage <- function(x) {
  pdftools::pdf_text(x) |>
    imap_dfr(~ tibble(page = .y, text = .x)) |>
    mutate(
      text = text |>
        gsub(pattern = "ffi", replacement = "ﬃ") |>
        gsub(pattern = "ffl", replacement = "ﬄ") |>
        gsub(pattern = "ff", replacement = "ﬀ") |>
        gsub(pattern = "fi", replacement = "ﬁ") |>
        gsub(pattern = "fl", replacement = "ﬂ") |>
        gsub(pattern = "ae", replacement = "æ") |>
        # gsub(pattern = "oe", replacement = "œ") |> # none in text
        gsub(pattern = "—", replacement = "---") |>
        gsub(pattern = "–", replacement = "--") |>
        gsub(pattern = "…", replacement = "...") |>
        gsub(pattern = "é", replacement = "e") |>
        gsub(pattern = "ë", replacement = "e") |>
        map(~ as.data.frame(table(unlist(strsplit(., "")))))
    ) |>
    unnest(text) |>
    filter(!Var1 %in% c("\n", " ")) |>
    group_by(page, char = Var1) |>
    summarise(count = sum(Freq), .groups = "drop") |>
    mutate(
      char = char |>
        sub(pattern = "^ﬃ$", replacement = "ffi") |>
        sub(pattern = "^ﬄ$", replacement = "ffl") |>
        sub(pattern = "^ﬀ$", replacement = "ff") |>
        sub(pattern = "^ﬁ$", replacement = "fi") |>
        sub(pattern = "^ﬂ$", replacement = "fl") |>
        sub(pattern = "^æ$", replacement = "ae") |>
        sub(pattern = "^‘$", replacement = "`") |>
        sub(pattern = "^’$", replacement = "'") |>
        sub(pattern = "^“$", replacement = "``") |>
        sub(pattern = "^”$", replacement = "''")
    ) |>
    inner_join(
      readRDS(inst("centaur.rds")) |>
        filter(font == "Centaur 11/12", case %in% c("upper", "lower")) |>
        transmute(
          char = if_else(
            case %in% "upper",
            toupper(as.character(glyph)),
            tolower(as.character(glyph))
          ),
          sort_count = count
        ),
      by = "char"
    ) |>
    group_by(page) |>
    summarise(pct = max(count / sort_count)) |>
    with(max(pct))
}

count_hyphen_runs <- function(x, verbose = FALSE) {
  y <- pdftools::pdf_text(x) |>
    strsplit("\n") |>
    map(~ if_else(grepl("-$", .), "-", " ")) |>
    map_chr(paste, collapse = "") |>
    strsplit("\\s+") |>
    imap_dfr(~ tibble(page = as.integer(.y), hyphens = .x))

  if (verbose) {
    y <- y |>
      filter(hyphens != "") |>
      summarise(.by = everything(), count = n()) |>
      arrange(across(everything()))
  } else {
    y <- sum(nchar(y$hyphens) > 3)
  }
  return(y)
}

# x <- "a4897c979d685472a9da1d39520e2c07"
analyze_whitespace <- function(x, verbose = FALSE) {
  pdf_path <- proof_path(x)
  stopifnot(file_exists(pdf_path))

  rds_path <- path_ext_set(pdf_path, "rds")
  stopifnot(file_exists(rds_path))
  params <- tidy_params(readRDS(rds_path))

  id <- path_ext_remove(path_file(pdf_path))

  text <- extract_body_text(id) |>
    dplyr::filter(!chapter, !epigraph, !quotation, !closing, !text %in% "") |>
    dplyr::filter(!lead(first_line) %in% c(NA, TRUE)) |>
    select(page, line, indented, text, tex_text) |>
    mutate(
      typeset_width = tex_text,
      typeset_width_nospace = gsub("\\s+", "", tex_text),
      across(
        starts_with("typeset_"),
        function(x) {
          src <- sprintf("\\setlength{\\sw}{\\widthof{%s}}\\the\\sw", x) |>
            paste(collapse = "\n\n")

          params <- params |>
            mutate(
              title_page = FALSE,
              page_width = 1.5 * page_width,
              text_width = 1.5 * text_width,
              `header-includes` = paste(
                c(`header-includes`, "\\usepackage{calc}", "\\newlength{\\sw}"),
                collapse = "\n"
              )
            ) |>
            tidy_params()

          typeset(src = src, params = params) |>
            with(
              getOption("proofs_dir", "~/pdf_proofs/") |>
                path(substr(id, 1, 2), substr(id, 3, 4), id, ext = "pdf") |>
                pdftools::pdf_text() |>
                strsplit("\\s+") |>
                unlist() |>
                keep(grepl, pattern = "^\\d*\\.?\\d+pt$") |>
                sub(pattern = "pt", replacement = "") |>
                as.numeric()
            )
        }
      )
    ) |>
    transmute(
      id,
      page,
      line,
      typeset_width_natural = typeset_width,
      typeset_width_actual = 72.27 * params$text_width -
        indented * params$parindent,
      total_space_expected = typeset_width_natural - typeset_width_nospace,
      total_space_actual = typeset_width_actual - typeset_width_nospace,
      word_space_count = map_int(strsplit(text, "\\s+"), length) - 1L,
      word_space_expected = total_space_expected / word_space_count,
      word_space_actual = total_space_actual / word_space_count,
      text
    )

  if (verbose) {
    return(text)
  } else {
    text |>
      filter(word_space_count > 0) |>
      summarise(
        .by = id,
        word_space_mean = mean(word_space_actual),
        word_space_sd = sd(word_space_actual),
        word_space_lb = word_space_mean - qnorm(0.975) * word_space_sd / sqrt(n()),
        word_space_ub = word_space_mean + qnorm(0.975) * word_space_sd / sqrt(n()),
        word_space_min = min(word_space_actual),
        word_space_p01 = as.numeric(quantile(word_space_actual, probs = 0.01)),
        word_space_p05 = as.numeric(quantile(word_space_actual, probs = 0.05)),
        word_space_p95 = as.numeric(quantile(word_space_actual, probs = 0.95)),
        word_space_p99 = as.numeric(quantile(word_space_actual, probs = 0.99)),
        word_space_max = max(word_space_actual),
        word_space_rmse = sqrt(weighted.mean(
          x = (word_space_actual - word_space_expected)^2,
          w = word_space_count
        ))
      )
  }
}

extract_body_text <- function(x) {
  pdf_path <- proof_path(x)
  stopifnot(file_exists(pdf_path))

  rds_path <- path_ext_set(pdf_path, "rds")
  stopifnot(file_exists(rds_path))
  params <- tidy_params(readRDS(rds_path))

  # get text, remove draft markup, tidy whitespace
  pdf_text <- pdftools::pdf_text(pdf_path) |>
    strsplit("\n") |>
    map(gsub, pattern = " *\\* *", replacement = " ") |>
    map(gsub, pattern = " +", replacement = " ") |>
    map(stringr::str_trim)

  # drop title page
  if (length(pdf_text[[2]]) == 0L) pdf_text <- tail(pdf_text, -2)

  # check page numbers, flatten
  pdf_text <- pdf_text |>
    imap_dfr(function(x, i) {
      stopifnot(identical(i, as.integer(tail(x, 1))))
      x <- head(x, -2)
      tibble(
        page = i,
        line = if_else(
          cumsum(grepl("chapter", x, ignore.case = TRUE)) > 0,
          seq(
            max(map_int(pdf_text, length)) - length(x) + 1,
            max(map_int(pdf_text, length))
          ),
          seq_along(x)
        ),
        text = x
      )
    })

  # confirm all source regexes are matched
  regex_counts <- source_text_regexes |>
    mutate(
      match_count = map_int(regex, ~ sum(grepl(., pdf_text$text))),
      match_count_expected = 1,
      match_count_expected = if_else(
        params$reflow_epi_3 & grepl("Voyage of the two inhabitants", regex),
        2,
        1
      ),
      match_count_expected = if_else(
        params$reflow_epi_5 & grepl("Experiments and reasonings", regex),
        2,
        1
      )
    )
  stopifnot(all(regex_counts$match_count == regex_counts$match_count_expected))

  # use regexes to flag special lines/environments
  y <- pdf_text |>
    mutate(
      regexes = text |>
        map(~ tibble(text = ., source_text_regexes)) |>
        map(~ dplyr::filter(., map2_lgl(regex, text, grepl))) |>
        map(~ select(., where(is.logical))) |>
        map(function(x) {
          if (nrow(x) == 0L) {
            as_tibble(map(x, ~FALSE))
          } else {
            x
          }
        })
    ) |>
    unnest(regexes)
  stopifnot(identical(pdf_text, select(y, names(pdf_text))))

  # Tidy text and prepare TeX text with inline formatting
  y |>
    mutate(
      text = if_else(chapter, toupper(text), text) |>
        sub(pattern = "^O\\s+n one\\b", replacement = "ON ONE") |>
        gsub(pattern = " *([:;!?])", replacement = "\\1"),
      tex_text = stringr::str_trim(text) |>
        sub(pattern = "^O n one\\b", replacement = "O\\\\hspace{11pt plus 1pt minus 2pt}{\\\\scshape{}n one}") |>
        sub(pattern = "\\bthe Mediterranean\\b", replacement = "\\\\emph{the Mediterranean}") |>
        sub(pattern = "pond we call the$", replacement = "pond we call \\\\emph{the}") |>
        sub(pattern = "^Mediterranean\b", replacement = "\\\\emph{Mediterranean}") |>
        sub(pattern = "we call the Mediter-$", replacement = "we call \\\\emph{the Mediter-}") |>
        sub(pattern = "^ranean\b", replacement = "\\\\emph{ranean}") |>
        sub(pattern = "\\bOcean\\b", replacement = "\\\\emph{Ocean}") |>
        sub(pattern = "\\bOn the Soul\\b", replacement = "\\\\emph{On the Soul}") |>
        sub(pattern = "Aristotle, \\bOn the$", replacement = "Aristotle, \\\\emph{On the}") |>
        sub(pattern = "^Soul\\b", replacement = "\\\\emph{Soul}") |>
        sub(pattern = "Aristotle, \\bOn$", replacement = "Aristotle, \\\\emph{On}") |>
        sub(pattern = "^the Soul\\b", replacement = "\\\\emph{the Soul}") |>
        sub(pattern = "\\bSumma\\b", replacement = "\\\\emph{Summa}") |>
        sub(pattern = "\\b([iI]{2})\\b", replacement = "\\\\allsc{II}")
    ) |>
    relocate(where(is.character), .after = everything())
}
