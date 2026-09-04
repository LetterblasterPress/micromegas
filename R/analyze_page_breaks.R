analyze_page_breaks <- function(x) {
  # read log file
  log <- proof_path(x) |>
    path_ext_set("log") |>
    readLines()

  # confirm widows-and-orphans package was active
  stopifnot(any(grepl("Detecting widows and orphans", log)))

  # parse widows-and-orphans warnings
  widows_and_orphans <- log
  widows_and_orphans <- widows_and_orphans |>
    subset(grepl("warn|error", widows_and_orphans, ignore.case = TRUE))
  widows_and_orphans <- widows_and_orphans |>
    subset(grepl("Package widows-and-orphans", widows_and_orphans))

  # categorize and count page-break issues
  if (length(widows_and_orphans) == 0L) {
    y <- tibble(
      orphan_widow_recto = 0,
      orphan_widow_verso = 0,
      widow_verso = 0,
      widow_recto = 0,
      orphan_recto = 0,
      orphan_verso = 0,
      hyphen_recto = 0,
      hyphen_verso = 0
    )

    # analyze text to find pagination issues widows-and-orphan package missed
    second_opinion <- parse_pdf_text(x, strict = FALSE) |>
      filter(.by = "page", .data$line %in% range(.data$line))

    second_opinion <- split(second_opinion, second_opinion$page) |>
      map_dfr(mutate, line = c("top", "bottom")) |>
      tail(-1) |>
      head(-1)

    recto <- subset(second_opinion, (second_opinion$page %% 2) == 1)
    y$orphan_recto <- sum( # sic, to match package definition
      recto$line == "bottom" & recto$first_line & !recto$last_line
    )
    y$widow_recto <- sum( # sic, to match package definition
      recto$line == "top" & recto$last_line & !recto$first_line
    )

    verso <- subset(second_opinion, (second_opinion$page %% 2) == 0)
    y$orphan_verso <- sum( # sic, to match package definition
      verso$line == "bottom" & verso$first_line & !verso$last_line
    )
    y$widow_verso <- sum( # sic, to match package definition
      verso$line == "top" & verso$last_line & !verso$first_line
    )
  } else {
    y <- widows_and_orphans |>
      map_dfr(function(i) {
        # NOTE: package defines orphan at bottom of page, widow at top

        y <- tibble(i)
        y$page <- as.integer(sub("^.*page (\\d+).*$", "\\1", y$i))
        y$recto <- (y$page %% 2) == 1
        y$verso <- (y$page %% 2) == 0
        y$orphan_widow <- grepl(" and", y$i)
        y$orphan <- !y$orphan_widow & grepl("Orphan", y$i)
        y$widow <- grepl("Widow", y$i)
        y$hyphen <- grepl("Hyphen", y$i)

        y <- tibble(
          orphan_widow_recto = 2 * sum(y$orphan_widow & y$recto),
          orphan_widow_verso = 2 * sum(y$orphan_widow & y$verso),
          widow_verso = sum(y$widow & y$verso),
          widow_recto = sum(y$widow & y$recto),
          orphan_recto = sum(y$orphan & y$recto),
          orphan_verso = sum(y$orphan & y$verso),
          hyphen_recto = sum(y$hyphen & y$recto),
          hyphen_verso = sum(y$hyphen & y$verso)
        )

        return(y)
      })
  }

  y$w_o_h_total <- sum(unlist(y))

  return(y)
}
