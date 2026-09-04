estimate_type_usage <- function(x, verbose = FALSE) {
  # tally available metal type
  sorts <- readRDS(inst("centaur.rds"))
  sorts <- sorts[sorts$font == "Centaur 11/12", ]
  sorts <- sorts[sorts$case %in% c("upper", "lower"), ]
  sorts <- tibble(
    char = if_else(
      sorts$case %in% "upper",
      toupper(as.character(sorts$glyph)),
      tolower(as.character(sorts$glyph))
    ),
    sort_count = sorts$count
  )

  # tally type usage in the proof PDF
  y <- proof_path(x) |>
    pdf_text() |>
    imap_dfr(function(x, i) tibble(page = i, text = x))

  y$text <- y$text |>
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
    map(function(x) as.data.frame(table(unlist(strsplit(x, "")))))

  y <- unnest(y, "text")
  y <- tibble(page = y$page, char = as.character(y$Var1), count = y$Freq)
  y <- y[!y$char %in% c("\n", " "), ]
  y <- split(y, y[, c("page", "char")], drop = TRUE) |>
    map_dfr(function(x) {
      y <- x[, c("page", "char")]
      y$count <- sum(x$count)
      return(y)
    })

  # compare type usage to available metal type
  y$char <- y$char |>
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

  y <- inner_join(y, sorts, by = "char")
  y <- split(y, y$page) |>
    map_dfr(function(x) {
      tibble(
        page = unique(x$page),
        pct = max(x$count / x$sort_count)
      )
    })

  # return the max percentage of used type
  if (verbose) {
    return(y)
  } else {
    return(max(y$pct))
  }
}
