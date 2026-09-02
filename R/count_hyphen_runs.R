count_hyphen_runs <- function(x, verbose = FALSE) {
  pdf_path <- proof_path(x)
  stopifnot(file_exists(pdf_path))

  rds_path <- path_ext_set(pdf_path, "rds")
  stopifnot(file_exists(rds_path))
  params <- tidy_params(readRDS(rds_path))

  txt <- pdf_text(pdf_path) |>
    strsplit("\n")

  if (params$title_page) {
    stopifnot(length(txt[[2]]) == 0L)
    txt <- tail(txt, -2)
  }

  y <- txt |>
    map_chr(function(x) {
      if_else(grepl("-$", x), "-", " ") |>
        paste(collapse = "")
    }) |>
    strsplit("\\s+") |>
    imap_dfr(function(x, i) {
      tibble(page = as.integer(i), hyphens = x)
    })

  y <- y[!y$hyphens %in% "", ]

  if (verbose) {
    y <- summarise(y, .by = everything(), count = n()) |>
      arrange(across(everything()))
  } else {
    y <- sum(nchar(y$hyphens) > 3)
  }

  return(y)
}
