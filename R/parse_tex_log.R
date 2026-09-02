parse_tex_log <- function(x) {
  log_path <- path_ext_set(proof_path(x), "log")
  stopifnot(file_exists(log_path))

  log <- readLines(log_path)

  exceptions <- log[
    grepl("warn|error", log, ignore.case = TRUE) &
      !grepl("Package: infwarerr .* Providing info/warning/error messages", log) &
      !grepl("Package hyperref Warning: Draft mode on.", log) &
      !grepl("The material used in the headers is too large", log) &
      !grepl("Package widows-and-orphans", log) &
      !grepl("Overfull", dplyr::lag(log))
  ]

  overfulls <- tibble(log, overfull = cumsum(grepl("Overfull", log)))
  overfulls <- overfulls[overfulls$overfull > 0, ]
  overfulls <- split(overfulls, overfulls$overfull)
  overfulls <- map(overfulls, head, n = 3)
  overfulls <- map_dfr(overfulls, function(x) {
    log <- x$log
    log <- gsub("[]", " ", log, fixed = TRUE)
    log <- str_trim(log)
    log <- paste(log, collapse = " ")
    log <- log[!grepl(" \\\\vbox ", log)]

    tibble(
      amount = as.numeric(sub("^.+?([0-9\\.]+)pt .+?$", "\\1", log)),
      log = sub(
        "Overfull \\\\hbox \\([0-9\\.]+pt too wide\\) in paragraph at ",
        "",
        log
      )
    )
  })
  if (nrow(overfulls) == 0L) {
    overfulls <- tibble(amount = 0, log = NA)
  }

  underfulls <- tibble(log, underfull = cumsum(grepl("Underfull", log)))
  underfulls <- underfulls[underfulls$underfull > 0, ]
  underfulls <- split(underfulls, underfulls$underfull)
  underfulls <- map(underfulls, head, n = 4)
  underfulls <- map_dfr(underfulls, function(x) {
    log <- x$log
    log <- str_trim(log)
    log <- paste(log, collapse = " ")

    tibble(
      amount = as.numeric(sub("^.+?\\(badness ([0-9\\.]+)\\) .+?$", "\\1", log)),
      log
    )
  })
  if (nrow(underfulls) == 0L) {
    underfulls <- tibble(amount = 0, log = NA)
  }

  tibble(
    id = path_ext_remove(path_file(log_path)),
    exception_count = length(exceptions),
    overfull_count = nrow(drop_na(overfulls)),
    overfull_total = sum(overfulls$amount),
    underfull_count = nrow(drop_na(underfulls)),
    underfull_total = sum(underfulls$amount),
    exceptions = list(exceptions),
    overfulls = list(overfulls),
    underfulls = list(underfulls),
  )
}
