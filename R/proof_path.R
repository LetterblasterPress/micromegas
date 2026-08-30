proof_path <- function(x) {
  if (file_exists(x)) {
    y <- x
  } else {
    y <- proofs_dir() |>
      path(substr(x, 1, 2), substr(x, 3, 4), x, ext = "pdf")
  }

  path_expand(y)
}
