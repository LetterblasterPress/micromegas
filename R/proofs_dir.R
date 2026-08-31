proofs_dir <- function() {
  y <- dir_create(Sys.getenv("proofs_dir", "~/pdf_proofs/"))
  return(y)
}
