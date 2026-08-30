proofs_dir <- function() {
  dir_create(Sys.getenv("proofs_dir", "~/pdf_proofs/"))
}
