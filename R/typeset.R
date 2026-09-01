#' Render one or more PDFs and cache results locally
#'
#' The `typeset()` function renders a PDF using the supplied inputs, caching all
#' intermediate files in `proofs_dir()`. The `map_typeset()` function can be
#' used to render many PDFs by supplying a data frame of parameters.
#'
#' @param params typesetting parameters, passed to `tidy_params()`
#' @param src typesetting source as a character vector of Markdown text
#' @param meta typesetting metadata as a character vector of (unparsed) YAML
#' @param template Quarto template as a character vector, used to convert
#'   Markdown to LaTeX
#' @param proof_dir path to proofs directory
#' @param dryrun if `TRUE`, returns tidied parameters without rendering
#'
#' @returns a [tibble][tibble::tibble-package] of tidied typesetting parameters
#'   plus an ID column with the MD5 hash of all typesetting inputs
#' @export
#'
#' @name typeset
NULL

#' @rdname typeset
#' @export
typeset <- function(
  params = list(),
  src = source_text("md+tex"),
  meta = typesetting_metadata(),
  template = typesetting_template(),
  proof_dir = proofs_dir()
) {
  # nocov start
  params <- do.call(tidy_params, params)

  # use hash of inputs as a cache ID
  id <- digest(list(
    src = src,
    meta = meta,
    template = template,
    params = params,
    ttc_id = digest(inst("CentaurMH.ttc"), file = TRUE)
  ))

  # define paths for intermediate files
  proof_dir <- dir_create(proof_path(id))
  src_path <- path(proof_dir, id, ext = "md")
  meta_path <- path(proof_dir, id, ext = "yml")
  template_path <- path(proof_dir, id, ext = "latex")
  tex_path <- path(proof_dir, id, ext = "tex")
  log_path <- path(proof_dir, id, ext = "log")
  pdf_path <- path(proof_dir, id, ext = "pdf")
  rds_path <- path(proof_dir, id, ext = "rds")
  ttc_path <- path(proof_dir, "CentaurMH.ttc")

  if (!file_exists(rds_path)) {
    message("Rendering ", id)

    # copy inputs
    write(src, src_path)
    write(c(meta, paste("template:", path_file(template_path))), meta_path)
    write(template, template_path)
    file_copy(inst("CentaurMH.ttc"), ttc_path)
    on.exit(unlink(ttc_path))

    # Markdown -> LaTeX
    with_dir(
      proof_dir,
      quarto_render(
        input = path_file(src_path),
        output_format = "latex",
        output_file = path_file(tex_path),
        metadata_file = path_file(meta_path),
        metadata = as.list(params),
        quiet = TRUE
      )
    )

    # LaTeX -> PDF (twice to ensure references are correct)
    with_dir(
      proof_dir,
      system2("lualatex", path_file(tex_path), stdout = FALSE)
    )
    with_dir(
      proof_dir,
      system2("lualatex", path_file(tex_path), stdout = FALSE)
    )

    # save
    saveRDS(tibble(id, params), rds_path)
  }

  # read cached results
  read_rds(rds_path)
}

#' @rdname typeset
#' @export
map_typeset <- function(params, dryrun = FALSE) {
  y <- distinct(tidy_params(params))
  if (dryrun) {
    return(y)
  } else {
    map_dfr(split(y, seq(nrow(y))), typeset)
  }

  # nocov end
}
