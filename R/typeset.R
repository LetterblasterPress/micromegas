#' TODO
#'
#' TODO
#'
#' @param params TODO
#' @param src TODO
#' @param meta TODO
#' @param template TODO
#' @param proof_dir TODO
#' @param dryrun TODO
#'
#' @returns a [tibble][tibble::tibble-package] of tidied typesetting
#'   parameters plus metrics describing the resulting typesets
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
  params <- do.call(tidy_params, params)

  # use hash of inputs as a cache ID
  id <- digest(list(
    src = src,
    meta = meta,
    template = template,
    params = params,
    ttc_id = digest(inst("CentaurMH.ttc"), file = TRUE)
  ))

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
    write(src, src_path)
    write(c(meta, paste("template:", path_file(template_path))), meta_path)
    write(template, template_path)
    file_copy(inst("CentaurMH.ttc"), ttc_path)
    on.exit(unlink(ttc_path))

    message("Rendering ", id)

    withr::with_dir(
      proof_dir,
      quarto::quarto_render(
        input = path_file(src_path),
        output_format = "latex",
        output_file = path_file(tex_path),
        metadata_file = path_file(meta_path),
        metadata = as.list(params),
        quiet = TRUE
      )
    )

    withr::with_dir(
      proof_dir,
      system2("lualatex", path_file(tex_path), stdout = FALSE)
    )
    withr::with_dir(
      proof_dir,
      system2("lualatex", path_file(tex_path), stdout = FALSE)
    )

    tibble(id, params, log = list(read_lines(log_path))) |>
      parse_tex_log() |>
      mutate(
        type_usage = estimate_type_usage(pdf_path),
        hyphen_run_count = count_hyphen_runs(pdf_path)
      ) |>
      saveRDS(rds_path)
  }

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
}
