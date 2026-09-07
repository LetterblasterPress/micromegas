# https://ropensci.org/blog/2019/12/08/precompute-vignettes/

fs::dir_ls("vignettes", glob = "*.Rmd.orig") |>
  map(~ knitr::knit(., output = fs::path_ext_remove(sub("\\d{2}_", "", .))))

if (fs::dir_exists("figure")) {
  fs::dir_copy("figure", "vignettes/figure", overwrite = TRUE)
  fs::dir_delete("figure")
}
