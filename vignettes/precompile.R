# https://ropensci.org/blog/2019/12/08/precompute-vignettes/

library(fs)
library(knitr)

"vignettes/source_text_md.Rmd.orig" |>
  knit(output = "vignettes/source_text_md.Rmd")

"vignettes/source_text_md_tex.Rmd.orig" |>
  knit(output = "vignettes/source_text_md_tex.Rmd")

if (dir_exists("figure")) {
  dir_copy("figure", "vignettes/figure", overwrite = TRUE)
  dir_delete("figure")
}
