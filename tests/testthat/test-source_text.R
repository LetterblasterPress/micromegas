test_that("it reads `md` format by default", {
  expect_identical(source_text(), source_text("md"))
  expect_identical(source_text(), readLines(inst("source_text", "md.md")))
})

test_that("it reads `md+tex` format", {
  expect_identical(
    source_text("md+tex"),
    readLines(inst("source_text", "md_tex.md"))
  )
})
