test_that("it returns path to `~/pdf_proofs` if no environment variable", {
  expect_identical(
    withr::with_envvar(c(proofs_dir = NA), proofs_dir()),
    path("~/pdf_proofs")
  )
})

test_that("it returns custom path if environment variable set", {
  expect_identical(
    withr::with_envvar(c(proofs_dir = "test_proofs_dir"), proofs_dir()),
    path("test_proofs_dir")
  )
})
