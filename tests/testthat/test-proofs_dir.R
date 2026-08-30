test_that("it returns path to `~/pdf_proofs` if no environment variable", {
  expect_identical(
    withr::with_envvar(c(proofs_dir = NA), proofs_dir()),
    fs::path_expand(path("~/pdf_proofs"))
  )
})

test_that("it returns custom path if environment variable set", {
  tmp <- fs::file_temp()
  on.exit(unlink(tmp))
  expect_false(fs::dir_exists(tmp))

  expect_identical(
    withr::with_envvar(c(proofs_dir = tmp), proofs_dir()),
    fs::path_expand(path(tmp))
  )

  expect_true(fs::dir_exists(tmp))
})
