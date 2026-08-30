test_that("it returns the input path when the file exists", {
  mock_file_exists <- mock(TRUE)
  stub(proof_path, "file_exists", mock_file_exists)

  expect_identical(proof_path("test_path"), path_expand("test_path"))
})

test_that("it constructs path to PDF when just an ID is supplied", {
  mock_file_exists <- mock(FALSE)
  stub(proof_path, "file_exists", mock_file_exists)

  expect_identical(
    proof_path("test_path"),
    path_expand(path(proofs_dir(), path("te", "st", "test_path", ext = "pdf")))
  )
})
