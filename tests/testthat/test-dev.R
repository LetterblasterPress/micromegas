## inst ########################################################################

test_that("inst() returns the expected path or an empty string", {
  expect_identical(
    inst("foo", "bar"),
    ""
  )

  expect_identical(
    inst("foo", "bar"),
    system.file("foo", "bar", package = "micromegas")
  )

  expect_identical(
    inst("WORDLIST"),
    system.file("WORDLIST", package = "micromegas")
  )
})
