test_that("typesetting_metadata() reads typesetting_metadata.yml", {
  expect_identical(
    typesetting_metadata(),
    readLines(inst("typesetting_metadata.yml"))
  )
})

test_that("typesetting_template() reads typesetting_template.latex", {
  expect_identical(
    typesetting_template(),
    readLines(inst("typesetting_template.latex"))
  )
})
