required_params <- tibble(
  page_height = 101,
  page_width = 102,
  text_height_lines = 103,
  text_width = 104,
  t_mar = 105,
  i_mar = 106
)

all_params <- formals(tidy_params) %>%
  subset(names(.) != "...") |>
  purrr::imap(function(x, i) {
    if (i %in% names(required_params)) {
      required_params[[i]]
    } else {
      with(formals(tidy_params), eval(x))
    }
  }) |>
  tibble::as_tibble()

test_that("given named parameters, it returns a tibble of default values", {
  y <- do.call(tidy_params, as.list(required_params))

  expect_s3_class(y, "tbl_df")
  expect_named(y, setdiff(names(formals(tidy_params)), "..."))
  expect_identical(y, all_params)

  y <- do.call(tidy_params, as.list(all_params))

  expect_s3_class(y, "tbl_df")
  expect_named(y, setdiff(names(formals(tidy_params)), "..."))
  expect_identical(y, all_params)
})

test_that("given a one-row tibble of parameters, it returns a tibble of default values", {
  y <- tidy_params(as_tibble(required_params))

  expect_s3_class(y, "tbl_df")
  expect_named(y, setdiff(names(formals(tidy_params)), "..."))
  expect_identical(y, all_params)

  y <- tidy_params(as_tibble(all_params))

  expect_s3_class(y, "tbl_df")
  expect_named(y, setdiff(names(formals(tidy_params)), "..."))
  expect_identical(y, all_params)
})

test_that("given a multi-row tibble of parameters, it returns a tibble of default values", {
  y <- tidy_params(bind_rows(required_params, required_params))

  expect_s3_class(y, "tbl_df")
  expect_named(y, setdiff(names(formals(tidy_params)), "..."))
  expect_identical(y, bind_rows(all_params, all_params))

  y <- tidy_params(bind_rows(all_params, all_params))

  expect_s3_class(y, "tbl_df")
  expect_named(y, setdiff(names(formals(tidy_params)), "..."))
  expect_identical(y, bind_rows(all_params, all_params))
})

test_that("it throws an error when default values are missing", {
  for (i in names(required_params)) {
    expect_error(tidy_params(select(required_params, -any_of(i))), i)
  }
})

test_that("it passes non-default values to the returned tibble", {
  for (i in names(all_params)) {
    these_params <- all_params
    these_params[, i] <- "test value"
    expect_identical(these_params, tidy_params(these_params))
  }
})

test_that("it ignores extra arguments", {
  y_tbl <- tidy_params(layout_candidates)
  y_list <- do.call(tidy_params, as.list(layout_candidates))

  expect_identical(y_tbl, y_list)
  expect_named(y_tbl, names(all_params))
  expect_named(y_list, names(all_params))
})

test_that("it is idempotent", {
  expect_identical(all_params, tidy_params(all_params))
  expect_identical(all_params, tidy_params(tidy_params(all_params)))
  expect_identical(
    tidy_params(layout_candidates),
    tidy_params(tidy_params(layout_candidates))
  )
})
