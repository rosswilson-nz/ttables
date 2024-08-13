test_that("ttab() produces a `ttables_tbl` object", {
  expect_s3_class(ttab(mtcars), "ttables_tbl")
})
