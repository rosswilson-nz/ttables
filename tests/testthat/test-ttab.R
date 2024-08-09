test_that("ttab() produces a `typst_table` object", {
  expect_s3_class(ttab(mtcars), "typst_table")
})
