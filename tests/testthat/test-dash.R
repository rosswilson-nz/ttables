test_that("dash() works", {
  expect_equal(dash("solid"), new_dash("solid"))
  expect_equal(
    dash(c("dotted", "densely-dotted", "loosely-dotted")),
    new_dash(c("dotted", "densely-dotted", "loosely-dotted"))
  )
})

test_that("dash() errors correctly", {
  expect_error(dash("mydash"))
  expect_error(dash(1))
})

test_that("is_dash() works", {
  expect_true(is_dash(dash("dashed")))
  expect_false(is_dash("dashed"))
})

test_that("is.na.ttables_dash() works", {
  expect_equal(is.na(dash("densely-dashed")), FALSE)
  expect_equal(
    is.na(dash(c("loosely-dashed", NA, "dash-dotted"))),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(is.na(NA_dash_), TRUE)
})

test_that("format.ttables_dash() prints correctly", {
  expect_snapshot(dash(c(
    "densely-dash-dotted",
    "loosely-dash-dotted",
    NA,
    "solid"
  )))
})

test_that("as_dash() works", {
  expect_equal(as_dash("dotted"), dash("dotted"))
  expect_equal(
    as_dash(c("solid", "dashed", "dotted")),
    dash(c("solid", "dashed", "dotted"))
  )
})

test_that("as.character.ttables_dash() works", {
  expect_equal(
    as.character(dash(c("densely-dotted", "loosely-dotted", "densely-dashed"))),
    c("densely-dotted", "loosely-dotted", "densely-dashed")
  )
})
