test_that("colour() works", {
  expect_equal(colour("black"), new_colour("black"))
  expect_equal(
    colour(c("gray", "navy", "green")),
    new_colour(c("gray", "navy", "green"))
  )
})

test_that("colour() errors correctly", {
  expect_error(colour("mycolour"))
  expect_error(colour(1))
})

test_that("is_colour() works", {
  expect_true(is_colour(colour("fuchsia")))
  expect_false(is_colour("fuchsia"))
})

test_that("is.na.ttables_colour() works", {
  expect_equal(is.na(colour("white")), FALSE)
  expect_equal(
    is.na(colour(c("silver", NA, "blue"))),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(is.na(NA_colour_), TRUE)
})

test_that("format.ttables_colour() prints correctly", {
  expect_snapshot(colour(c("aqua", "teal", NA, "eastern")))
})

test_that("as_colour() works", {
  expect_equal(as_colour("purple"), colour("purple"))
  expect_equal(
    as_colour(c("maroon", "red", "orange")),
    colour(c("maroon", "red", "orange"))
  )
})

test_that("as.character.ttables_colour() works", {
  expect_equal(
    as.character(colour(c("yellow", "olive", "lime"))),
    c("yellow", "olive", "lime")
  )
})
