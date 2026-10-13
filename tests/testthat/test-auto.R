test_that("auto() works", {
  expect_s3_class(auto(), "ttables_auto")
  expect_equal(
    horiz_alignment(c("right", "centre", "left")),
    new_horiz_alignment(c("right", "center", "left"))
  )
})

test_that("is_auto() works", {
  expect_true(is_auto(auto()))
  expect_false(is_auto("auto"))
})

test_that("format.ttables_auto() prints correctly", {
  expect_snapshot(auto())
  expect_snapshot(format(auto()))
})

test_that("none() works", {
  expect_s3_class(none(), "ttables_none")
  expect_equal(
    horiz_alignment(c("right", "centre", "left")),
    new_horiz_alignment(c("right", "center", "left"))
  )
})

test_that("is_none() works", {
  expect_true(is_none(none()))
  expect_false(is_none("none"))
})

test_that("format.ttables_none() prints correctly", {
  expect_snapshot(none())
  expect_snapshot(format(none()))
})
