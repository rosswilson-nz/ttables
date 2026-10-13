test_that("ratio() works", {
  expect_equal(ratio(1), new_ratio(1))
  expect_equal(ratio(2:3), new_ratio(c(2, 3)))
})

test_that("ratio() errors correctly", {
  expect_error(ratio("1"))
})

test_that("is_ratio() works", {
  expect_true(is_ratio(ratio(1)))
  expect_false(is_ratio("1%"))
})

test_that("is.na.ttables_ratio() works", {
  expect_equal(is.na(ratio(1)), FALSE)
  expect_equal(is.na(ratio(c(2, NA, 3))), c(FALSE, TRUE, FALSE))
  expect_equal(is.na(NA_ratio_), TRUE)
})

test_that("format.ttables_ratio() prints correctly", {
  expect_snapshot(ratio(1:4))
})

test_that("as_ratio() works", {
  expect_equal(as_ratio(c("2%", "3%")), ratio(2:3))
  expect_error(as_ratio(c("4cm")))
})

test_that("as.character.ttables_ratio() works", {
  expect_equal(
    as.character(ratio(1:4)),
    c("1%", "2%", "3%", "4%")
  )
})

test_that("ratio arithmetic works", {
  expect_equal(
    ratio(1) + ratio(2),
    ratio(3)
  )
  expect_equal(
    ratio(1:2) + ratio(2:3),
    ratio(c(3, 5))
  )

  expect_equal(
    ratio(1) - ratio(2),
    -ratio(1)
  )

  expect_equal(ratio(3) / ratio(2), 1.5)

  expect_equal(ratio(3) * 2, ratio(6))
  expect_equal(ratio(8) / 4, ratio(2))

  expect_equal(3 * ratio(3), ratio(9))

  expect_equal(+ratio(2), ratio(2))
})
