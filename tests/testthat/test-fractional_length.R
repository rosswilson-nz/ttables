test_that("fractional_length() works", {
  expect_equal(fractional_length(1), new_fractional_length(1))
  expect_equal(fractional_length(2:3), new_fractional_length(c(2, 3)))
})

test_that("fractional_length() errors correctly", {
  expect_error(fractional_length("1"))
})

test_that("is_fractional_length() works", {
  expect_true(is_fractional_length(fractional_length(1)))
  expect_false(is_fractional_length("1fr"))
})

test_that("is.na.ttables_fractional_length() works", {
  expect_equal(is.na(fractional_length(1)), FALSE)
  expect_equal(is.na(fractional_length(c(2, NA, 3))), c(FALSE, TRUE, FALSE))
  expect_equal(is.na(NA_fractional_length_), TRUE)
})

test_that("format.ttables_fractional_length() prints correctly", {
  expect_snapshot(fractional_length(1:4))
})

test_that("as_fractional_length() works", {
  expect_equal(as_fractional_length(c("2fr", "3fr")), fractional_length(2:3))
  expect_error(as_fractional_length(c("4cm")))
})

test_that("as.character.ttables_fractional_length() works", {
  expect_equal(
    as.character(fractional_length(1:4)),
    c("1fr", "2fr", "3fr", "4fr")
  )
})

test_that("fractional_length arithmetic works", {
  expect_equal(
    fractional_length(1) + fractional_length(2),
    fractional_length(3)
  )
  expect_equal(
    fractional_length(1:2) + fractional_length(2:3),
    fractional_length(c(3, 5))
  )

  expect_equal(
    fractional_length(1) - fractional_length(2),
    -fractional_length(1)
  )

  expect_equal(fractional_length(3) / fractional_length(2), 1.5)

  expect_equal(fractional_length(3) * 2, fractional_length(6))
  expect_equal(fractional_length(8) / 4, fractional_length(2))

  expect_equal(3 * fractional_length(3), fractional_length(9))

  expect_equal(+fractional_length(2), fractional_length(2))
})
