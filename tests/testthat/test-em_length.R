test_that("em_length() works", {
  expect_equal(em_length(1), new_em_length(1))
  expect_equal(em_length(2:3), new_em_length(c(2, 3)))
})

test_that("em_length() errors correctly", {
  expect_error(em_length("1"))
})

test_that("is_em_length() works", {
  expect_true(is_em_length(em_length(1)))
  expect_false(is_em_length("1em"))
})

test_that("is.na.ttables_em_length() works", {
  expect_equal(is.na(em_length(1)), FALSE)
  expect_equal(is.na(em_length(c(2, NA, 3))), c(FALSE, TRUE, FALSE))
  expect_equal(is.na(NA_em_length_), TRUE)
})

test_that("format.ttables_em_length() prints correctly", {
  expect_snapshot(em_length(1:4))
})

test_that("as_em_length() works", {
  expect_equal(as_em_length(c("2em", "3em")), em_length(2:3))
  expect_error(as_em_length(c("4cm")))
})

test_that("as.character.ttables_em_length() works", {
  expect_equal(
    as.character(em_length(1:4)),
    c("1em", "2em", "3em", "4em")
  )
})

test_that("em_length arithmetic works", {
  expect_equal(em_length(1) + em_length(2), em_length(3))
  expect_equal(
    em_length(1:2) + em_length(2:3),
    em_length(c(3, 5))
  )

  expect_equal(em_length(1) - em_length(2), -em_length(1))

  expect_equal(em_length(3) / em_length(2), 1.5)

  expect_equal(em_length(3) * 2, em_length(6))
  expect_equal(em_length(8) / 4, em_length(2))

  expect_equal(3 * em_length(3), em_length(9))

  expect_equal(+em_length(2), em_length(2))
})
