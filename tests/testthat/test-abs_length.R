test_that("abs_length() works", {
  expect_equal(abs_length(1, "pt"), new_abs_length(1, "pt"))
  expect_equal(abs_length(2:3, "cm"), new_abs_length(c(2, 3), c("cm", "cm")))
  expect_equal(
    abs_length(4, c("mm", "in")),
    new_abs_length(c(4, 4), c("mm", "in"))
  )
})

test_that("abs_length() errors correctly", {
  expect_error(abs_length(1, NA))
  expect_error(abs_length(2, "a"))
  expect_error(abs_length("3", "cm"))
})

test_that("is_abs_length() works", {
  expect_true(is_abs_length(abs_length(1, "pt")))
  expect_false(is_abs_length("1pt"))
})

test_that("is.na.ttables_abs_length() works", {
  expect_equal(is.na(abs_length(1, "pt")), FALSE)
  expect_equal(is.na(abs_length(c(2, NA, 3), "cm")), c(FALSE, TRUE, FALSE))
  expect_equal(is.na(NA_abs_length_), TRUE)
})

test_that("format.ttables_abs_length() prints correctly", {
  expect_snapshot(abs_length(1:4, c("pt", "mm", "cm", "in")))
})

test_that("as_abs_length() works", {
  expect_equal(as_abs_length("1pt"), abs_length(1, "pt"))
  expect_equal(
    as_abs_length(c("2mm", "3cm", "4in")),
    abs_length(2:4, c("mm", "cm", "in"))
  )
})

test_that("as.character.ttables_abs_length() works", {
  expect_equal(
    as.character(abs_length(1:4, c("pt", "mm", "cm", "in"))),
    c("1pt", "2mm", "3cm", "4in")
  )
})

test_that("length conversion works", {
  expect_equal(
    abs_length(1:4, c("pt", "cm", "mm", "in")) ==
      abs_length(c(1, 20, 2, 10.16), c("pt", "mm", "cm", "cm")),
    c(TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("abs_length arithmetic works", {
  expect_equal(abs_length(1, "pt") + abs_length(2, "pt"), abs_length(3, "pt"))
  expect_equal(
    abs_length(1:2, "cm") + abs_length(2:3, "mm"),
    abs_length(c(1.2, 2.3), "cm")
  )

  expect_equal(abs_length(1, "pt") - abs_length(2, "pt"), -abs_length(1, "pt"))

  expect_equal(abs_length(1, "in") / abs_length(1, "mm"), 25.4)

  expect_equal(abs_length(3, "cm") * 2, abs_length(6, "cm"))
  expect_equal(abs_length(8, "pt") / 4, abs_length(2, "pt"))

  expect_equal(3 * abs_length(3, "mm"), abs_length(9, "mm"))

  expect_equal(+abs_length(2, "in"), abs_length(2, "in"))
})
