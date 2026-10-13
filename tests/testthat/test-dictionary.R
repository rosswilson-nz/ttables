test_that("dictionary() works", {
  expect_equal(dictionary(a = 1), new_dictionary(list(a = 1)))
  expect_equal(
    dictionary(b = 2, c = "3"),
    new_dictionary(list(b = 2, c = "3"))
  )
})

test_that("dictionary() errors correctly", {
  expect_error(dictionary("mydictionary"))
  expect_error(dictionary(list(a = 1)))
})

test_that("is_dictionary() works", {
  expect_true(is_dictionary(dictionary(a = 1)))
  expect_false(is_dictionary(list("fuchsia")))
})

test_that("format.ttables_dictionary() prints correctly", {
  expect_snapshot(format(dictionary(a = 1, b = "2", c = 3:4)))
  expect_snapshot(dictionary(a = 1, b = "2", c = 3:4))
})

test_that("as_dictionary() works", {
  expect_equal(as_dictionary(list(d = 4)), dictionary(d = 4))
  expect_equal(
    as_dictionary(list(e = 5, f = "6")),
    dictionary(e = 5, f = "6")
  )
})

test_that("as.list.ttables_dictionary() works", {
  expect_equal(
    as.list(dictionary(e = 5, f = "6")),
    list(e = 5, f = "6")
  )
})
