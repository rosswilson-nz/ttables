test_that("gutter() works", {
  expect_equal(
    gutter(width(list(abs_length(1, "cm")))),
    new_gutter(width(list(relative(abs_length(1, "cm")))))
  )
  expect_equal(
    gutter(row = width(list(abs_length(1, "cm")))),
    new_gutter(row = width(list(relative(abs_length(1, "cm")))))
  )
  expect_equal(
    gutter(auto(), width(list(ratio(10), fractional_length(1)))),
    new_gutter(auto(), width(list(ratio(10), fractional_length(1))))
  )
})

test_that("gutter() errors correctly", {
  expect_error(gutter("1x"))
})

test_that("is_gutter() works", {
  expect_true(is_gutter(gutter(width(list(abs_length(1, "cm"))))))
  expect_false(is_gutter("1pt"))
})

test_that("as_gutter() works", {
  expect_equal(as_gutter(NULL), gutter())
  expect_equal(as_gutter(list(column = auto())), gutter(auto()))
  expect_equal(as_gutter(list(row = auto())), gutter(row = auto()))
  expect_equal(
    as_gutter(list(column = "2pt", row = c("1pt", "1cm"))),
    gutter(
      width(list(abs_length(2, "pt"))),
      width(list(abs_length(1, "pt"), abs_length(1, "cm")))
    )
  )
})

test_that("gutter object prints correctly", {
  expect_snapshot(gutter(
    width(list(ratio(10), fractional_length(1), auto(), em_length(5))),
    auto()
  ))
})
