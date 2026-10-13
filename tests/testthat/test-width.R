test_that("width() works", {
  expect_equal(
    width(list(ttables_length(abs_length(5, "cm")))),
    new_width(list(ttables_length(abs_length(5, "cm"))))
  )
  expect_equal(em_length(2:3), new_em_length(c(2, 3)))
  expect_equal(
    width(list(ttables_length(abs_length(1, "cm")), auto(), ratio(10), fractional_length(1))),
    new_width(list(ttables_length(abs_length(1, "cm")), auto(), ratio(10), fractional_length(1)))
  )
})

test_that("width() errors correctly", {
  expect_error(width(auto()))
  expect_error(width(ratio(10)))
  expect_error(width(list("1pt")))
})

test_that("is_width() works", {
  expect_true(is_width(width(list(em_length(1)))))
  expect_false(is_width("1em"))
})

test_that("format.ttables_width() prints correctly", {
  expect_snapshot(
    width(list(ttables_length(abs_length(1, "cm")), auto(), ratio(10), fractional_length(1)))
  )
})

test_that("as_width() works", {
  expect_equal(
    as_width(c("1cm", "auto", "10%", "1fr")),
    width(list(
      relative(ttables_length(abs_length(1, "cm"))),
      auto(),
      relative(ratio = ratio(10)),
      fractional_length(1)
    ))
  )
  expect_equal(
    as_width(c(1, 2, 5)),
    width(list(fractional_length(1), fractional_length(2), fractional_length(5)))
  )
  expect_equal(
    as_width(fractional_length(c(1, 2, 5))),
    width(list(fractional_length(1), fractional_length(2), fractional_length(5)))
  )
  expect_error(as_width(c("1")))
})

test_that("as.character.ttables_width() works", {
  expect_equal(
    as.character(width(list(
      ttables_length(abs_length(1, "cm")),
      auto(),
      ratio(10),
      fractional_length(1)
    ))),
    c("1cm", "auto", "10%", "1fr")
  )
})
