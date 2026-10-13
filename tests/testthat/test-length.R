test_that("ttables_length() works", {
  expect_equal(
    ttables_length(abs_length(1:2, "pt")),
    new_length(abs_length(1:2, "pt"), em_length(c(0, 0)))
  )
  expect_equal(
    ttables_length(em = em_length(2:3)),
    new_length(abs_length(c(0, 0), "pt"), em_length(2:3))
  )
  expect_equal(
    ttables_length(abs_length(1:2, c("pt", "mm")), em_length(3)),
    new_length(abs_length(1:2, c("pt", "mm")), em_length(c(3, 3)))
  )
  expect_equal(
    ttables_length(abs_length(1, "cm"), em_length(2:3)),
    new_length(abs_length(c(1, 1), c("cm", "cm")), em_length(2:3))
  )
  expect_equal(
    ttables_length(abs_length(1:2, c("mm", "in")), em_length(3:4)),
    new_length(abs_length(1:2, c("mm", "in")), em_length(3:4))
  )
})

test_that("ttables_length() errors correctly", {
  expect_error(ttables_length(1))
  expect_error(ttables_length("1pt"))
  expect_error(ttables_length(em = 2))
  expect_error(ttables_length(em = "2em"))
})

test_that("is_length() works", {
  expect_true(is_length(ttables_length(abs_length(1, "pt"), em_length(1))))
  expect_true(is_length(ttables_length(abs_length(1, "pt"))))
  expect_true(is_length(ttables_length(em = em_length(1))))
  expect_false(is_length("1pt"))
  expect_false(is_length("1em"))
  expect_false(is_length("1pt + 1em"))
})

test_that("is.na.ttables_length() works", {
  expect_equal(is.na(ttables_length(abs_length(1, "pt"))), FALSE)
  expect_equal(
    is.na(ttables_length(
      abs_length(c(2, NA, 3, 4, NA), "cm"),
      em_length(c(5, 6, 7, NA, NA))
    )),
    c(FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  expect_equal(is.na(NA_length_), TRUE)
})

test_that("format.ttables_length() prints correctly", {
  expect_snapshot(ttables_length(
    abs_length(c(1:3, NA, NA), c("pt", "mm", "cm", "in", "in")),
    em_length(c(2, NA, 3, 4, NA))
  ))
})

test_that("as_length() works", {
  expect_equal(as_length("1pt"), ttables_length(abs_length(1, "pt")))
  expect_equal(as_length("2em"), ttables_length(em = em_length(2)))
  expect_equal(
    as_length("3cm + 4em"),
    ttables_length(abs_length(3, "cm"), em_length(4))
  )
  expect_equal(
    as_length(c("2mm + 1em", "3cm + 0.5em", "4in + 2em")),
    ttables_length(
      abs_length(2:4, c("mm", "cm", "in")),
      em_length(c(1, 0.5, 2))
    )
  )
})

test_that("as.character.ttables_length() works", {
  expect_equal(
    as.character(ttables_length(
      abs_length(c(1:3, NA, NA), c("pt", "mm", "cm", "in", "in")),
      em_length(c(2, NA, 3, 4, NA))
    )),
    c("1pt + 2em", "2mm", "3cm + 3em", "4em", NA)
  )
})

test_that("conversion works", {
  expect_equal(
    as_length(abs_length(1, "pt")),
    ttables_length(abs_length(1, "pt"))
  )
  expect_equal(
    as_length(em_length(2)),
    ttables_length(em = em_length(2))
  )
})

test_that("length arithmetic works", {
  expect_equal(
    ttables_length(abs_length(1, "pt")) + ttables_length(abs_length(2, "pt")),
    ttables_length(abs_length(3, "pt"))
  )
  expect_equal(
    ttables_length(em = em_length(1:2)) + ttables_length(em = em_length(2:3)),
    ttables_length(em = em_length(c(3, 5)))
  )
  expect_equal(
    ttables_length(abs_length(c(1, 2, NA), "cm"), em_length(c(3, NA, 4))) +
      ttables_length(abs_length(c(NA, 5, 6), "mm"), em_length(c(NA, 7, 8))),
    ttables_length(
      abs_length(c(NA, 2.5, 7200 / 2540 * 6), c("cm", "cm", "pt")),
      em_length(c(NA, 7, 12))
    )
  )

  expect_equal(
    ttables_length(abs_length(1, "pt")) - ttables_length(abs_length(2, "pt")),
    -ttables_length(abs_length(1, "pt"))
  )
  expect_equal(
    ttables_length(abs_length(1, "cm"), em_length(4)) -
      ttables_length(abs_length(6, "mm"), em_length(2.5)),
    ttables_length(abs_length(0.4, "cm"), em_length(1.5))
  )

  expect_equal(
    ttables_length(abs_length(3, "cm"), em_length(1)) * 2,
    ttables_length(abs_length(6, "cm"), em_length(2))
  )
  expect_equal(
    ttables_length(abs_length(8, "pt"), em_length(3)) / 4,
    ttables_length(abs_length(2, "pt"), em_length(3 / 4))
  )

  expect_equal(
    3 * ttables_length(abs_length(3, "mm"), em_length(0.3)),
    ttables_length(abs_length(9, "mm"), em_length(0.9))
  )

  expect_equal(
    +ttables_length(abs_length(2, "in"), em_length(2)),
    ttables_length(abs_length(2, "in"), em_length(2))
  )
})
