test_that("relative() works", {
  expect_equal(
    relative(ttables_length(abs_length(1:2, "pt"))),
    new_relative(ttables_length(abs_length(1:2, "pt")), ratio(c(0, 0)))
  )
  expect_equal(
    relative(ratio = ratio(2:3)),
    new_relative(ttables_length(em = em_length(c(0, 0))), ratio(2:3))
  )
  expect_equal(
    relative(ttables_length(abs_length(1:2, c("pt", "mm"))), ratio(50)),
    new_relative(
      ttables_length(abs_length(1:2, c("pt", "mm"))),
      ratio(c(50, 50))
    )
  )
  expect_equal(
    relative(ttables_length(abs_length(1, "cm")), ratio(c(30, 40))),
    new_relative(ttables_length(abs_length(c(1, 1), "cm")), ratio(c(30, 40)))
  )
})

test_that("is_relative() works", {
  expect_true(is_relative(relative(
    ttables_length(abs_length(1, "pt")),
    ratio(50)
  )))
  expect_true(is_relative(relative(ttables_length(abs_length(1, "pt")))))
  expect_true(is_relative(relative(ratio = ratio(50))))
  expect_false(is_relative("1pt"))
  expect_false(is_relative("1%"))
  expect_false(is_relative("1pt + 1%"))
})

test_that("is.na.relative() works", {
  expect_equal(is.na(relative(ttables_length(abs_length(1, "pt")))), FALSE)
  expect_equal(
    is.na(relative(
      ttables_length(
        abs_length(c(2, NA, 3, 4, NA), "cm"),
        em_length(c(2, NA, 3, 4, NA))
      ),
      ratio(c(5, 6, 7, NA, NA))
    )),
    c(FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  expect_equal(is.na(NA_relative_), TRUE)
})

test_that("format.relative() prints correctly", {
  expect_snapshot(relative(
    ttables_length(
      abs_length(c(1:3, NA, NA), c("pt", "mm", "cm", "in", "in")),
      em_length(c(2, NA, 3, 4, NA))
    ),
    ratio(c(NA, 5, 6, 7, NA))
  ))
})

test_that("as_relative() works", {
  expect_equal(
    as_relative("1pt"),
    relative(ttables_length(abs_length(1, "pt")))
  )
  expect_equal(as_relative("2em"), relative(ttables_length(em = em_length(2))))
  expect_equal(
    as_relative("3cm + 4em"),
    relative(ttables_length(abs_length(3, "cm"), em_length(4)))
  )
  expect_equal(as_relative("30%"), relative(ratio = ratio(30)))
  expect_equal(
    as_relative("5cm + 2em + 10%"),
    relative(ttables_length(abs_length(5, "cm"), em_length(2)), ratio(10))
  )
  expect_equal(
    as_relative(c("2mm + 1em + 5%", "10% + 3cm + 0.5em", "4in + 20% + 2em")),
    relative(
      ttables_length(
        abs_length(2:4, c("mm", "cm", "in")),
        em_length(c(1, 0.5, 2))
      ),
      ratio(c(5, 10, 20))
    )
  )
})

test_that("as.character.relative() works", {
  expect_equal(
    as.character(relative(
      ttables_length(
        abs_length(c(1:3, NA, NA), c("pt", "mm", "cm", "in", "in")),
        em_length(c(2, NA, 3, 4, NA))
      ),
      ratio(c(NA, 5, 10, 20, NA))
    )),
    c("1pt + 2em", "5% + 2mm", "10% + 3cm + 3em", "20% + 4em", NA)
  )
})

test_that("conversion works", {
  expect_equal(
    as_relative(abs_length(1, "pt")),
    relative(ttables_length(abs_length(1, "pt")))
  )
  expect_equal(
    as_relative(em_length(2)),
    relative(ttables_length(em = em_length(2)))
  )
  expect_equal(
    as_relative(ttables_length(abs_length(3, "cm"), em_length(4))),
    relative(ttables_length(abs_length(3, "cm"), em_length(4)))
  )
  expect_equal(as_relative(ratio(20)), relative(ratio = ratio(20)))
})

test_that("length arithmetic works", {
  expect_equal(
    relative(ttables_length(
      abs_length(c(1, 2, NA), "cm"),
      em_length(c(3, NA, 4))
    )) +
      relative(ttables_length(
        abs_length(c(NA, 5, 6), "mm"),
        em_length(c(NA, 7, 8))
      )),
    relative(ttables_length(
      abs_length(c(NA, 2.5, 7200 / 2540 * 6), c("cm", "cm", "pt")),
      em_length(c(NA, 7, 12))
    ))
  )
  expect_equal(
    relative(
      ttables_length(
        abs_length(c(1, 2, NA, 3), "cm"),
        em_length(c(3, NA, 4, 5))
      ),
      ratio(c(1, 1, 2, 3))
    ) +
      relative(
        ttables_length(
          abs_length(c(NA, 5, 6, 7), "mm"),
          em_length(c(NA, 7, 8, 9))
        ),
        ratio(c(NA, 2, 3, 4))
      ),
    relative(
      ttables_length(
        abs_length(c(NA, 2.5, 7200 / 2540 * 6, 3.7), c("cm", "cm", "pt", "cm")),
        em_length(c(NA, 7, 12, 14))
      ),
      ratio(c(NA, 3, 5, 7))
    )
  )

  expect_equal(
    relative(ttables_length(abs_length(1, "cm"), em_length(4)), ratio(20)) -
      relative(ttables_length(abs_length(6, "mm"), em_length(2.5)), ratio(5)),
    relative(ttables_length(abs_length(0.4, "cm"), em_length(1.5)), ratio(15))
  )

  expect_equal(
    relative(ttables_length(abs_length(3, "cm"), em_length(1)), ratio(5)) * 2,
    relative(ttables_length(abs_length(6, "cm"), em_length(2)), ratio(10))
  )
  expect_equal(
    relative(ttables_length(abs_length(8, "pt"), em_length(3)), ratio(30)) / 4,
    relative(ttables_length(abs_length(2, "pt"), em_length(3 / 4)), ratio(7.5))
  )

  expect_equal(
    3 * relative(ttables_length(abs_length(3, "mm"), em_length(0.3)), ratio(2)),
    relative(ttables_length(abs_length(9, "mm"), em_length(0.9)), ratio(6))
  )

  expect_equal(
    +relative(ttables_length(abs_length(2, "in"), em_length(2)), ratio(5)),
    relative(ttables_length(abs_length(2, "in"), em_length(2)), ratio(5))
  )
})
