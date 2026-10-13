test_that("horiz_alignment() works", {
  expect_equal(horiz_alignment("left"), new_horiz_alignment("left"))
  expect_equal(
    horiz_alignment(c("right", "centre", "left", "end", "start")),
    new_horiz_alignment(c("right", "center", "left", "end", "start"))
  )
})

test_that("horiz_alignment() errors correctly", {
  expect_error(horiz_alignment("top"))
  expect_error(horiz_alignment(1))
})

test_that("is_horiz_alignment() works", {
  expect_true(is_horiz_alignment(horiz_alignment("left")))
  expect_false(is_horiz_alignment("left"))
})

test_that("is.na.ttables_horiz_alignment() works", {
  expect_equal(is.na(horiz_alignment("center")), FALSE)
  expect_equal(
    is.na(horiz_alignment(c("left", NA, "end"))),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(is.na(NA_horiz_alignment_), TRUE)
})

test_that("format.ttables_horiz_alignment() prints correctly", {
  expect_snapshot(horiz_alignment(c("right", "centre", NA, "start")))
})

test_that("as_horiz_alignment() works", {
  expect_equal(as_horiz_alignment("left"), horiz_alignment("left"))
  expect_equal(
    as_horiz_alignment(c("centre", "right", "center", "start", "end")),
    horiz_alignment(c("center", "right", "centre", "start", "end"))
  )
})

test_that("as.character.ttables_horiz_alignment() works", {
  expect_equal(
    as.character(horiz_alignment(c("centre", "right", "left", "start", "end"))),
    c("center", "right", "left", "start", "end")
  )
})

test_that("vert_alignment() works", {
  expect_equal(vert_alignment("top"), new_vert_alignment("top"))
  expect_equal(
    vert_alignment(c("bottom", "horizon", "top")),
    new_vert_alignment(c("bottom", "horizon", "top"))
  )
})

test_that("vert_alignment() errors correctly", {
  expect_error(vert_alignment("left"))
  expect_error(vert_alignment(1))
})

test_that("is_vert_alignment() works", {
  expect_true(is_vert_alignment(vert_alignment("top")))
  expect_false(is_vert_alignment("top"))
})

test_that("is.na.ttables_vert_alignment() works", {
  expect_equal(is.na(vert_alignment("horizon")), FALSE)
  expect_equal(
    is.na(vert_alignment(c("top", NA, "bottom"))),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(is.na(NA_vert_alignment_), TRUE)
})

test_that("format.ttables_vert_alignment() prints correctly", {
  expect_snapshot(vert_alignment(c("bottom", "horizon", NA, "top")))
})

test_that("as_vert_alignment() works", {
  expect_equal(as_vert_alignment("top"), vert_alignment("top"))
  expect_equal(
    as_vert_alignment(c("horizon", "bottom", "horizon")),
    vert_alignment(c("horizon", "bottom", "horizon"))
  )
})

test_that("as.character.ttables_vert_alignment() works", {
  expect_equal(
    as.character(vert_alignment(c("horizon", "bottom", "top"))),
    c("horizon", "bottom", "top")
  )
})

test_that("alignment() works", {
  expect_equal(
    alignment("left"),
    new_alignment(horiz_alignment("left"), NA_vert_alignment_)
  )
  expect_equal(
    alignment(vert = "top"),
    new_alignment(NA_horiz_alignment_, vert_alignment("top"))
  )
  expect_equal(
    alignment(c("left", "right", "centre"), c("bottom", "horizon", "top")),
    new_alignment(
      horiz_alignment(c("left", "right", "center")),
      vert_alignment(c("bottom", "horizon", "top"))
    )
  )
})

test_that("alignment() errors correctly", {
  expect_error(alignment("top"))
  expect_error(alignment(vert = "left"))
  expect_error(alignment(1))
  expect_error(alignment(vert = 1))
})

test_that("is_alignment() works", {
  expect_true(is_alignment(alignment("left")))
  expect_false(is_alignment("left"))
})

test_that("is.na.ttables_alignment() works", {
  expect_equal(is.na(alignment("center")), FALSE)
  expect_equal(
    is.na(alignment(c("right", NA, "left"))),
    c(FALSE, TRUE, FALSE)
  )
  expect_equal(is.na(NA_vert_alignment_), TRUE)
})

test_that("format.ttables_alignment() prints correctly", {
  expect_snapshot(alignment(
    c("left", NA, "right", "centre"),
    c("bottom", "horizon", NA, "top")
  ))
})

test_that("as_alignment() works", {
  expect_equal(as_alignment("left"), alignment("left"))
  expect_equal(as_alignment("top"), alignment(vert = "top"))
  expect_equal(
    as_alignment(c("left + bottom", "center + top", "right + horizon")),
    alignment(c("left", "center", "right"), c("bottom", "top", "horizon"))
  )
})

test_that("as.character.ttables_alignment() works", {
  expect_equal(
    as.character(alignment(
      c("left", "center", "right"),
      c("bottom", "top", "horizon")
    )),
    c("left + bottom", "center + top", "right + horizon")
  )
})
