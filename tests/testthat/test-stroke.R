test_that("stroke() works", {
  expect_equal(stroke("red"), new_stroke(colour("red"), NA_length_, NA_dash_))
  expect_equal(
    stroke(thickness = "1pt"),
    new_stroke(NA_colour_, ttables_length(abs_length(1, "pt")), NA_dash_)
  )
  expect_equal(stroke(pattern = "dashed"), new_stroke(NA_colour_, NA_length_, dash("dashed")))
  expect_equal(
    stroke("green", "2cm", "solid"),
    new_stroke(colour("green"), ttables_length(abs_length(2, "cm")), dash("solid"))
  )
})

test_that("stroke() errors correctly", {
  expect_error(stroke(1))
  expect_error(stroke(thickness = "dotted"))
  expect_error(stroke(pattern = "blue"))
})

test_that("is_stroke() works", {
  expect_true(is_stroke(stroke("yellow")))
  expect_false(is_stroke("yellow"))
})

test_that("is.na.stroke() works", {
  expect_equal(is.na(stroke("maroon")), FALSE)
  expect_equal(is.na(stroke(NA_colour_)), TRUE)
  expect_equal(is.na(NA_stroke_), TRUE)
})

test_that("stroke object prints correctly", {
  expect_snapshot(stroke("red"))
  expect_snapshot(stroke(thickness = "1pt"))
  expect_snapshot(stroke(pattern = "dashed"))
  expect_snapshot(stroke("yellow", "1cm"))
  expect_snapshot(stroke("yellow", pattern = "dotted"))
  expect_snapshot(stroke(thickness = "5mm", pattern = "solid"))
  expect_snapshot(stroke("blue", ".5in", "loosely-dashed"))
})

test_that("as_stroke() works", {
  expect_equal(as_stroke("black"), stroke("black"))
  expect_equal(as_stroke("1pt"), stroke(thickness = "1pt"))
  expect_equal(as_stroke("dashed"), stroke(pattern = "dashed"))
  expect_equal(as_stroke("red + 2cm + solid"), stroke("red", "2cm", "solid"))
  expect_equal(as_stroke("2cm + solid + red"), stroke("red", "2cm", "solid"))
})

test_that("as.character.stroke() works", {
  expect_equal(
    as.character(stroke("blue", ".5in", "loosely-dashed")),
    "blue + 0.5in + loosely-dashed"
  )
})
