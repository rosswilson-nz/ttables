#' Combine table cells spanning multiple columns and/or rows
#'
#' @param x A Typst table
#' @param start Top-left cell of the new span. Should be a `cells_location`
#'     object created by [cells()].
#' @param size A list with elements `row` and `col` giving the number of rows
#'     and columns the combined cell should span.
#' @param combine How the existing contents of the grouped cells should be
#'     combined. Either `NA` to keep only the top-left cell or a character
#'     string used to separate the contents of the individual cells. (This may
#'     be revised in future to allow more a flexible specification, using e.g.
#'     `glue` syntax.)
#'
#' @returns A Typst table with the combined cell span added.
#' @export
span <- function(x, start, size, combine = NA_character_) {
  start <- expand_location(resolve_location(start, x))

  new_span <- tibble::tibble(start, size = list(size), combine = as.character(combine))
  x$`_layout` <- tibble::add_row(x$`_layout`, new_span)
  x
}

#' @param ncols The number of columns the combined cell should span
#' @rdname span
#' @export
colspan <- function(x, start, ncols, combine = NA_character_) {
  size <- list(rows = 1, cols = ncols)
  span(x, start, size, combine)
}

#' @param nrows The number of rows the combined cell should span
#' @rdname span
#' @export
rowspan <- function(x, start, nrows, combine = NA_character_) {
  size <- list(rows = nrows, cols = 1)
  span(x, start, size, combine)
}
