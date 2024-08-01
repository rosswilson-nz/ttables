span <- function(x, start, size, combine = NA) {
  start <- resolve_location(start, x)

  new_span <- tibble(start, size = list(size), combine)
  x$`_layout` <- tibble::add_row(x$`_layout`, new_span)
  x
}

# `as_typst()` (and similar) will first apply formatting according to the individual cells, then
# create cell spans by concatenating cell contents (if `!is.na(combine)`) or keeping only the top-left cell (if `is.na(combine)`)

colspan <- function(x, start, ncols, combine = NA) {
  size <- list(rows = 1, cols = ncols)
  span(x, start, size, combine)
}

rowspan <- function(x, start, nrows, combine = NA) {
  size <- list(rows = nrows, cols = 1)
  span(x, start, size, combine)
}
