#' @include width.R
NULL

new_gutter <- function(column = width(), row = width()) {
  if (!(is_auto(column) || is_width(column))) {
    rlang::abort("'column' must be `auto` or a `ttables_width` list.")
  }
  if (!(is_auto(row) || is_width(row))) {
    rlang::abort("'row' must be `auto` or a `ttables_width` list.")
  }
  structure(list(column = column, row = row), class = "ttables_gutter")
}

#' Gutter specification
#'
#' Create a new gutter specification
#'
#' Consists of a list of `column` and `row` gutter widths, each a `width` list
#'     of track sizes.
#'
#' @param column,row A `width` list of track sizes, or `auto`.
#' @param x An object to test or convert.
#'
#' @export
gutter <- function(column = width(), row = width()) {
  if (!is_auto(column)) {
    column <- as_width(column)
  }
  if (!is_auto(row)) {
    row <- as_width(row)
  }
  new_gutter(column = column, row = row)
}

#' @export
print.ttables_gutter <- function(x, ...) {
  column <- format(field(x, "column"))
  row <- format(field(x, "row"))

  column.f <- if (length(column)) paste0("  columns: ", paste(column, collapse = ", "))
  row.f <- if (length(row)) paste0("  rows: ", paste(row, collapse = ", "))

  out <- paste0(column.f, if (length(column.f) && length(row.f)) "\n", row.f)
  if (length(out) == 0) {
    out <- "(empty)"
  }

  cat(paste0("gutter:\n", out))
}

#' @rdname gutter
#' @export
is_gutter <- function(x) inherits(x, "ttables_gutter")

#' @rdname gutter
#' @export
as_gutter <- function(x) {
  if (is.null(x)) {
    return(gutter())
  }
  if (!is.list(x)) {
    rlang::abort("'x' must be a list")
  }
  if (!any(c("column", "row") %in% names(x))) {
    rlang::abort("'x' must have 'column' and/or 'row' elements")
  }
  gutter(x$column, x$row)
}

#' @export
vec_ptype_abbr.ttables_gutter <- function(x, ...) "gttr"
#' @export
vec_ptype_full.ttables_gutter <- function(x, ...) "gutter"
#' @export
vec_ptype2.ttables_gutter.character <- function(x, y, ...) character()
#' @export
vec_cast.character.ttables_gutter <- function(x, to, ...) format(x)
