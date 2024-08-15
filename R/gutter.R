#' @include relative.R auto.R fractional-length.R
NULL

new_gutter <- function(column = relative(), row = relative()) {
  if (!(is_auto(column) || is_relative(column) || is_fractional_length(column)))
    rlang::abort("'column' must be a relative length (`ttables_relative`) or fractional length (`ttables_fractional_length`) vector.")
  if (!(is_auto(row) || is_relative(row) || is_fractional_length(row)))
    rlang::abort("'row' must be a relative length (`ttables_relative`) or fractional length (`ttables_fractional_length`) vector.")
  new_rcrd(list(column = column, row = row), class = "ttables_gutter")
}
gutter <- function(column = relative(), row = relative()) {
  if (!is_fractional_length(column)) column <- vec_cast(column, relative())
  if (!is_fractional_length(row)) row <- vec_cast(row, relative())
  if (length(column) == 0 && length(row)) column <- rep(NA_relative_, length(row))
  if (length(row) == 0 && length(column)) row <- rep(NA_relative_, length(column))
  inputs <- vec_recycle_common(column, row)
  new_gutter(column = inputs[[1]], row = inputs[[2]])
}
#' @export
format.ttables_gutter <- function(x, ...) {
  column <- format(field(x, "column"))
  row <- format(field(x, "row"))

  column.f <- paste0("column: ", format(column))
  column.f[is.na(column)] <- NA_character_
  row.f <- paste0("row: ", format(row))
  row.f[is.na(row)] <- NA_character_

  out <- paste(column.f, row.f, sep = ", ")
  out[is.na(column.f)] <- row.f[is.na(column.f)]
  out[is.na(row.f)] <- column.f[is.na(row.f)]
  out[is.na(column.f) & is.na(row.f)] <- NA

  out
}
is_gutter <- function(x) inherits(x, "ttables_gutter")
#' @export
is.na.ttables_gutter <- function(x) vec_detect_missing(x)
NA_gutter_ <- gutter(NA_relative_, NA_relative_)
as_gutter <- function(x) vec_cast(x, new_gutter())
#' @export
vec_ptype_abbr.ttables_gutter <- function(x, ...) "gttr"
#' @export
vec_ptype_full.ttables_gutter <- function(x, ...) "gutter"
#' @export
vec_ptype2.ttables_gutter.character <- function(x, y, ...) character()
#' @export
vec_cast.character.ttables_gutter <- function(x, to, ...) format(x)
