new_fractional_length <- function(x = double()) {
  if (!rlang::is_double(x)) rlang::abort("'x' must be a double vector.")
  new_vctr(x, class = "ttables_fractional_length")
}
fractional_length <- function(x = double()) {
  x <- vec_cast(x, double())
  new_fractional_length(x)
}
is_fractional_length <- function(x) inherits(x, "ttables_fractional_length")
NA_fractional_length_ <- new_fractional_length(NA_real_)
#'@export
format.ttables_fractional_length <- function(x, ...) {
  out <- formatC(vec_data(x), ...)
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "fr")
  out
}
#'@export
vec_ptype_abbr.ttables_fractional_length <- function(x, ...) "frlen"
#'@export
vec_ptype_full.ttables_fractional_length <- function(x, ...) "frac_length"
#'@export
vec_cast.ttables_fractional_length.double <- function(x, to, ...) fractional_length(x)
#'@export
vec_cast.double.ttables_fractional_length <- function(x, to, ...) vec_data(x)
#'@export
vec_cast.ttables_fractional_length.character <- function(x, to, ...) as_fractional_length(x)
as_fractional_length <- function(x) {
  n <- nchar(x)
  unit <- substr(x, n - 1, n)
  if (!all(substr(x, n - 1, n) == "fr")) rlang::abort("Invalid fractional length")
  value <- gsub("fr$", "", x)
  #value <- rlang::try_fetch(as.numeric(value), warning = \(w) rlang::abort("Invalid fractional length", parent = w))
  value <- as.numeric(value)
  new_fractional_length(value)
}
#'@export
#' @method vec_arith ttables_fractional_length
vec_arith.ttables_fractional_length <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_fractional_length", y)
}
#'@export
#' @method vec_arith.ttables_fractional_length default
vec_arith.ttables_fractional_length.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#'@export
#' @method vec_arith.ttables_fractional_length ttables_fractional_length
vec_arith.ttables_fractional_length.ttables_fractional_length <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_fractional_length(vec_arith_base(op, x, y)),
    "/" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}
#'@export
#' @method vec_arith.ttables_fractional_length numeric
vec_arith.ttables_fractional_length.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = new_fractional_length(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}
#'@export
#' @method vec_arith.numeric ttables_fractional_length
vec_arith.numeric.ttables_fractional_length <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_fractional_length(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}
#'@export
#' @method vec_arith.ttables_fractional_length MISSING
vec_arith.ttables_fractional_length.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}
