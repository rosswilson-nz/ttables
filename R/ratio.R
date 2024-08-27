new_ratio <- function(x = double()) {
  if (!rlang::is_double(x)) rlang::abort("'x' must be a double vector.")
  new_vctr(x, class = "ttables_ratio")
}
ratio <- function(x = double()) {
  x <- vec_cast(x, double())
  new_ratio(x)
}
is_ratio <- function(x) inherits(x, "ttables_ratio")
NA_ratio_ <- new_ratio(NA_real_)
#' @export
format.ttables_ratio <- function(x, ...) {
  out <- formatC(vec_data(x), ...)
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}
#' @export
vec_ptype_abbr.ttables_ratio <- function(x, ...) "rtio"
#' @export
vec_ptype_full.ttables_ratio <- function(x, ...) "ratio"
#' @export
vec_cast.ttables_ratio.double <- function(x, to, ...) ratio(x)
#' @export
vec_cast.double.ttables_ratio <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.ttables_ratio.character <- function(x, to, ...) {
  n <- nchar(x)
  unit <- substr(x, n, n)
  if (!all(unit == "%" | is.na(x))) rlang::abort("Invalid ratio")
  value <- gsub("%$", "", x)
  #value <- rlang::try_fetch(as.numeric(value), warning = \(w) rlang::abort("Invalid relative length", parent = w))
  value <- gsub("[[:blank:]]", "", value)
  value <- as.numeric(value)
  new_ratio(value)
}
#' @export
vec_cast.character.ttables_ratio <- function(x, to, ...) format(x)
as_ratio <- function(x) vec_cast(x, new_ratio())
#' @export
#' @method vec_arith ttables_ratio
vec_arith.ttables_ratio <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_ratio", y)
}
#' @export
#' @method vec_arith.ttables_ratio default
vec_arith.ttables_ratio.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.ttables_ratio ttables_ratio
vec_arith.ttables_ratio.ttables_ratio <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_ratio(vec_arith_base(op, x, y)),
    "/" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_ratio numeric
vec_arith.ttables_ratio.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = new_ratio(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.numeric ttables_ratio
vec_arith.numeric.ttables_ratio <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_ratio(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_ratio MISSING
vec_arith.ttables_ratio.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}
