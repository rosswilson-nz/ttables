new_em_length <- function(x = double()) {
  if (!rlang::is_double(x)) rlang::abort("'x' must be a double vector.")
  new_vctr(x, class = "ttables_em_length")
}
em_length <- function(x = double()) {
  x <- vec_cast(x, double())
  new_em_length(x)
}
is_em_length <- function(x) inherits(x, "ttables_em_length")
NA_em_length_ <- new_em_length(NA_real_)
#' @export
format.ttables_em_length <- function(x, ...) {
  out <- formatC(vec_data(x), ...)
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "em")
  out
}
#' @export
vec_ptype_abbr.ttables_em_length <- function(x, ...) "em"
#' @export
vec_ptype_full.ttables_em_length <- function(x, ...) "em_length"
#' @export
vec_cast.ttables_em_length.double <- function(x, to, ...) em_length(x)
#' @export
vec_cast.double.ttables_em_length <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.ttables_em_length.character <- function(x, to, ...) as_em_length(x)
as_em_length <- function(x) {
  n <- nchar(x)
  unit <- substr(x, n - 1, n)
  if (!all(unit == "em" | is.na(x))) rlang::abort("Invalid em length")
  value <- gsub("em$", "", x)
  #value <- rlang::try_fetch(as.numeric(value), warning = \(w) rlang::abort("Invalid em length", parent = w))
  value <- gsub("[[:blank:]]", "", value)
  value <- as.numeric(value)
  new_em_length(value)
}
#' @export
#' @method vec_arith ttables_em_length
vec_arith.ttables_em_length <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_em_length", y)
}
#' @export
#' @method vec_arith.ttables_em_length default
vec_arith.ttables_em_length.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.ttables_em_length ttables_em_length
vec_arith.ttables_em_length.ttables_em_length <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_em_length(vec_arith_base(op, x, y)),
    "/" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_em_length numeric
vec_arith.ttables_em_length.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = new_em_length(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.numeric ttables_em_length
vec_arith.numeric.ttables_em_length <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_em_length(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_em_length MISSING
vec_arith.ttables_em_length.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}
