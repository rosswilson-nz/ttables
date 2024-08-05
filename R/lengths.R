#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name ttables-vctrs
NULL

new_abs_length <- function(x = double(), unit = "mm") {
  if (!rlang::is_double(x)) rlang::abort("'x' must be a double vector.")
  if (!rlang::is_scalar_character(unit)) rlang::abort("'unit' must be a character scalar")
  if (!(unit %in% c("pt", "mm", "cm", "in"))) rlang::abort("'unit' must be one of c('pt', 'mm', 'cm', 'in')")
  new_vctr(x, unit = unit, class = "ttables_abs_length")
}
abs_length <- function(x = double(), unit = "mm") {
  x <- vec_cast(x, double())
  new_abs_length(x, unit)
}
is_abs_length <- function(x) inherits(x, "ttables_abs_length")
unit <- function(x) attr(x, "unit")
format.ttables_abs_length <- function(x, ...) {
  out <- formatC(vec_data(x), ...)
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], unit(x))
  out
}
vec_ptype_abbr.ttables_abs_length <- function(x, ...) "abslen"
vec_ptype_full.ttables_abs_length <- function(x, ...) "abs_length"
vec_ptype2.ttables_abs_length.ttables_abs_length <- function(x, y, ...) new_abs_length()
vec_cast.ttables_abs_length.double <- function(x, to, ...) abs_length(x)
vec_cast.double.ttables_abs_length <- function(x, to, ...) vec_data(x)
vec_cast.ttables_abs_length.ttables_abs_length <- function(x, to, ...) {
  if (identical(unit(x), unit(to))) x else {
    y <- convert_units(vec_data(x), from = unit(x), to = unit(to))
    new_abs_length(y, unit(to))
  }
}
convert_units <- function(x, from, to) {
  from <- switch(from, pt = 72, mm = 25.4, cm = 2.54, "in" = 1)
  to <- switch(to, pt = 72, mm = 25.4, cm = 2.54, "in" = 1)
  x / from * to
}
vec_proxy_equal.ttables_abs_length <- function(x, ...) convert_units(vec_data(x), unit(x), "mm")
vec_arith.ttables_abs_length <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_abs_length", y)
}
vec_arith.ttables_abs_length.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
vec_arith.ttables_abs_length.ttables_abs_length <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_abs_length(vec_arith_base(op, x, vec_cast(y, x)), unit = unit(x)),
    "/" = vec_arith_base(op, x, vec_cast(y, x)),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.ttables_abs_length.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = new_abs_length(vec_arith_base(op, x, y), unit(x)),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.numeric.ttables_abs_length <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_abs_length(vec_arith_base(op, x, y), unit(y)),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.ttables_abs_length.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}

new_length <- function(abs = abs_length(), rel = double()) {
  if (!is_abs_length(abs)) rlang::abort("'abs' must be an absolute length (`ttables_abs_length`) vector.")
  if (!rlang::is_double(rel)) rlang::abort("'rel' must be a double vector.")
  new_rcrd(list(abs = abs, rel = rel), class = "ttables_length")
}
ttables_length <- function(abs = abs_length(), rel = double()) {
  rel <- as.double(rel)
  inputs <- vec_recycle_common(abs, rel)
  new_length(inputs[[1]], inputs[[2]])
}
format.ttables_length <- function(x, ...) {
  abs <- field(x, "abs")
  rel <- field(x, "rel")
  out <- paste0(format(abs), " + ", rel, "em")
  out[is.na(abs) | is.na(rel)] <- NA

  out
}
is.na.ttables_length <- function(x) is.na(field(x, "abs")) | is.na(field(x, "rel"))
vec_ptype_abbr.ttables_length <- function(x, ...) "len"
vec_ptype_full.ttables_length <- function(x, ...) "length"
vec_ptype2.ttables_length.ttables_length <- function(x, y, ...) new_length()
vec_cast.ttables_length.ttables_length <- function(x, to, ...) {
  if (identical(unit(field(x, "abs")), unit(field(to, "abs")))) x else {
    abs <- vec_cast(field(x, "abs"), field(to, "abs"))
    new_length(abs, field(x, "rel"))
  }
}
vec_proxy_compare.ttables_length <- function(x, ...) {
  rlang::abort("Comparison of `ttables_length` objects is not implemented.")
}
vec_arith.ttables_length <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_length", y)
}
vec_arith.ttables_length.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
vec_arith.ttables_length.ttables_length <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_length(vec_arith(op, field(x, "abs"), field(y, "abs")),
                     vec_arith_base(op, field(x, "rel"), field(y, "rel"))),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.ttables_length.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = new_length(vec_arith(op, field(x, "abs"), y),
                     vec_arith_base(op, field(x, "rel"), y)),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.numeric.ttables_length <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_length(vec_arith(op, x, field(y, "abs")),
                     vec_arith_base(op, x, field(y, "rel"))),
    stop_incompatible_op(op, x, y)
  )
}
vec_arith.ttables_length.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}

#abs_length <- function(x, unit) new_vctr(x, unit = unit, class = "abs_length")
#rel_length <- function(x) new_vctr(x, class = "rel_length")
#new_length <- function(abs = "0pt", rel = "0em") new_rcrd(list(abs = abs, rel = rel), class = "length")

#' @export
#' @method vec_arith length
vec_arith.length <- function(op, x, y, ...) {
  UseMethod("vec_arith.length", y)
}

#' @export
#' @method vec_arith.length default
vec_arith.length.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.length length
vec_arith.length.length <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_length(abs = vec_arith_base(op, x$abs, y$abs),
                     rel = vec_arith_base(op, x$rel, y$rel)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith abs_length
vec_arith.abs_length <- function(op, x, y, ...) {
  UseMethod("vec_arith.abs_length", y)
}

#' @export
#' @method vec_arith.abs_length default
vec_arith.abs_length.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.abs_length abs_length
vec_arith.abs_length.abs_length <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = if (identical(attr(x, "unit"), attr(y, "unit"))) abs_length(vec_arith_base(op, x, y), attr(x, "unit")) else abs_length(vec_arith_base(op, as_mm(x), as_mm(y)), "mm"),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

as_mm <- function(x) {
  switch(attr(x, "unit"),
         pt = 25.4 / 72,
         mm = 1,
         cm = 10,
         "in" = 25.4) * as.numeric(x)
}
