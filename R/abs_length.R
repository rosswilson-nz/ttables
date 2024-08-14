new_abs_length <- function(x = double(), unit = character()) {
  if (!rlang::is_double(x)) rlang::abort("'x' must be a double vector.")
  if (!rlang::is_character(unit)) rlang::abort("'unit' must be a character vector.")
  if (!all(unit %in% c("pt", "mm", "cm", "in") | is.na(x))) rlang::abort("'unit' must be one of c('pt', 'mm', 'cm', 'in').")
  new_rcrd(list(value = x, unit = unit), class = "ttables_abs_length")
}
abs_length <- function(x = double(), unit = character()) {
  x <- vec_cast(x, double())
  unit <- vec_cast(unit, character())
  inputs <- vec_recycle_common(x, unit)
  new_abs_length(inputs[[1]], inputs[[2]])
}
is_abs_length <- function(x) inherits(x, "ttables_abs_length")
#' @export
is.na.ttables_abs_length <- function(x) is.na(field(x, "value"))
NA_abs_length_ <- new_abs_length(NA_real_, NA_character_)
#' @export
format.ttables_abs_length <- function(x, ...) {
  value <- field(x, "value")
  unit <- field(x, "unit")
  out.value <- vapply(value, format, character(1))
  out <- paste0(out.value, unit)
  out[is.na(value) | is.na(unit)] <- NA
  out
}
#' @export
vec_ptype_abbr.ttables_abs_length <- function(x, ...) "abslen"
#' @export
vec_ptype_full.ttables_abs_length <- function(x, ...) "abs_length"
#' @export
vec_ptype2.ttables_abs_length.ttables_abs_length <- function(x, y, ...) new_abs_length()
#' @export
vec_ptype2.ttables_abs_length.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ttables_abs_length <- function(x, y, ...) character()
#' @export
vec_cast.ttables_abs_length.character <- function(x, to, ...) {
  n <- nchar(x)
  unit <- substr(x, n - 1, n)
  if (!all(unit %in% c("pt", "mm", "cm", "in") | is.na(x))) rlang::abort("Invalid length")
  value <- gsub("(pt|mm|cm|in)$", "", x)
  #value <- rlang::try_fetch(as.numeric(value), warning = \(w) rlang::abort("Invalid length", parent = w))
  value <- gsub("[[:blank:]]", "", value)
  value <- as.numeric(value)
  abs_length(value, unit)
}
#' @export
vec_cast.character.ttables_abs_length <- function(x, to, ...) format(x)
#' @export
vec_cast.double.ttables_abs_length <- function(x, to, ...) field(x, "value")
#' @export
vec_cast.ttables_abs_length.ttables_abs_length <- function(x, to, ...) x
as_abs_length <- function(x) vec_cast(x, new_abs_length())
convert_units <- function(x, from, to) {
  ratios <- c(pt = 7200, mm = 2540, cm = 254, "in" = 100)
  unname(x * (ratios[to] / ratios[from]))
}
#' @export
vec_proxy_equal.ttables_abs_length <- function(x, ...) {
  value <- field(x, "value")
  unit <- field(x, "unit")
  value <- convert_units(value, unit, "pt")
  unit <- rep(NA_character_, length(value))
  unit[!is.na(value)] <- "pt"
  data.frame(value = value, unit = unit)
}
#' @export
#' @method vec_arith ttables_abs_length
vec_arith.ttables_abs_length <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_abs_length", y)
}
#' @export
#' @method vec_arith.ttables_abs_length default
vec_arith.ttables_abs_length.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.ttables_abs_length ttables_abs_length
vec_arith.ttables_abs_length.ttables_abs_length <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_abs_length(vec_arith_base(op, field(x, "value"), field(vec_cast(y, x), "value")), unit = field(x, "unit")),
    "/" = vec_arith_base(op, x, vec_cast(y, x)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_abs_length numeric
vec_arith.ttables_abs_length.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = new_abs_length(vec_arith_base(op, field(x, "value"), y), field(x, "unit")),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.numeric ttables_abs_length
vec_arith.numeric.ttables_abs_length <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_abs_length(vec_arith_base(op, x, field(y, "value")), field(y, "unit")),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_abs_length MISSING
vec_arith.ttables_abs_length.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}
