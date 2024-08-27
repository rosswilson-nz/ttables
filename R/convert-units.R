# Base convert_units() function
convert_units <- function(x, from, to) {
  ratios <- c(pt = 7200, mm = 2540, cm = 254, "in" = 100)
  unname(x * (ratios[to] / ratios[from]))
}

# General conversion function
as_unit <- function(x, to) {
  UseMethod("as_unit", x)
}
#' @export
as_unit.ttables_abs_length <- function(x, to) {
  if (!rlang::is_character(to) || !all(to %in% c("pt", "mm", "cm", "in"))) rlang::abort("Invalid 'to'")
  if (!(length(to) %in% c(1, length(x)))) rlang::abort("'to' must be a scalar or the same length as 'x'")
  value <- field(x, "value")
  from <- field(x, "unit")
  convert_units(value, from, to)
}
#' @export
as_unit.ttables_length <- function(x, to) {
  if (!rlang::is_scalar_character(to) || !(to %in% c("pt", "mm", "cm", "in")))
    rlang::abort("Invalid 'to'")
  em <- field(x, "em")
  if (all(is.na(em) | as.character(em) == "0em")) {
    as_unit(field(x, "abs"), to)
  } else rlang::abort("as_unit() does not work with non-zero em units")
}
#' @export
as_unit.ttables_relative <- function(x, to) {
  if (!rlang::is_scalar_character(to) || !(to %in% c("pt", "mm", "cm", "in")))
    rlang::abort("Invalid 'to'")
  ratio <- field(x, "ratio")
  if (all(is.na(ratio) | as.character(ratio) == "0%")) {
    as_unit(field(x, "length"), to)
  } else rlang::abort("as_unit() does not work with non-zero ratio component")
}

# Wrappers for each length unit
as_pt <- function(x) as_unit(x, "pt")
as_mm <- function(x) as_unit(x, "mm")
as_cm <- function(x) as_unit(x, "cm")
as_in <- function(x) as_unit(x, "in")

