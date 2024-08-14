#' @include length.R ratio.R

new_relative <- function(length = new_length(), ratio = new_ratio()) {
  if (!inherits(length, "ttables_length")) rlang::abort("'length' must be a length (`ttables_length`) vector.")
  if (!inherits(ratio, "ttables_ratio")) rlang::abort("'ratio' must be a ratio (`ttables_ratio`) vector.")
  new_rcrd(list(length = length, ratio = ratio), class = "ttables_relative")
}
relative <- function(length = ttables_length(), ratio = new_ratio()) {
  if (length(length) == 0 && length(ratio)) length <- ttables_length(abs_length(0, "pt"), em_length(0))
  if (length(ratio) == 0 && length(length)) ratio <- ratio(0)
  inputs <- vec_recycle_common(length, ratio)
  length <- inputs[[1]]
  ratio <- inputs[[2]]
  length[is.na(length) & !is.na(ratio)] <-ttables_length(abs_length(0, "pt"), em_length(0))
  ratio[is.na(ratio) & !is.na(ratio)] <- ratio(0)
  new_relative(length, ratio)
}
#' @export
format.ttables_relative <- function(x, ...) {
  vapply(x, format_ttables_relative, character(1))
}
format_ttables_relative <- function(x) {
  if (is.na(x)) return(NA_character_)
  len <- field(x, "length")
  ratio <- field(x, "ratio")
  if (is.na(len)) return(format(ratio))
  if (is.na(ratio)) return(format(len))
  len.f <- format(len)
  ratio.n <- unclass(ratio)
  if (len.f == "0" & ratio.n == 0) return("0")
  if (len.f == "0") return(format(ratio))
  if (ratio.n == 0) return(len.f)
  if (substr(len.f, 1, 1) != "-" & ratio.n > 0) return(paste0(format(ratio), " + ", len.f))
  if (substr(len.f, 1, 1) != "-") return(paste0(len.f, " - ", format(-ratio)))
  len.abs.n <- field(field(len, "abs"), "value")
  len.em.n <- unclass(field(len, "em"))
  len.2 <- (!is.na(len.abs.n) & len.abs.n != 0) & (!is.na(len.em.n) & len.em.n != 0)
  if (ratio.n > 0 & len.2) return(paste0(format(ratio), " - (", format(-len), ")"))
  if (ratio.n > 0) return(paste0(format(ratio), " - ", format(-len)))
  return(paste0("-(", format(-len), " + ", format(-ratio), ")"))
}
is_relative <- function(x) inherits(x, "ttables_relative") || is_length(x) || is_ratio(x)
#' @export
is.na.ttables_relative <- function(x) vec_detect_missing(x)
NA_relative_ <- relative(NA_length_, NA_ratio_)
as_relative <- function(x) vec_cast(x, new_relative())
#' @export
vec_ptype_abbr.ttables_relative <- function(x, ...) "rel"
#' @export
vec_ptype_full.ttables_relative <- function(x, ...) "relative"
#' @export
vec_ptype2.ttables_relative.ttables_relative <- function(x, y, ...) new_relative()
#' @export
vec_ptype2.ttables_relative.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ttables_relative <- function(x, y, ...) character()
#' @export
vec_cast.character.ttables_relative <- function(x, to, ...) format(x)
#' @export
vec_cast.ttables_relative.character <- function(x, to, ...) {
  length <- as_length(x)
  ratio <- extract_ratio(x)
  relative(length, ratio)
}
extract_ratio <- function(x) {
  pattern <- paste0("-?[[:blank:]]*\\d*(\\.\\d+)?%")
  out <- rep(NA_character_, length(x))
  idx <- grep(pattern, x, ignore.case = TRUE)
  m <- gregexpr(pattern, x, ignore.case = TRUE)
  lengths <- vapply(m, \(x) length(attr(x, "match.length")), integer(1))
  if (!all(lengths == 1)) rlang::abort("Invalid relative length")
  values <- unlist(regmatches(x, m))
  out[idx] <- values
  as_ratio(out)
}
#'@export
vec_cast.ttables_relative.ttables_length <- function(x, to, ...) relative(length = x)
#' @export
vec_cast.ttables_relative.ttables_ratio <- function(x, to, ...) relative(ratio = x)
#' @export
vec_cast.ttables_relative.ttables_relative <- function(x, to, ...) {
  len <- vec_cast(field(x, "length"), field(to, "length"))
  new_relative(len, field(x, "ratio"))
}
#' @export
vec_proxy_compare.ttables_relative <- function(x, ...) {
  rlang::abort("Comparison of `ttables_relative` objects is not implemented.")
}
#' @export
#' @method vec_arith ttables_relative
vec_arith.ttables_relative <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_relative", y)
}
#' @export
#' @method vec_arith.ttables_relative default
vec_arith.ttables_relative.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.ttables_relative ttables_relative
vec_arith.ttables_relative.ttables_relative <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_relative(vec_arith(op, field(x, "length"), field(y, "length")),
                       vec_arith(op, field(x, "ratio"), field(y, "ratio"))),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_relative numeric
vec_arith.ttables_relative.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = new_relative(vec_arith(op, field(x, "length"), y),
                       vec_arith(op, field(x, "ratio"), y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.numeric ttables_relative
vec_arith.numeric.ttables_relative <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_relative(vec_arith(op, x, field(y, "length")),
                       vec_arith(op, x, field(y, "ratio"))),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_relative MISSING
vec_arith.ttables_relative.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}
