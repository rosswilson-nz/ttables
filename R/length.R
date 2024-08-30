#' @include abs_length.R em_length.R
NULL

new_length <- function(abs = abs_length(), em = em_length()) {
  if (!is_abs_length(abs)) rlang::abort("'abs' must be an absolute length (`ttables_abs_length`) vector.")
  if (!is_em_length(em)) rlang::abort("'em' must be an em length (`ttables_em_length`) vector.")
  new_rcrd(list(abs = abs, em = em), class = "ttables_length")
}
ttables_length <- function(abs = abs_length(), em = em_length()) {
  if (length(abs) == 0 && length(em)) abs <- abs_length(0, "pt")
  if (length(em) == 0 && length(abs)) em <- em_length(0)
  inputs <- vec_recycle_common(abs, em)
  abs <- inputs[[1]]
  em <- inputs[[2]]
  if (any(is.na(abs) & !is.na(em))) abs[is.na(abs) & !is.na(em)] <- abs_length(0, "pt")
  if (any(is.na(em) & !is.na(abs))) em[is.na(em) & !is.na(abs)] <- em_length(0)
  new_length(abs, em)
}
#' @export
format.ttables_length <- function(x, ...) {
  vapply(x, format_ttables_length, character(1))
}
format_ttables_length <- function(x) {
  if (is.na(x)) return(NA_character_)
  abs <- field(x, "abs")
  em <- field(x, "em")
  if (is.na(abs)) return(format(em))
  if (is.na(em)) return(format(abs))
  abs.n <- field(abs, "value")
  em.n <- unclass(em)
  if (abs.n == 0 & em.n == 0) return("0")
  if (abs.n == 0) return(format(em))
  if (em.n == 0) return(format(abs))
  if (abs.n > 0 & em.n > 0) return(paste0(format(abs), " + ", format(em)))
  if (abs.n > 0) return(paste0(format(abs), " - ", format(-em)))
  if (em.n > 0) return(paste0(format(em), " - ", format(-abs)))
  return(paste0("-(", format(-abs), " + ", format(-em), ")"))
}
is_length <- function(x) inherits(x, c("ttables_length", "ttables_abs_length", "ttables_em_length"))
#' @export
is.na.ttables_length <- function(x) vec_detect_missing(x)
NA_length_ <- ttables_length(NA_abs_length_, NA_em_length_)
#' @export
as_length <- function(x) vec_cast(x, new_length())
#' @export
vec_ptype_abbr.ttables_length <- function(x, ...) "len"
#' @export
vec_ptype_full.ttables_length <- function(x, ...) "length"
#' @export
vec_ptype2.ttables_length.ttables_length <- function(x, y, ...) new_length()
#' @export
vec_ptype2.ttables_length.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ttables_length <- function(x, y, ...) character()
#' @export
vec_cast.character.ttables_length <- function(x, to, ...) format(x)
#' @export
vec_cast.ttables_length.character <- function(x, to, ...) {
  abs <- extract_abs_length(x)
  em <- extract_em_length(x)
  ttables_length(abs, em)
}
extract_abs_length <- function(x) {
  pattern <- paste0("-?[[:blank:]]*\\d*(\\.\\d+)?(pt|mm|cm|in)")
  out <- rep(NA_character_, length(x))
  idx <- grep(pattern, x, ignore.case = TRUE)
  m <- gregexpr(pattern, x, ignore.case = TRUE)
  lengths <- vapply(m, \(x) length(attr(x, "match.length")), integer(1))
  if (!all(lengths == 1)) rlang::abort("Invalid absolute length")
  values <- unlist(regmatches(x, m))
  out[idx] <- values
  as_abs_length(out)
}
extract_em_length <- function(x) {
  pattern <- paste0("-?[[:blank:]]*\\d*(\\.\\d+)?em")
  out <- rep(NA_character_, length(x))
  idx <- grep(pattern, x, ignore.case = TRUE)
  m <- gregexpr(pattern, x, ignore.case = TRUE)
  lengths <- vapply(m, \(x) length(attr(x, "match.length")), integer(1))
  if (!all(lengths == 1)) rlang::abort("Invalid em length")
  values <- unlist(regmatches(x, m))
  out[idx] <- values
  as_em_length(out)
}
#' @export
vec_cast.ttables_length.ttables_abs_length <- function(x, to, ...) ttables_length(abs = x)
#' @export
vec_cast.ttables_length.ttables_em_length <- function(x, to, ...) ttables_length(em = x)
#' @export
vec_proxy_compare.ttables_length <- function(x, ...) {
  rlang::abort("Comparison of `ttables_length` objects is not implemented.")
}
#' @export
#' @method vec_arith ttables_length
vec_arith.ttables_length <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_length", y)
}
#' @export
#' @method vec_arith.ttables_length default
vec_arith.ttables_length.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.ttables_length ttables_length
vec_arith.ttables_length.ttables_length <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_length(vec_arith(op, field(x, "abs"), field(y, "abs")),
                     vec_arith(op, field(x, "em"), field(y, "em"))),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_length numeric
vec_arith.ttables_length.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = new_length(vec_arith(op, field(x, "abs"), y),
                     vec_arith(op, field(x, "em"), y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.numeric ttables_length
vec_arith.numeric.ttables_length <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_length(vec_arith(op, x, field(y, "abs")),
                     vec_arith(op, x, field(y, "em"))),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_length MISSING
vec_arith.ttables_length.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}
