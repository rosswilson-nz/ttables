new_horiz_alignment <- function(x = character()) {
  if (!rlang::is_character(x)) rlang::abort("'x' must be a character vector")
  if (!all(x %in% c("left", "center", "right", NA_character_))) rlang::abort("All 'x' values must be in c('left', 'center', 'right')")
  new_vctr(x, class = "ttables_horiz_alignment")
}
horiz_alignment <- function(x = character()) {
  x <- gsub("centre", "center", x)
  new_horiz_alignment(x)
}
is_horiz_alignment <- function(x) inherits(x, "ttables_horiz_alignment")
NA_horiz_alignment_ <- new_horiz_alignment(NA_character_)
#' @export
format.ttables_horiz_alignment <- function(x, ..., justify = "none") {
  out <- format(vec_data(x), justify = justify)
  out[is.na(x)] <- NA
  out
}
#' @export
vec_ptype_abbr.ttables_horiz_alignment <- function(x, ...) "hlgn"
#' @export
vec_ptype_full.ttables_horiz_alignment <- function(x, ...) "horiz_alignment"
#' @export
vec_ptype2.ttables_horiz_alignment.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ttables_horiz_alignment <- function(x, y, ...) character()
#' @export
vec_cast.ttables_horiz_alignment.character <- function(x, to, ...) horiz_alignment(x)
#' @export
vec_cast.character.ttables_horiz_alignment <- function(x, to, ...) vec_data(x)
#' @export
#' @method vec_arith ttables_horiz_alignment
vec_arith.ttables_horiz_alignment <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_horiz_alignment", y)
}
#' @export
#' @method vec_arith.ttables_horiz_alignment default
vec_arith.ttables_horiz_alignment.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.ttables_horiz_alignment ttables_vert_alignment
vec_arith.ttables_horiz_alignment.ttables_vert_alignment <- function(op, x, y, ...) {
  switch(op,
         "+" = new_alignment(x, y),
         stop_incompatible_op(op, x, y))
}

new_vert_alignment <- function(x = character()) {
  if (!rlang::is_character(x)) rlang::abort("'x' must be a character vector")
  if (!all(x %in% c("top", "horizon", "bottom", NA_character_))) rlang::abort("All 'x' values must be in c('top', 'horizon', 'bottom')")
  new_vctr(x, class = "ttables_vert_alignment")
}
vert_alignment <- function(x = character()) {
  new_vert_alignment(x)
}
is_vert_alignment <- function(x) inherits(x, "ttables_vert_alignment")
NA_vert_alignment_ <- new_vert_alignment(NA_character_)
#' @export
format.ttables_vert_alignment <- function(x, ..., justify = "none") {
  out <- format(vec_data(x), justify = justify, ...)
  out[is.na(x)] <- NA
  out
}
#' @export
vec_ptype_abbr.ttables_vert_alignment <- function(x, ...) "vlgn"
#' @export
vec_ptype_full.ttables_vert_alignment <- function(x, ...) "vert_alignment"
#' @export
vec_ptype2.ttables_vert_alignment.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ttables_vert_alignment <- function(x, y, ...) character()
#' @export
vec_cast.ttables_vert_alignment.character <- function(x, to, ...) vert_alignment(x)
#' @export
vec_cast.character.ttables_vert_alignment <- function(x, to, ...) vec_data(x)
#' @export
#' @method vec_arith ttables_vert_alignment
vec_arith.ttables_vert_alignment <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_vert_alignment", y)
}
#' @export
#' @method vec_arith.ttables_vert_alignment default
vec_arith.ttables_vert_alignment.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.ttables_vert_alignment ttables_horiz_alignment
vec_arith.ttables_vert_alignment.ttables_horiz_alignment <- function(op, x, y, ...) {
  switch(op,
         "+" = new_alignment(y, x),
         stop_incompatible_op(op, x, y))
}

new_alignment <- function(x = horiz_alignment(), y = vert_alignment()) {
  if (!(is_horiz_alignment(x))) rlang::abort("'x' must be a horizontal alignment (`ttables_horiz_alignment`) vector.")
  if (!(is_vert_alignment(y))) rlang::abort("'x' must be a vertical alignment (`ttables_vert_alignment`) vector.")
  new_rcrd(list(x = x, y = y), class = "ttables_alignment")
}
alignment <- function(horiz = horiz_alignment(), vert = vert_alignment()) {
  horiz <- vec_cast(horiz, horiz_alignment())
  vert <- vec_cast(vert, vert_alignment())
  if (length(horiz) == 0 && length(vert)) horiz <- rep(NA_horiz_alignment_, length(vert))
  if (length(vert) == 0 && length(horiz)) vert <- rep(NA_vert_alignment_, length(horiz))
  inputs <- vec_recycle_common(horiz, vert)
  new_alignment(x = inputs[[1]], y = inputs[[2]])
}
#' @export
format.ttables_alignment <- function(x, ...) {
  horiz <- format(field(x, "x"))
  vert <- format(field(x, "y"))

  out <- paste(horiz, vert, sep = " + ")
  out[is.na(horiz)] <- vert[is.na(horiz)]
  out[is.na(vert)] <- horiz[is.na(vert)]
  out[is.na(horiz) & is.na(vert)] <- NA

  out
}
is_alignment <- function(x) inherits(x, "ttables_alignment")
#' @export
is.na.ttables_alignment <- function(x) vec_detect_missing(x)
NA_alignment_ <- alignment(NA_horiz_alignment_, NA_vert_alignment_)
as_alignment <- function(x) vec_cast(x, alignment())
#' @export
vec_ptype_abbr.ttables_alignment <- function(x, ...) "algn"
#' @export
vec_ptype_full.ttables_alignment <- function(x, ...) "alignment"
#' @export
vec_ptype2.ttables_alignment.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ttables_alignment <- function(x, y, ...) character()
#' @export
vec_cast.ttables_alignment.character <- function(x, to, ...) {
  horiz <- get_horiz_part(x)
  vert <- get_vert_part(x)
  alignment(horiz, vert)
}
#' @export
vec_cast.character.ttables_alignment <- function(x, to, ...) format(x)
get_horiz_part <- function(x) extract_text(x, "left|centre|center|right")
get_vert_part <- function(x) extract_text(x, "top|horizon|bottom")
extract_text <- function(x, pattern) {
  out <- rep(NA_character_, length(x))
  idx <- grep(pattern, x, ignore.case = TRUE)
  m <- gregexpr(pattern, x, ignore.case = TRUE)
  lengths <- vapply(m, \(x) length(attr(x, "match.length")), integer(1))
  if (!all(lengths == 1)) rlang::abort("Invalid alignment")
  values <- unlist(regmatches(x, m))
  out[idx] <- values
  out
}
