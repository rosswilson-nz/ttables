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
vec_cast.ttables_abs_length.character <- function(x, to, ...) as_abs_length(x)
#' @export
vec_cast.character.ttables_abs_length <- function(x, to, ...) format(x)
#' @export
vec_cast.double.ttables_abs_length <- function(x, to, ...) field(x, "value")
#' @export
vec_cast.ttables_abs_length.ttables_abs_length <- function(x, to, ...) x
as_abs_length <- function(x) {
  n <- nchar(x)
  unit <- substr(x, n - 1, n)
  if (!all(unit %in% c("pt", "mm", "cm", "in") | is.na(x))) rlang::abort("Invalid length")
  value <- gsub("(pt|mm|cm|in)$", "", x)
  #value <- rlang::try_fetch(as.numeric(value), warning = \(w) rlang::abort("Invalid length", parent = w))
  value <- as.numeric(value)
  abs_length(value, unit)
}
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
    "*" = new_abs_length(vec_arith_base(op, x, y), field(x, "unit")),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.numeric ttables_abs_length
vec_arith.numeric.ttables_abs_length <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_abs_length(vec_arith_base(op, x, y), field(y, "unit")),
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

new_rel_length <- function(x = double()) {
  if (!rlang::is_double(x)) rlang::abort("'x' must be a double vector.")
  new_vctr(x, class = "ttables_rel_length")
}
rel_length <- function(x = double()) {
  x <- vec_cast(x, double())
  new_rel_length(x)
}
is_rel_length <- function(x) inherits(x, "ttables_rel_length")
NA_rel_length_ <- new_rel_length(NA_real_)
#' @export
format.ttables_rel_length <- function(x, ...) {
  out <- formatC(vec_data(x), ...)
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "em")
  out
}
#' @export
vec_ptype_abbr.ttables_rel_length <- function(x, ...) "rellen"
#' @export
vec_ptype_full.ttables_rel_length <- function(x, ...) "rel_length"
#' @export
vec_cast.ttables_rel_length.double <- function(x, to, ...) rel_length(x)
#' @export
vec_cast.double.ttables_rel_length <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.ttables_rel_length.character <- function(x, to, ...) as_rel_length(x)
as_rel_length <- function(x) {
  n <- nchar(x)
  unit <- substr(x, n - 1, n)
  if (!all(unit == "em" | is.na(x))) rlang::abort("Invalid relative length")
  value <- gsub("em$", "", x)
  #value <- rlang::try_fetch(as.numeric(value), warning = \(w) rlang::abort("Invalid relative length", parent = w))
  value <- as.numeric(value)
  new_rel_length(value)
}
#' @export
#' @method vec_arith ttables_rel_length
vec_arith.ttables_rel_length <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_rel_length", y)
}
#' @export
#' @method vec_arith.ttables_rel_length default
vec_arith.ttables_rel_length.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.ttables_rel_length ttables_rel_length
vec_arith.ttables_rel_length.ttables_rel_length <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_rel_length(vec_arith_base(op, x, y)),
    "/" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_rel_length numeric
vec_arith.ttables_rel_length.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "/" = ,
    "*" = new_rel_length(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.numeric ttables_rel_length
vec_arith.numeric.ttables_rel_length <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_rel_length(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.ttables_rel_length MISSING
vec_arith.ttables_rel_length.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = x * -1,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}


new_length <- function(abs = abs_length(), rel = rel_length()) {
  if (!is_abs_length(abs)) rlang::abort("'abs' must be an absolute length (`ttables_abs_length`) vector.")
  if (!is_rel_length(rel)) rlang::abort("'rel' must be a relative length (`ttables_rel_length`) vector.")
  new_rcrd(list(abs = abs, rel = rel), class = "ttables_length")
}
ttables_length <- function(abs = abs_length(), rel = rel_length()) {
  if (length(abs) == 0 && length(rel)) abs <- abs_length(0, "pt")
  if (length(rel) == 0 && length(abs)) rel <- rel_length(0)
  inputs <- vec_recycle_common(abs, rel)
  abs <- inputs[[1]]
  rel <- inputs[[2]]
  abs[is.na(abs) & !is.na(rel)] <- abs_length(0, "pt")
  rel[is.na(rel) & !is.na(abs)] <- rel_length(0)
  new_length(abs, rel)
}
#' @export
format.ttables_length <- function(x, ...) {
  abs <- field(x, "abs")
  rel <- field(x, "rel")
  out <- paste0(format(abs), " + ", format(rel))
  out[is.na(abs) | vec_cast(abs, numeric()) == 0] <- format(rel[is.na(abs) | vec_cast(abs, numeric()) == 0])
  out[is.na(rel) | vec_cast(rel, numeric()) == 0] <- format(abs[is.na(rel) | vec_cast(rel, numeric()) == 0])
  out[(is.na(abs) | vec_cast(abs, numeric()) == 0) & (is.na(rel) | vec_cast(rel, numeric()) == 0)] <- "0"
  out[is.na(x)] <- NA

  out
}
is_length <- function(x) inherits(x, "ttables_length")
#' @export
is.na.ttables_length <- function(x) vec_detect_missing(x)
NA_length_ <- ttables_length(NA_abs_length_, NA_rel_length_)
as_length <- function(x) {
  abs <- extract_abs_length(x)
  rel <- extract_rel_length(x)
  ttables_length(abs, rel)
}
extract_abs_length <- function(x) {
  pattern <- paste0("\\d*(\\.\\d+)?(pt|mm|cm|in)")
  out <- rep(NA_character_, length(x))
  idx <- grep(pattern, x, ignore.case = TRUE)
  m <- gregexpr(pattern, x, ignore.case = TRUE)
  lengths <- vapply(m, \(x) length(attr(x, "match.length")), integer(1))
  if (!all(lengths == 1)) rlang::abort("Invalid absolute length")
  values <- unlist(regmatches(x, m))
  out[idx] <- values
  as_abs_length(out)
}
extract_rel_length <- function(x) {
  pattern <- paste0("\\d*(\\.\\d+)?em")
  out <- rep(NA_character_, length(x))
  idx <- grep(pattern, x, ignore.case = TRUE)
  m <- gregexpr(pattern, x, ignore.case = TRUE)
  lengths <- vapply(m, \(x) length(attr(x, "match.length")), integer(1))
  if (!all(lengths == 1)) rlang::abort("Invalid relative length")
  values <- unlist(regmatches(x, m))
  out[idx] <- values
  as_rel_length(out)
}
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
vec_cast.ttables_length.character <- function(x, to, ...) as_length(x)
#' @export
vec_cast.ttables_length.ttables_length <- function(x, to, ...) {
  abs_x <- field(x, "abs")
  abs_to <- field(to, "abs")
  if (identical(field(abs_x, "unit"), field(abs_to, "unit"))) x else {
    abs <- vec_cast(abs_x, abs_to)
    new_length(abs, field(x, "rel"))
  }
}
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
                     vec_arith(op, field(x, "rel"), field(y, "rel"))),
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
                     vec_arith(op, field(x, "rel"), y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.numeric ttables_length
vec_arith.numeric.ttables_length <- function(op, x, y, ...) {
  switch(
    op,
    "*" = new_length(vec_arith(op, x, field(y, "abs")),
                     vec_arith(op, x, field(y, "rel"))),
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
