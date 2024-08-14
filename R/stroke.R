new_stroke <- function(paint = new_colour(), thickness = new_length()) {
  if (length(paint) > 1 | !(is_colour(paint) | is_auto(paint)))
    rlang::abort("'paint' must be a single Typst colour (`ttables_colour`) specification.")
  if (length(thickness) > 1 | !(is_length(thickness) | is_auto(thickness)))
    rlang::abort("'thickness' must be a single Typst length (`ttables_length`) specification.")
  new_rcrd(list(paint = paint, thickness = thickness), class = "ttables_stroke")
}
stroke <- function(paint = colour(), thickness = ttables_length()) {
  paint <- vec_cast(paint, colour())
  thickness <- vec_cast(thickness, ttables_length())
  if (length(paint) == 0 && length(thickness)) paint <- rep(NA_colour_, length(thickness))
  if (length(thickness) == 0 && length(paint)) thickness <- rep(NA_length_, length(paint))
  inputs <- vec_recycle_common(paint, thickness)
  new_stroke(paint = inputs[[1]], thickness = inputs[[2]])
}
#' @export
format.ttables_stroke <- function(x, ...) {
  paint <- format(field(x, "paint"))
  thickness <- format(field(x, "thickness"))

  out <- paste(paint, thickness, sep = " + ")
  out[is.na(paint)] <- thickness[is.na(paint)]
  out[is.na(thickness)] <- paint[is.na(thickness)]
  out[is.na(paint) & is.na(thickness)] <- NA

  out
}
is_stroke <- function(x) inherits(x, "ttables_stroke")
#' @export
is.na.ttables_stroke <- function(x) vec_detect_missing(x)
NA_stroke_ <- stroke(NA_colour_, NA_length_)
as_stroke <- function(x) vec_cast(x, stroke())
#' @export
vec_ptype_abbr.ttables_stroke <- function(x, ...) "strk"
#' @export
vec_ptype_full.ttables_stroke <- function(x, ...) "stroke"
#' @export
vec_ptype2.ttables_stroke.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ttables_stroke <- function(x, y, ...) character()
#' @export
vec_cast.ttables_stroke.character <- function(x, to, ...) {
  paint <- get_paint_part(x)
  length <- as_length(x)
  stroke(paint, length)
}
#' @export
vec_cast.character.ttables_stroke <- function(x, to, ...) format(x)
get_paint_part <- function(x) {
  pattern <- glue::glue_collapse(typst_colours(), "|")
  out <- rep(NA_character_, length(x))
  idx <- grep(pattern, x, ignore.case = TRUE)
  m <- gregexpr(pattern, x, ignore.case = TRUE)
  lengths <- vapply(m, \(x) length(attr(x, "match.length")), integer(1))
  if (!all(lengths == 1)) rlang::abort("Invalid paint specification")
  values <- unlist(regmatches(x, m))
  out[idx] <- values
  out
}
