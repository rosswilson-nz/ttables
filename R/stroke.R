#' @include colour.R length.R dash.R
NULL

new_stroke <- function(paint = new_colour(), thickness = new_length(), dash = new_dash()) {
  if (length(paint) > 1 || !(is_colour(paint) || is_auto(paint))) {
    rlang::abort(
      "'paint' must be a single Typst colour (`ttables_colour`) specification."
    )
  }
  if (length(thickness) > 1 || !(is_length(thickness) || is_auto(thickness))) {
    rlang::abort(
      "'thickness' must be a single Typst length (`ttables_length`) specification."
    )
  }
  if (length(dash) > 1 || !(is_dash(dash) || is_auto(dash))) {
    rlang::abort(
      "'dash' must be a single Typst dash (`ttables_dash`) specification."
    )
  }
  new_rcrd(list(paint = paint, thickness = thickness, dash = dash), class = "ttables_stroke")
}

#' Stroke specification
#'
#' Create a new stroke specification
#'
#' Corresponds and translates to a Typst `stroke`. Only the paint (solid
#'     colour), thickness, and dash attributes are currently implemented.
#'
#' @param paint A scalar `colour` value
#' @param thickness A scalar `ttables_length` value
#' @param pattern A scalar `dash` value
#' @param x An object to test or convert.
#'
#' @export
stroke <- function(paint = colour(), thickness = ttables_length(), pattern = dash()) {
  paint <- vec_cast(paint, colour())
  thickness <- vec_cast(thickness, ttables_length())
  dash <- vec_cast(pattern, dash())

  if (length(paint) == 0 && (length(thickness) || length(dash))) {
    paint <- NA_colour_
  }
  if (length(thickness) == 0 && (length(paint) || length(dash))) {
    thickness <- NA_length_
  }
  if (length(dash) == 0 && (length(paint) || length(thickness))) {
    dash <- NA_dash_
  }
  new_stroke(paint = paint, thickness = thickness, dash = dash)
}

#' @export
format.ttables_stroke <- function(x, ...) {
  paint <- format(field(x, "paint"))
  thickness <- format(field(x, "thickness"))
  dash <- format(field(x, "dash"))
  if (is.na(paint) && is.na(thickness) && is.na(dash)) {
    return(NA_character_)
  }

  out <- paste0(
    if (!is.na(paint)) paint,
    if (!is.na(paint) && !is.na(thickness)) " + ",
    if (!is.na(thickness)) thickness,
    if ((!is.na(paint) || !is.na(thickness)) && !is.na(dash)) " + ",
    if (!is.na(dash)) dash
  )

  out
}
#' @export
print.ttables_stroke <- function(x, ...) cat(paste("stroke:", format(x, ...)))

#' @rdname stroke
#' @export
is_stroke <- function(x) inherits(x, "ttables_stroke")

#' @export
is.na.ttables_stroke <- function(x) vec_detect_missing(x)

#' @rdname stroke
#' @export
NA_stroke_ <- stroke(NA_colour_, NA_length_, NA_dash_)

#' @rdname stroke
#' @export
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
  length <- tryCatch(as_length(x), error = function(e) NA)
  dash <- get_pattern_part(x)
  stroke(paint, length, dash)
}
#' @export
vec_cast.character.ttables_stroke <- function(x, to, ...) format(x)

get_paint_part <- function(x) {
  pattern <- paste(typst_colours(), collapse = "|")
  out <- rep(NA_character_, length(x))
  idx <- grep(pattern, x, ignore.case = TRUE)
  m <- gregexpr(pattern, x, ignore.case = TRUE)
  lengths <- vapply(m, \(x) length(attr(x, "match.length")), integer(1))
  if (!all(lengths == 1)) {
    rlang::abort("Invalid paint specification")
  }
  values <- unlist(regmatches(x, m))
  out[idx] <- values
  out
}

get_pattern_part <- function(x) {
  pattern <- paste(typst_dashes(), collapse = "|")
  out <- rep(NA_character_, length(x))
  idx <- grep(pattern, x, ignore.case = TRUE)
  m <- gregexpr(pattern, x, ignore.case = TRUE)
  lengths <- vapply(m, \(x) length(attr(x, "match.length")), integer(1))
  if (!all(lengths == 1)) {
    rlang::abort("Invalid dash specification")
  }
  values <- unlist(regmatches(x, m))
  out[idx] <- values
  out
}
