new_width <- function(x = list()) {
  if (!rlang::is_list(x)) rlang::abort("'x' must be a list.")
  if (!all(vapply(x, is_track_size, logical(1)))) rlang::abort("'x' must be a list of Typst track sizes")
  new_vctr(x, class = "ttables_width")
}
width <- function(x = list()) {
  x <- rlang::as_list(x)
  new_width(x)
}
is_width <- function(x) inherits(x, "width")
is_track_size <- function(x) {
  length(x) == 1 && (is_length(x) || is_auto(x) || is_fractional_length(x))
}
#' @export
format.ttables_width <- function(x, ...) {
  vapply(x, format, character(1))
}
#' @export
vec_ptype_abbr.ttables_width <- function(x, ...) "wdth"
#' @export
vec_ptype_full.ttables_width <- function(x, ...) "width"
#' @export
vec_ptype2.ttables_width.ttables_width <- function(x, y, ...) new_width()
#' @export
vec_ptype2.ttables_width.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ttables_width <- function(x, y, ...) character()
#' @export
vec_cast.ttables_width.character <- function(x, to, ...) as_width(x)
#' @export
vec_cast.ttables_width.double <- function(x, to, ...) as_width(x)
#' @export
vec_cast.character.ttables_width <- function(x, to, ...) format(x)
#' @export
vec_cast.ttables_width.ttables_width <- function(x, to, ...) x
as_width <- function(x) {
  value <- lapply(x, extract_track_size)
  new_width(value)
}
extract_track_size <- function(x) {
  if (is.numeric(x)) return(new_fractional_length(x))
  if (x == "auto") return(auto())
  n <- nchar(x)
  if (substr(x, n - 1, n) == "fr") return(as_fractional_length(x))
  as_length(x)
}
