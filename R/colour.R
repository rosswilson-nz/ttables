new_colour <- function(x = character(), call = rlang::caller_env()) {
  if (!rlang::is_character(x)) cli::cli_abort(c("'x' must be a character vector.",
                                                "*" = "You've supplied {.obj_type_friendly {x}}."),
                                              call = call)
  if (!all(x %in% c(typst_colours(), NA_character_))) cli::cli_abort(c("All 'x' values must be valid colours.",
                                                                       i = "See {.fn typst_colours} for the valid options."),
                                                                     call = call)
  new_vctr(x, class = "ttables_colour")
}

#' List the available Typst colours by name
#' @export
typst_colours <- function() {
  c("black", "gray", "silver", "white", "navy", "blue", "aqua", "teal", "eastern", "purple",
    "fuchsia", "maroon", "red", "orange", "yellow", "olive", "green", "lime")
}
colour <- function(x = character()) {
  new_colour(x)
}
is_colour <- function(x) inherits(x, "ttables_colour")
NA_colour_ <- new_colour(NA_character_)
#' @export
is.na.ttables_colour <- function(x) vec_detect_missing(x)
as_colour <- function(x) vec_cast(x, colour())
#' @export
format.ttables_colour <- function(x, ..., justify = "none") {
  out <- format(vec_data(x), justify = justify)
  out[is.na(x)] <- NA
  out
}
#' @export
vec_ptype_abbr.ttables_colour <- function(x, ...) "colr"
#' @export
vec_ptype_full.ttables_colour <- function(x, ...) "colour"
#' @export
vec_ptype2.ttables_colour.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ttables_colour <- function(x, y, ...) character()
#' @export
vec_cast.ttables_colour.character <- function(x, to, ...) colour(x)
#' @export
vec_cast.character.ttables_colour <- function(x, to, ...) vec_data(x)
#' @export
#' @method vec_arith ttables_colour
vec_arith.ttables_colour <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_colour", y)
}
#' @export
#' @method vec_arith.ttables_colour default
vec_arith.ttables_colour.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

# #' @export
# #' @method vec_arith.ttables_horiz_alignment ttables_vert_alignment
# vec_arith.ttables_horiz_alignment.ttables_vert_alignment <- function(op, x, y, ...) {
#   switch(op,
#          "+" = new_alignment(x, y),
#          stop_incompatible_op(op, x, y))
# }
#
