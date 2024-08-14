new_dash <- function(x = character(), call = rlang::caller_env()) {
  if (!rlang::is_character(x)) cli::cli_abort(c("'x' must be a character vector.",
                                                "*" = "You've supplied {.obj_type_friendly {x}}."),
                                              call = call)
  if (!all(x %in% c(typst_dashes(), "none", NA_character_))) cli::cli_abort(
    c("All 'x' values must be valid dash specifications",
      i = "See {.fn typst_dashes} for the valid options."),
    call = call
  )
  new_vctr(x, class = "ttables_dash")
}

#' List the available Typst dash specifications by name
#' @export
typst_dashes <- function() {
  c("solid", "dotted", "densely-dotted", "loosely-dotted", "dashed", "densely-dashed",
    "loosely-dashed", "dash-dotted", "densely-dash-dotted", "loosely-dash-dotted")
}
dash <- function(x = character()) {
  new_dash(x)
}
is_dash <- function(x) inherits(x, "ttables_dash")
NA_dash_ <- new_dash(NA_character_)
#' @export
format.ttables_dash <- function(x, ..., justify = "none") {
  out <- format(vec_data(x), justify = justify)
  out[is.na(x)] <- NA
  out
}
#' @export
vec_ptype_abbr.ttables_dash <- function(x, ...) "dash"
#' @export
vec_ptype_full.ttables_dash <- function(x, ...) "dash"
#' @export
vec_ptype2.ttables_dash.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.ttables_dash <- function(x, y, ...) character()
#' @export
vec_cast.ttables_dash.character <- function(x, to, ...) dash(x)
#' @export
vec_cast.character.ttables_dash <- function(x, to, ...) vec_data(x)
#' @export
#' @method vec_arith ttables_dash
vec_arith.ttables_dash <- function(op, x, y, ...) {
  UseMethod("vec_arith.ttables_dash", y)
}
#' @export
#' @method vec_arith.ttables_dash default
vec_arith.ttables_dash.default <- function(op, x, y, ...) {
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
