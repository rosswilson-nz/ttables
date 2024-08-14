new_dictionary <- function(x = list()) {
  if (!rlang::is_bare_list(x)) rlang::abort("'x' must be a bare list.")
  if (!rlang::is_named(x)) rlang::abort("All elements of 'x' must be named.")
  structure(list(x), class = "ttables_dictionary")
}
dictionary <- function(x = list()) {
  x <- unclass(x)
  new_dictionary(x)
}
#' @export
format.ttables_dictionary <- function(x, ...) {
  if (length(x) == 0) return ("(:)")
  items <- paste0(names(x[[1]]), ": ", lapply(x[[1]], make_string))
  paste0("(", paste0(items, collapse = ", "), ")")
}
#' @export
print.ttables_dictionary <- function(x, ...) {
  cat(format(x))
}

make_string <- function(x) {
  if (rlang::is_bare_character(x[[1]])) paste0('"', x, '"')
  else format(x[[1]])
}
