new_dictionary <- function(x = list()) {
  if (!rlang::is_bare_list(x)) {
    rlang::abort("'x' must be a bare list.")
  }
  if (!rlang::is_named(x)) {
    rlang::abort("All elements of 'x' must be named.")
  }
  structure(list(x), class = "ttables_dictionary")
}

#' Dictionary specification
#'
#' Create a new dictionary specification
#'
#' Corresponds and translates to a Typst `dictionary`.
#'
#' @param x An object to test or convert.
#' @param ... Named objects to populate the dictionary
#'
#' @export
dictionary <- function(...) {
  x <- list(...)
  new_dictionary(x)
}

#' @rdname dictionary
#' @export
is_dictionary <- function(x) inherits(x, "ttables_dictionary")

#' @rdname dictionary
#' @export
as_dictionary <- function(x) {
  x <- unclass(x)
  new_dictionary(x)
}

#' @export
format.ttables_dictionary <- function(x, ...) {
  if (length(x) == 0) {
    return("(:)")
  }
  items <- paste0(names(x[[1]]), ": ", lapply(x[[1]], make_string))
  paste0("(", paste0(items, collapse = ", "), ")")
}

#' @export
print.ttables_dictionary <- function(x, ...) {
  cat(format(x), "\n")
}

make_string <- function(x) {
  if (rlang::is_bare_character(x[[1]])) {
    paste0('"', x, '"')
  } else {
    format(x[[1]])
  }
}

#' Convert dictionary to list
#' @param x Dictionary object to Convert
#' @export
as.list.ttables_dictionary <- function(x) {
  if (!inherits(x, "ttables_dictionary")) {
    rlang::abort("'x' must be a dictionary")
  }
  x[[1]]
}
