#' Create Typst `auto` object
#' @export
auto <- function() structure("auto", class = "auto")
#' @export
format.auto <- function(x, ...) "auto"
#' @export
print.auto <- function(x, ...) cat("auto")
#' Test for `auto` object
#' @param x Object to test
#' @export
is_auto <- function(x) inherits(x, "auto")
#' Create Typst `none` object
#' @export
none <- function() structure("none", class = "none")
#' @export
format.none <- function(x, ...) "none"
#' @export
print.none <- function(x, ...) cat("none")
#' Test for `none` object
#' @param x Object to test
#' @export
is_none <- function(x) inherits(x, "none")
