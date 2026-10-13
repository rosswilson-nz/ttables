#' Create Typst `auto` object
#' @export
auto <- function() structure("auto", class = "ttables_auto")
#' @export
format.ttables_auto <- function(x, ...) "auto"
#' @export
print.ttables_auto <- function(x, ...) cat("auto")
#' Test for `auto` object
#' @param x Object to test
#' @export
is_auto <- function(x) inherits(x, "ttables_auto")
#' Create Typst `none` object
#' @export
none <- function() structure("none", class = "ttables_none")
#' @export
format.ttables_none <- function(x, ...) "none"
#' @export
print.ttables_none <- function(x, ...) cat("none")
#' Test for `none` object
#' @param x Object to test
#' @export
is_none <- function(x) inherits(x, "ttables_none")
