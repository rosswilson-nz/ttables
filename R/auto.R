auto <- function() structure("auto", class = "auto")
#' @export
format.auto <- function(x, ...) "auto"
#' @export
print.auto <- function(x, ...) cat("auto")
is_auto <- function(x) inherits(x, "auto")
none <- function() structure("none", class = "none")
#' @export
format.none <- function(x, ...) "none"
#' @export
print.none <- function(x, ...) cat("none")
is_none <- function(x) inherits(x, "none")
