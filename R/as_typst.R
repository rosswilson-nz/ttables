#' Print a Typst table or figure object as raw Typst
#'
#' @param x A Typst table (class `ttables_tbl`)
#' @returns A `glue` string containing the raw Typst output.
#'
#' @export
as_typst <- function(x) {
  UseMethod("as_typst", x)
}
