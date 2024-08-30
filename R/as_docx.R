#' Export a Typst table or figure object as raw Word XML
#'
#' @param x A Typst table (class `ttables_tbl`)
#' @returns A `glue` string containing the raw XML output.
#'
#' @export
as_docx <- function(x) {
  UseMethod("as_docx", x)
}
