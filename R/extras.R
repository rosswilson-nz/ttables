#' Put a Typst table on its own landscape page
#'
#' @param x A Typst table
#'
#' @returns A Typst table with the landscape page specification.
#' @export
landscape <- function(x) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")

  set_table_options(x, landscape = TRUE)
}

#' Mark a Typst table as supplementary material
#'
#' @param x A Typst table
#'
#' @returns A Typst table with the supplementary material specification.
#' @export
supplement <- function(x) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")

  set_table_options(x, supplement = TRUE)
}
