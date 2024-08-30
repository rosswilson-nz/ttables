#' Put a Typst table on its own landscape page
#'
#' @param x A Typst table
#'
#' @returns A Typst table with the landscape page specification.
#' @export
landscape <- function(x) {
  if (inherits(x, "ttables_tbl")) return(set_table_options(x, landscape = TRUE))

  stop("'x' must be a `ttables_tbl` object")
}

#' Mark a Typst table as supplementary material
#'
#' @param x A Typst table
#'
#' @returns A Typst table with the supplementary material specification.
#' @export
supplement <- function(x) {
  if (inherits(x, "ttables_tbl")) return(set_table_options(x, supplement = TRUE))
  stop("'x' must be a `ttables_tbl` object")
}
