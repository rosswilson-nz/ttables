landscape <- function(x) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")

  out <- x
  attr(out, "landscape") <- TRUE
  out
}

supplement <- function(x) {
  if (!inherits(x, "typst_table") && !inherits(x, "typst_figure"))
    stop("'x' must be a `typst_table` or `typst_figure` object")

  out <- x
  attr(out, "supplement") <- TRUE
  out
}
