add_header <- function(x, content, position = 1, span = colspan()) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.character(content)) stop("'content' must be a character vector")
  if (is.null(names(content))) {
    if (length(content) != ncol(x$`_header`)) stop("'content' must be a named vector or have one entry per table column")
    content <- setNames(content, colnames(x$`_header`))
  }

  header <- tibble::as_tibble_row(content)
  x$`_header` <- tibble::add_row(x$`_header`, header, .before = position)
  x
}
