format_cells <- function(x, location, bold = NULL, italic = NULL, align = NULL, indent = NULL) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  location <- resolve_location(location, x)

  if (!is.null(bold) && !(is.logical(bold) && length(bold) == 1)) stop("'bold' must be a logical scalar")
  if (!is.null(italic) && !(is.logical(italic) && length(italic) == 1)) stop("'italic' must be a logical scalar")
  if (!is.null(align) && !(is.character(align) && length(align) == 1 && align %in% c("left", "centre", "center", "right", "auto")))
    stop("'align' must be one of c('left', 'centre', 'center', 'right', 'auto'")
  if (!is.null(indent)) {
    if (!((is.numeric(indent) || is.character(indent)) && length(indent) == 1)) stop("'indent' must be a numeric or character scalar")
    if (is.numeric(indent)) indent <- paste0(indent, "fr")
    n <- nchar(indent)
    if (!(substring(indent, n - 1, n) %in% c("pt", "mm", "cm", "in", "em"))) stop("'indent' must be a valid Typst length")
  }

  format <- tibble::tibble(location, bold, italic, align, indent)
  x$`_format` <- tibble::add_row(x$`_format`, format)
  x
}

add_indent <- function(x, location, amount = "1em") {
  x
}
