format_table <- function(x, bold = NULL, italic = NULL, align = NULL, size = NULL) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")

  if (!is.null(bold) && !(is.logical(bold) && length(bold) == 1)) stop("'bold' must be a logical scalar")
  if (!is.null(italic) && !(is.logical(italic) && length(italic) == 1)) stop("'italic' must be a logical scalar")
  if (!is.null(align) && !(is.character(align) && length(align) == 1 && align %in% c("left", "centre", "center", "right", "auto")))
    stop("'align' must be one of c('left', 'centre', 'center', 'right', 'auto'")
  if (!is.null(size)) {
    if(!(is.numeric(size) && length(size) == 1 && is_wholenumber(size))) stop("'size' must be an integer scalar")
    size <- as.integer(size)
  }

  format <- tibble::tibble(column = NA_integer_, row = NA_integer_, location = "table",
                           bold = bold %||% NA, italic = italic %||% NA, align = align %||% NA_character_, size = size %||% NA_integer_)
  x$`_format` <- merge_formats(x$`_format`, format)
  x
}
