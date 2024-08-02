pack_rows <- function(x, start, nrows, label = NULL, italic = TRUE, bold = FALSE, align = NULL, indent = TRUE) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!(is.numeric(start) && length(start) == 1 && is_wholenumber(start))) stop("'start' must be an integer scalar")
  start <- resolve_location(cells(columns = 1, rows = start), x)
  if (!(is.numeric(nrows) && length(nrows) == 1 & is_wholenumber(nrows))) stop("'nrows' must be an integer scalar")
  if (!is.null(label) && !(is.character(label) && length(label) == 1)) stop("'label' must be a character scalar")
  if (!is.null(italic) && !(is.logical(italic) && length(italic) == 1)) stop("'italic' must be a logical scalar")
  if (!is.null(bold) && !(is.logical(bold) && length(bold) == 1)) stop("'bold' must be a logical scalar")
  if (!is.null(align) && !(is.character(align) && length(align) == 1 && align %in% c("left", "centre", "center", "right", "auto")))
    stop("'align' must be one of c('left', 'centre', 'center', 'right', 'auto'")
  if (!is.null(indent)) {
    if (isTRUE(indent)) indent <- "1em"
    if (!((is.numeric(indent) || is.character(indent)) && length(indent) == 1)) stop("'indent' must be a numeric or character scalar")
    if (is.numeric(indent)) indent <- paste0(indent, "em")
    n <- nchar(indent)
    if (!(substring(indent, n - 1, n) %in% c("pt", "mm", "cm", "in", "em"))) stop("'indent' must be a valid Typst length")
  }

  firstvar <- colnames(x$`_body`)[[1]]
  new_row <- tibble::tibble({{firstvar}} := label, `_insert_before` = start$rows)
  x$`_added_rows` <- tibble::add_row(x$`_added_rows`, new_row)
  x %>%
    format_cells(cells(1, !!nrow(x$`_added_rows`), "added_rows"), bold = bold, italic = italic, align = align) %>%
    add_indent(cells(1, start$rows + seq_len(nrows) - 1))
}
