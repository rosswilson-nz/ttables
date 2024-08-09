#' Group table rows together under a group label
#'
#' @param x A Typst table
#' @param start The first row of the group
#' @param nrows The number of rows to group together
#' @param label The label to add above the grouped rows. `NULL` for no label.
#' @param italic,bold Whether the group label should be formatted with italic
#'     and/or bold styling. Default is italic, regular weight.
#' @param align Cell alignment for the group label. Either `"auto"` or a Typst
#'     [alignment](https://typst.app/docs/reference/layout/alignment/)
#'     specification. `NULL` (the default) to use the table alignment.
#' @param indent Whether to indent the grouped rows. Either `TRUE` for the
#'     default (`1em`) indent, `FALSE` for no indent, or a Typst
#'     [length](https://typst.app/docs/reference/text/text/#parameters-size)
#'     specification. A numeric `indent` will be taken as 'em' units. Default is
#'     `TRUE.`
#'
#' @returns A Typst table with the specified grouped rows.
#' @export
pack_rows <- function(x, start, nrows, label = NULL, italic = TRUE, bold = FALSE, align = NULL, indent = TRUE) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!(is.numeric(start) && length(start) == 1 && is_wholenumber(start))) stop("'start' must be an integer scalar")
  start <- resolve_location(cells(columns = 1, rows = start), x)
  if (!(is.numeric(nrows) && length(nrows) == 1 & is_wholenumber(nrows))) stop("'nrows' must be an integer scalar")
  if (!is.null(label) && !(is.character(label) && length(label) == 1)) stop("'label' must be a character scalar")
  if (isTRUE(indent)) indent <- "1em"
  if (isFALSE(indent)) indent <- "0pt"
  if (is.numeric(indent)) indent <- paste0(indent, "em")
  indent <- as_length(indent)
  if (length(indent) != 1) rlang::abort("'indent' must be a scalar length")

  if (!is.null(label)) {
  firstvar <- colnames(x$`_body`)[[1]]
  new_row <- tibble::tibble({{firstvar}} := label, `_insert_before` = start$rows)
  x$`_added_rows` <- tibble::add_row(x$`_added_rows`, new_row)
  x <- x %>%
    format_cells(cells(1, !!nrow(x$`_added_rows`), "added_rows"), bold = bold, italic = italic, align = align)
  }
  x %>% add_indent(cells(1, start$rows + seq_len(nrows) - 1), indent)
}
