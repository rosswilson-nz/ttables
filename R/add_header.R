#' Add a header row to a Typst table
#'
#' @param x A Typst table
#' @param content The cell contents for the new row. Should be a character
#'     vector with one element per column of `x` (minus any spanned columns
#'     specified in `span`).
#' @param position Position at which the new header row should be inserted.
#'     Default is `1` (i.e. the new row should be the first header row).
#' @param span List of columns that should be grouped together into column
#'     spans. Each list element should be an integer vector of length 2 giving
#'     the first column and number of spanned columns for the group (e.g.,
#'     `c(2, 3)` indicates that columns 2, 3, and 4 should be combined into one
#'     column-spanning cell). Only the first cell of each span should be
#'     included in the `content` vector.
#'
#' @returns A Typst table with the new header row added.
#' @export
add_header <- function(x, content, position = 1, span = list()) {
  if (!inherits(x, "ttables_tbl")) stop("'x' must be a `ttables_tbl` object")
  if (!is.character(content)) stop("'content' must be a character vector")
  if (!rlang::is_list(span)) rlang::abort("'span' must be a list")
  if (!all(vapply(span, \(s) rlang::is_bare_numeric(s, 2), logical(1)))) rlang::abort("Each span element must be a vector c(<start-column>, <width>)")

  spans <- vector("list", length(span))
  for (i in seq_along(span)) {
    s <- span[[i]]
    if (is.null(names(s))) names(s) <- c("column", "width")
    spans[[i]] <- list(cell = cells(!!s[["column"]], !!position, "header"),
                       size = s[["width"]] - 1)
    content <- append(content, rep("", s[["width"]] - 1), s[["column"]])
  }
  if (length(content) != ncol(x$`_header`)) stop("'content' must have one entry per table column")
  names(content) <- colnames(x$`_header`)
  header <- tibble::as_tibble_row(content)
  x$`_header` <- tibble::add_row(x$`_header`, header, .before = position)
  for (s in spans) x <- colspan(x, s$cell, s$size)
  x
}
