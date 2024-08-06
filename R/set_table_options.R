new_table_options <- function(widths, align, placement, caption, label, nc) {
  structure(
    list(columns.widths = check_columns.widths(widths, nc),
         columns.align = check_columns.align(align, nc),
         table.placement = check_table.placement(placement),
         table.caption = check_table.caption(caption),
         table.label = check_table.label(label),
         table.footnotes.order = check_table.footnotes.order(c("general", "number", "alphabet", "symbol")),
         table.footnotes.number = check_table.footnotes.number("arabic"),
         table.footnotes.alphabet = check_table.footnotes.alphabet("lower"),
         table.footnotes.symbol = check_table.footnotes.symbol("extended"),
         table.footnotes.direction = check_table.footnotes.direction("horizontal"),
         cells.na = check_cells.na("---"),
         page.landscape = check_page.landscape(FALSE)),
    class = "table_opts"
  )
}

set_table_options <- function(x, columns.widths, table.placement, table.caption,
                              table.label, table.footnotes.order, table.footnotes.number,
                              table.footnotes.alphabet, table.footnotes.symbol, table.footnotes.direction,
                              page.landscape, cells.na) {
  stopifnot(inherits(x, "typst_table"))

  opts <- x$`_opts`

  if (!missing(columns.align)) opts$columns.align <- check_columns.align(columns.align, ncol(x$`_data`))
  if (!missing(columns.widths)) opts$columns.widths <- check_columns.widths(columns.widths, ncol(x$`_data`))
  if (!missing(table.placement)) opts$table.placement <- check_table.placement(table.placement)
  if (!missing(table.caption)) opts$table.caption <- check_table.caption(table.caption)
  if (!missing(table.label)) opts$table.label <- check_table.label(table.label)
  if (!missing(table.footnotes.order)) opts$table.footnotes.order <- check_table.footnotes.order(table.footnotes.order)
  if (!missing(table.footnotes.number)) opts$table.footnotes.number <- check_table.footnotes.number(table.footnotes.number)
  if (!missing(table.footnotes.alphabet)) opts$table.footnotes.alphabet <- check_table.footnotes.alphabet(table.footnotes.alphabet)
  if (!missing(table.footnotes.symbol)) opts$table.footnotes.symbol <- check_table.footnotes.symbol(table.footnotes.symbol)
  if (!missing(table.footnotes.direction)) opts$table.footnotes.direction <- check_table.footnotes.direction(table.footnotes.direction)
  if (!missing(cells.na)) opts$cells.na <- check_cells.na(cells.na)
  if (!missing(page.landscape)) opts$page.landscape <- check_page.landscape(page.landscape)

  x$`_opts` <- opts
  x
}

check_columns.align <- function(x, nc) {
  if (!is.character(x) || !(length(x) %in% c(1, nc))) stop("'align' must have the one element per table column")
  if (!is_valid_align(x)) stop("Each element of 'align' must be one of c('left', 'centre', 'center', 'right', 'auto')")
  structure(x, class = "columns_align")
}

is_valid_align <- function(x) all(x %in% c("left", "centre", "center", "right", "auto"))

check_columns.widths <- function(x, nc) {
  if (!(is.character(x) || is.numeric(x)) || !(length(x) %in% c(1, nc))) stop("'widths' must have the one element per table column")
  if (is.numeric(x)) x <- paste0(x, "fr")
  if (!is_valid_width(x)) stop("Each element of 'width' must be a valid Typst length or fractional size")
  structure(x, class = "columns_widths")
}

is_valid_width <- function(x) {
  if (length(x) == 1 && x %in% c("auto", "span")) TRUE else {
    n <- nchar(x)
    all(substring(x, n - 1, n) %in% c("pt", "mm", "cm", "in", "em", "fr"))
  }
}

check_columns.align <- function(x, nc) {
  if (!is.character(x) || !(length(x) %in% c(1, nc))) stop("'align' must have the one element per table column")
  if (!is_valid_align(x)) stop("Each element of 'align' must be a valid Typst alignment value")
  structure(x, class = "columns_align")
}

is_valid_align <- function(x) {
  if (length(x) == 1 && x == "auto") TRUE else {
    horiz <- c("left", "center", "right")
    vert <- c("top", "horizon", "bottom")
    horiz.vert <- apply(expand.grid(horiz, vert), 1, \(x) paste(x, collapse = " + "))
    vert.horiz <- apply(expand.grid(vert, horiz), 1, \(x) paste(x, collapse = " + "))
    all(x %in% c(horiz, vert, horiz.vert, vert.horiz))
  }
}

check_table.placement <- function(x) {
  if (!is.null(x) && (!is.character(x) || length(x) != 1)) stop("'placement' must be a character scalar")
  if (!is_valid_placement(x)) stop("'placement' must be one of c('none', 'auto', 'top', 'bottom')")
  structure(x, class = "table_placement")
}

is_valid_placement <- function(x) x %in% c("none", "auto", "top", "bottom")

check_table.caption <- function(x) {
  if (!is.null(x) && (!is.character(x) || length(x) != 1)) stop("'caption' must be a character scalar")
  if (is.null(x)) x <- character()
  structure(x, class = "table_caption")
}

check_table.label <- function(x) {
  if (!is.null(x) && (!is.character(x) || length(x) != 1)) stop("'label' must be a character scalar")
  if (is.null(x)) x <- character()
  structure(x, class = "table_label")
}

check_table.footnotes.order <- function(x) {
  if (!is.character(x)) stop("'table.footnotes.order' must be a character vector")
  if (!is_valid_footnotes_order(x)) stop("'table.footnotes.order' must be a permutation of c('general', 'number', 'alphabet', 'symbol')")
  structure(x, class = "table_footnotes_order")
}

is_valid_footnotes_order <- function(x) {
  length(x) == 4 & setequal(x, c("general", "number", "alphabet", "symbol"))
}

check_table.footnotes.number <- function(x) {
  if (!is.character(x) || length(x) != 1) stop("'table.footnotes.number' must be a character scalar")
  if (!is_valid_footnotes_number(x)) stop("'table.footnotes.number' must be one of c('arabic', 'roman', 'Roman')")
  structure(x, class = "table_footnotes_number")
}

is_valid_footnotes_number <- function(x) x %in% c("arabic", "roman", "Roman")

check_table.footnotes.alphabet <- function(x) {
  if (!is.character(x) || length(x) != 1) stop("'table.footnotes.alphabet' must be a character scalar")
  if (!is_valid_footnotes_alphabet(x)) stop("'table.footnotes.alphabet' must be one of c('lower', 'upper')")
  structure(x, class = "table_footnotes_alphabet")
}

is_valid_footnotes_alphabet <- function(x) x %in% c("lower", "upper")

check_table.footnotes.symbol <- function(x) {
  if (!is.character(x)) stop("'table.footnotes.symbol' must be a character vector")
  structure(x, class = "table_footnotes_symbol")
}

check_table.footnotes.direction <- function(x) {
  if (!is.character(x) || length(x) != 1) stop("'table.footnotes.direction' must be a character scalar")
  if (!is_valid_footnotes_direction(x)) stop("'table.footnotes.direction' must be one of c('horizontal', 'vertical')")
  structure(x, class = "table_footnotes_direction")
}

is_valid_footnotes_direction <- function(x) x %in% c("horizontal", "vertical")

check_cells.na <- function(x) {
  if (!is.character(x) || length(x) != 1) stop("'table.footnotes.direction' must be a character scalar")
  structure(x, class = "cells_na")
}

check_page.landscape <- function(x) {
  if (!is.logical(x) || length(x) != 1) stop("'page.landscape' must be a logical scalar")
  structure(x, class = "page_landscape")
}
