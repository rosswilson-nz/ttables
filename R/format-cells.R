#' Add formatting to Typst table cells
#'
#' @param x A Typst table
#' @param location Cells to apply the formating to. Should be a `cells_location`
#'     object created by [cells()].
#' @param bold,italic Whether to apply bold or italic styling to the cell(s).
#' @param align How to align the cells contents. Either `"auto"` or a Typst
#'     [alignment](https://typst.app/docs/reference/layout/alignment/)
#'     specification.
#' @param indent How much to indent cell contents. Should be a Typst
#'     [length](https://typst.app/docs/reference/layout/length/) specification.
#'     A numeric `indent` will be taken as 'em' units.
#' @param size Font size. Should be a Typst
#'     [length](https://typst.app/docs/reference/layout/length/) specification.
#'     A numeric `size` will be taken as points.
#' @param stroke Cell stroke (border) style. Should be Typst
#'     [stroke](https://typst.app/docs/reference/visualize/stroke/)
#'     specification. Only the paint and thickness components are supported for
#'     now.
#'
#' @returns A Typst table with formatting specification applied to the given
#'     cell(s).
#'
#' @export
format_cells <- function(x, location, bold = NULL, italic = NULL, align = NULL, indent = NULL,
                         size = NULL, stroke = NULL) {
  if (!inherits(x, "ttables_tbl")) stop("'x' must be a `ttables_tbl` object")
  location <- expand_location(resolve_location(location, x))

  if (!is.null(bold) && !(is.logical(bold) && length(bold) == 1)) stop("'bold' must be a logical scalar")
  if (!is.null(italic) && !(is.logical(italic) && length(italic) == 1)) stop("'italic' must be a logical scalar")
  if (!is.null(align)) align <- check_align(align, 1)
  if (!is.null(indent)) indent <- check_indent(indent)
  if (!is.null(size)) size <- check_size(size)
  if (!is.null(stroke)) stroke <- check_stroke(stroke)

  format <- tibble::tibble(location, bold = bold %||% NA, italic = italic %||% NA,
                           align = align %||% NA_alignment_,
                           indent = indent %||% NA_length_,
                           size = size %||% NA_length_,
                           stroke = stroke %||% list(NULL))
  x$`_format` <- merge_formats(x$`_format`, format)
  x
}

check_indent <- function(x) {
  if (rlang::is_bare_numeric(x)) x <- paste0(x, "em")
  x <- as_length(x)
  if (length(x) != 1) rlang::abort("'x' must be a scalar length")
  x
}

check_size <- function(x) {
  if (rlang::is_bare_numeric(x)) x <- paste0(x, "pt")
  x <- as_length(x)
  if (length(x) != 1) rlang::abort("'x' must be a scalar length")
  x
}

check_stroke <- function(x, list_ok = TRUE) {
  if (rlang::is_scalar_character(x)) {
    switch(x,
           none = list(none()),
           list(vec_cast(x, stroke())))
  } else if (list_ok && rlang::is_bare_list(x)) {
    if (!all(names(x) %in% c("left", "right", "top", "bottom"))) rlang::abort("Invalid item names in 'x'")
    list(dictionary(lapply(x, \(y) check_stroke(y, FALSE))))
  } else {
    x <- as_stroke(x)
    if (length(x) != 1) rlang::abort("'x' must be a scalar stroke specification")
    list(x)
  }
}

merge_formats <- function(old, new) {
  out <- dplyr::full_join(old, new, by = c("column", "row", "location"))
  out$bold <- dplyr::coalesce(out$bold.y, out$bold.x)
  out$bold.x <- out$bold.y <- NULL
  out$italic <- dplyr::coalesce(out$italic.y, out$italic.x)
  out$italic.x <- out$italic.y <- NULL
  out$align <- dplyr::coalesce(out$align.y, out$align.x)
  out$align.x <- out$align.y <- NULL
  out$indent <- dplyr::coalesce(out$indent.y, out$indent.x)
  out$indent.x <- out$indent.y <- NULL
  out$size <- dplyr::coalesce(out$size.y, out$size.x)
  out$size.x <- out$size.y <- NULL
  out$stroke <- dplyr::coalesce(out$stroke.y, out$stroke.x)
  out$stroke.x <- out$stroke.y <- NULL
  out
}

#' Add indentation to Typst table cells
#'
#' @param x A Typst table
#' @param location Cells to apply indenting to. Should be a `cells_location`
#'     object created by [cells()].
#' @param amount Amount of indentation to add. Should be a Typst
#'     [length](https://typst.app/docs/reference/text/text/#parameters-size)
#'     specification. A numeric `amount` will be taken as 'em' units.
#'
#' @returns A Typst table with indentation added to the specified cells.
#' @export
add_indent <- function(x, location, amount = "1em") {
  if (!inherits(x, "ttables_tbl")) stop("'x' must be a `ttables_tbl` object")
  location <- expand_location(resolve_location(location, x))
  if (is.numeric(amount)) amount <- paste0(amount, "em")
  indent <- as_length(amount)
  if (length(indent) != 1) rlang::abort("'indent' must be a scalar length")
  format <- tibble::tibble(location, indent = indent)
  format <- dplyr::full_join(x$`_format`, format, by = c("column", "row", "location"))
  indent.x <- tidyr::replace_na(format$indent.x, ttables_length(abs_length(0), em_length(0)))
  indent.y <- tidyr::replace_na(format$indent.y, ttables_length(abs_length(0), em_length(0)))
  format$indent <- vec_arith.ttables_length.ttables_length("+", indent.x, indent.y)
  format$indent.x <- format$indent.y <- NULL
  x$`_format` <- format
  x
}
