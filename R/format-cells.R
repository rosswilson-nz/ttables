format_cells <- function(x, location, bold = NULL, italic = NULL, align = NULL, indent = NULL) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  location <- expand_location(resolve_location(location, x))

  if (!is.null(bold) && !(is.logical(bold) && length(bold) == 1)) stop("'bold' must be a logical scalar")
  if (!is.null(italic) && !(is.logical(italic) && length(italic) == 1)) stop("'italic' must be a logical scalar")
  if (!is.null(align) && !(is.character(align) && length(align) == 1 && align %in% c("left", "centre", "center", "right", "auto")))
    stop("'align' must be one of c('left', 'centre', 'center', 'right', 'auto'")
  if (!is.null(indent)) {
    if (!((is.numeric(indent) || is.character(indent)) && length(indent) == 1)) stop("'indent' must be a numeric or character scalar")
    if (is.numeric(indent)) indent <- paste0(indent, "fr")
    n <- nchar(indent)
    if (!(substring(indent, n - 1, n) %in% c("pt", "mm", "cm", "in", "em"))) stop("'indent' must be a valid Typst length")
    indent <- resolve_indent(indent)
  }

  format <- tibble::tibble(location, bold = bold %||% NA, italic = italic %||% NA, align = align %||% NA_character_, indent = indent %||% ttables_length(abs_length(NA), NA))
  x$`_format` <- merge_formats(x$`_format`, format)
  x
}

merge_formats <- function(old, new) {
  out <- dplyr::full_join(old, new, by = c("column", "row", "location"))
  dplyr::mutate(out,
                bold = dplyr::coalesce(bold.y, bold.x),
                italic = dplyr::coalesce(italic.y, italic.x),
                align = dplyr::coalesce(align.y, align.x),
                indent = dplyr::coalesce(indent.x, indent.y),
                .keep = "unused")
}

add_indent <- function(x, location, amount = "1em") {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  location <- expand_location(resolve_location(location, x))
  format <- tibble::tibble(location, indent = resolve_indent(amount))
  x$`_format` <- dplyr::mutate(
    dplyr::full_join(x$`_format`, format, by = c("column", "row", "location")),
    indent = (dplyr::coalesce(indent.x, ttables_length(abs_length(0), 0))
              + dplyr::coalesce(indent.y, ttables_length(abs_length(0), 0))),
    .keep = "unused"
  )
  x
}

resolve_indent <- function(x) {
  n <- nchar(x)
  if (substring(x, n - 1, n) %in% c("pt", "mm", "cm", "in")) {
    new_length(abs_length(as.numeric(substring(x, 1, n - 2)), substring(x, n - 1, n)), 0)
  } else {
    new_length(abs_length(0), as.numeric(substring(x, 1, n - 2)))
  }
}
