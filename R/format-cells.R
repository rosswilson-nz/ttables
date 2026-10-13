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
#' @param fill Cell fill colour. Should be Typst
#'     [color](https://typst.app/docs/reference/visualize/color/)
#'     specification.
#'
#' @returns A Typst table with formatting specification applied to the given
#'     cell(s).
#'
#' @export
format_cells <- function(
  x,
  location,
  bold = NULL,
  italic = NULL,
  align = NULL,
  indent = NULL,
  size = NULL,
  stroke = NULL,
  fill = NULL
) {
  if (!inherits(x, "ttables_tbl")) {
    stop("'x' must be a `ttables_tbl` object")
  }
  location <- expand_location(resolve_location(location, x))

  if (!is.null(bold) && !(is.logical(bold) && length(bold) == 1)) {
    stop("'bold' must be a logical scalar")
  }
  if (!is.null(italic) && !(is.logical(italic) && length(italic) == 1)) {
    stop("'italic' must be a logical scalar")
  }
  if (!is.null(align)) {
    align <- check_align(align, 1)
  }
  if (!is.null(indent)) {
    indent <- check_indent(indent)
  }
  if (!is.null(size)) {
    size <- check_size(size)
  }
  if (!is.null(stroke)) {
    stroke <- check_stroke(stroke)
  } else {
    stroke <- list(stroke)
  }
  if (!is.null(fill)) {
    fill <- check_fill(fill)
  }

  format <- vctrs::data_frame(
    location,
    bold = bold %||% NA,
    italic = italic %||% NA,
    align = align %||% NA_alignment_,
    indent = indent %||% NA_length_,
    size = size %||% NA_length_,
    stroke = stroke %||% list(NULL),
    fill = fill %||% NA_colour_
  )
  x$`_format` <- merge_formats(x$`_format`, format)
  x
}

check_indent <- function(x) {
  if (rlang::is_bare_numeric(x)) {
    x <- paste0(x, "em")
  }
  x <- as_length(x)
  if (length(x) != 1) {
    rlang::abort("'x' must be a scalar length")
  }
  x
}

check_size <- function(x) {
  if (rlang::is_bare_numeric(x)) {
    x <- paste0(x, "pt")
  }
  x <- as_length(x)
  if (length(x) != 1) {
    rlang::abort("'x' must be a scalar length")
  }
  x
}

check_stroke <- function(x, list_ok = TRUE) {
  if (rlang::is_scalar_character(x)) {
    switch(x,
      none = list(none()),
      list(vec_cast(x, stroke()))
    )
  } else if (list_ok && rlang::is_bare_list(x)) {
    if (
      !all(names(x) %in% c("left", "right", "top", "bottom", "x", "y", "rest"))
    ) {
      rlang::abort("Invalid item names in 'x'")
    }
    list(as_dictionary(lapply(x, \(y) check_stroke(y, FALSE))))
  } else {
    x <- as_stroke(x)
    if (length(x) != 1) {
      rlang::abort("'x' must be a scalar stroke specification")
    }
    list(x)
  }
}

check_fill <- function(x) {
  if (rlang::is_scalar_character(x)) {
    switch(x,
      none = none(),
      vec_cast(x, colour())
    )
  } else {
    x <- as_colour(x)
    if (length(x) != 1) {
      rlang::abort("'x' must be a scalar colour specification")
    }
    x
  }
}

merge_formats <- function(old, new) {
  out <- as.list(old)
  for (i in seq_len(nrow(new))) {
    r <- which(
      old$column == new$column[[i]] &
        old$row == new$row[[i]] &
        old$location == new$location[[i]]
    )
    if (length(r)) {
      for (c in seq_len(length(out))) {
        if (!is.na(new[[c]][i])) out[[c]][r] <- new[[c]][i]
      }
    } else {
      for (c in seq_len(length(out))) {
        out[[c]] <- c(out[[c]], new[[c]][i])
      }
    }
  }
  vctrs::new_data_frame(out)
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
  if (!inherits(x, "ttables_tbl")) {
    stop("'x' must be a `ttables_tbl` object")
  }
  location <- expand_location(resolve_location(location, x))
  if (is.numeric(amount)) {
    amount <- paste0(amount, "em")
  }
  indent <- as_length(amount)
  if (length(indent) != 1) {
    rlang::abort("'indent' must be a scalar length")
  }
  format <- as.list(x$`_format`)
  for (i in seq_len(nrow(location))) {
    r <- which(
      format$column == location$column[[i]] &
        format$row == location$row[[i]] &
        format$location == location$location[[i]]
    )
    if (length(r)) {
      if (is.na(format$indent[[r]])) {
        format$indent[[r]] <- indent
      } else {
        format$indent[[r]] <- vec_arith.ttables_length.ttables_length(
          "+",
          format$indent[[r]],
          indent
        )
      }
    } else {
      format$column <- c(format$column, location$column[[i]])
      format$row <- c(format$row, location$row[[i]])
      format$location <- c(format$location, location$location[[i]])
      format$indent <- c(format$indent, indent)
      format$bold <- c(format$bold, NA)
      format$italic <- c(format$italic, NA)
      format$align <- c(format$align, NA_alignment_)
      format$size <- c(format$size, NA_length_)
      format$stroke <- c(format$stroke, list(NULL))
      format$fill <- c(format$fill, NA_colour_)
    }
  }
  x$`_format` <- vctrs::new_data_frame(format)
  x
}
