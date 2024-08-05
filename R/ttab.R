#' Create tables in Typst format
#'
#' This is a table generator inspired by *kableExtra* and *gt*. This base
#' function takes an R data frame and sets up a basic Typst table. Auxiliary
#' functions allow the adding of additional formatting features.
#'
#' @param x An R data frame.
#' @param caption The table caption.
#' @param label Table label, used for cross-referencing
#' @param align Column alignment. Either a single value or a vector with the
#'     same length as the number of columns in `x`. Possible values are
#'     `"left"`,  `"center"`, `"right"`.
#' @param widths Columns widths. Either 'auto' for automatically determined
#'     column widths, a vector of numeric values (for relative widths), or a
#'     vector of Typst track sizes ('auto' or Typst fixed, relative, or
#'     fractional lengths).
#' @param placement (optional) Table placement. As in Typst's #figure() function.
#' @param footnotes Footnotes to add below the table. Pass a vector for multiple
#'     footnotes. At this stage, footnote numbering needs to be added manually
#'     (as does the corresponding numbering in table cells).
#'
#' @export
ttab <- function(x, caption = NULL, label = NULL, rownames = NULL, colnames = NULL, align = "left", widths = "auto", placement = "auto", fontsize = NA_integer_) {
  if (!is.data.frame(x)) stop("'x' must be a data frame")

  `_body` <- tibble::as_tibble(x, rownames = rownames)

  if (is.null(colnames)) colnames <- colnames(`_body`)
  `_header` <- tibble::as_tibble_row(setNames(colnames(`_body`), colnames))

  `_format` <- tibble::tibble(
    column = NA_integer_,
    row = NA_integer_,
    location = "table",
    bold = FALSE,
    italic = FALSE,
    align = align,
    indent = ttables_length(abs_length(0), 0),
    size = fontsize
  )

  tibble::as_tibble(lapply(`_body`, \(x) rep(list(cell_format()), ncol(`_body`))))
  `_opts` <- new_table_options(widths = widths,
                               placement = placement,
                               caption = caption,
                               label = label,
                               nc = ncol(`_body`))

  `_footnotes` <- tibble::tibble(
    location = list(),
    type = character(),
    content = character()
  )

  `_layout` <- tibble::tibble(
    column = integer(),
    row = integer(),
    location = character(),
    size = list(),
    combine = character()
  )

  `_added_rows` <- dplyr::mutate(`_body`[0, ], `_insert_before` = integer())

  structure(list(
    `_opts` = `_opts`,
    `_format` = `_format`,
    `_layout` = `_layout`,
    `_header` = `_header`,
    `_body` = `_body`,
    `_added_rows` = `_added_rows`,
    `_footnotes` = `_footnotes`
  ), class = "typst_table")
}
