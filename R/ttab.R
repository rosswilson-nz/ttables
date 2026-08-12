#' Create tables in Typst format
#'
#' This is a table generator inspired by *kableExtra* and *gt*. This base
#' function takes an R data frame and sets up a basic Typst table. Auxiliary
#' functions allow the adding of additional formatting features.
#'
#' @param x An R data frame.
#' @param caption The table caption.
#' @param label Table label, used for cross-referencing
#' @param rownames How to treat existing row names of `x`. Same specification as
#'     [tibble::as_tibble()].
#' @param colnames Column names to be used in the table. Default is to take the
#'     existing column names of `x`.
#' @param align Column alignment. Either `"auto"` or a vector with the
#'     same length as the number of columns in `x`. Each element should be a
#'     Typst [alignment](https://typst.app/docs/reference/layout/alignment/)
#'     specification. A single non-`"auto"` value will be recycled to the number
#'     of columns of `x`.
#' @param widths Columns widths. Either `"auto"` for automatically determined
#'     column widths, `"span"` for equal widths spanning the full page, or a
#'     vector of Typst [track sizes](https://typst.app/docs/reference/layout/grid/).
#'     A numeric vector will be taken as fractional lengths.
#' @param placement Table placement. Either `"none"`, `"auto"`, `"top"`,
#'     `"bottom"`. The default is `"auto"`. See the Typst
#'     [figure](https://typst.app/docs/reference/model/figure/#parameters-placement)
#'     documentation for details.
#' @param fontsize Font size. A Typst
#'     [length](https://typst.app/docs/reference/text/text/#parameters-size)
#'     specification. A numeric `fontsize` will be taken as points.
#' @param gutter The gaps between table cells. Should be a list with elements
#'     `column` and `row`, each element being either `"auto"` or a Typst
#'     [length](https://typst.app/docs/reference/layout/length/) specification.
#'
#' @returns A `ttables_tbl` object describing the table content, formatting, and
#'     layout.
#'
#' @examples
#' ttab(mtcars)
#'
#' @export
ttab <- function(
  x,
  caption = NULL,
  label = NULL,
  rownames = NULL,
  colnames = NULL,
  align = "auto",
  widths = "auto",
  placement = "auto",
  fontsize = NULL,
  gutter = NULL
) {
  if (!is.data.frame(x)) {
    stop("'x' must be a data frame")
  }

  `_body` <- x
  if (is.null(rownames)) {
    rownames(`_body`) <- NULL
  } else if (is.character(rownames) && length(rownames) == 1) {
    `_body`[[rownames]] <- rownames(`_body`)
    rownames(`_body`) <- NULL
  }

  if (is.null(colnames)) {
    colnames <- colnames(`_body`)
  }
  names(colnames) <- colnames(`_body`)
  `_header` <- as.data.frame(as.list(colnames))

  fontsize <- if (is.null(fontsize)) {
    NA_length_
  } else if (is.numeric(fontsize)) {
    as_length(paste0(fontsize, "pt"))
  } else {
    as_length(fontsize)
  }

  `_format` <- vctrs::data_frame(
    column = NA_integer_,
    row = NA_integer_,
    location = "table",
    bold = NA,
    italic = NA,
    align = NA_alignment_,
    indent = NA_length_,
    size = fontsize,
    stroke = list(NULL),
    fill = NA_colour_
  )

  `_opts` <- collate_initial_table_opts(
    widths = widths,
    align = align,
    gutter = gutter,
    placement = placement,
    caption = caption,
    label = label,
    fontsize = fontsize,
    ncols = ncol(`_body`)
  )

  `_footnotes` <- vctrs::data_frame(
    location = list(),
    type = character(),
    content = character()
  )

  `_layout` <- vctrs::data_frame(
    column = integer(),
    row = integer(),
    location = character(),
    size = list(),
    combine = character()
  )

  `_added_rows` <- `_body`[0, ]
  `_added_rows`["_insert_before"] <- integer()

  structure(
    list(
      `_opts` = `_opts`,
      `_format` = `_format`,
      `_layout` = `_layout`,
      `_header` = `_header`,
      `_body` = `_body`,
      `_added_rows` = `_added_rows`,
      `_footnotes` = `_footnotes`
    ),
    class = "ttables_tbl"
  )
}
