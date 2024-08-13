#' Set Typst table options
#'
#' @param x A Typst table
#' @param align Column alignment. Either `"auto"` or a vector with the
#'     same length as the number of columns in `x`. Possible values are
#'     `"left"`,  `"center"` (or `"centre"`), `"right"`. A single non-`"auto"`
#'     value will be recycled to the number of columns of `x`.
#' @param widths Columns widths. Either `"auto"` for automatically determined
#'     column widths, `"span"` for equal widths spanning the full page, or a
#'     vector of Typst track sizes. A numeric vector will be taken as fractional
#'     lengths.
#' @param placement Table placement. Either `"none"`, `"auto"`, `"top"`,
#'     `"bottom"`. The default is `"auto"`.
#' @param caption The table caption.
#' @param label Table label, used for cross-referencing
#' @param footnotes.order The order of different footnote types below the table.
#'     A permumation of `c('general', 'number', 'alphabet', 'symbol')`.
#' @param footnotes.number The style used for numeric footnote labels. One of
#'     `c("arabic", "roman", "Roman")`. Default is `"arabic"`.
#' @param footnotes.alphabet The style used for alphabetic footnote labels. One
#'     of `c("lower", "upper")`. Default is `"lower"`.
#' @param footnotes.symbol The style used for symbolic footnote labels. One of
#'     `c("standard", "extended")` or a character vector of symbols. The
#'     `"standard"` set is `*`, `†`, `‡`, and `§`; `"extended"` adds
#'     `‖` and `¶`. Default is `"extended"`.
#' @param footnotes.direction The table direction in which footnote numbering is
#'     accumulated. Either `"horizontal"` or `"vertical"`. Default is
#'     `"horizontal"`.
#' @param supplement Whether the table is to be placed in supplementary material
#'     in the output. This only changes the Typst 'kind' parameter to
#'     `"suppl-table"` instead of `"table"`. Typst templates may make use of
#'     this to format the table differently. Default is `FALSE.`
#' @param landscape Whether the table should be placed on its own landscape
#'     page. Default is `FALSE`.
#' @param na Character string to print for `NA` values. Default is `"---"`.
#'
#' @returns A Typst figure with the specified options set.
set_figure_options <- function(x, width, height, placement, caption,
                              label, supplement, landscape) {
  stopifnot(inherits(x, "ttables_fig"))

  opts <- x$`_opts`

  # check_XX() wrappers to be added
  if (!missing(width)) opts$width <- width
  if (!missing(height)) opts$height <- height
  if (!missing(placement)) opts$placement <- check_placement(placement)
  if (!missing(caption)) opts$caption <- check_caption(caption)
  if (!missing(label)) opts$label <- check_label(label)
  if (!missing(supplement)) opts$supplement <- check_supplement(supplement)
  if (!missing(landscape)) opts$landscape <- check_landscape(landscape)

  x$`_opts` <- opts
  x
}
