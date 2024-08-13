#' Set Typst table options
#'
#' @param x A Typst table
#' @param width,height Figure width and height.
#' @param placement Figure placement. Either `"none"`, `"auto"`, `"top"`,
#'     `"bottom"`. The default is `"auto"`.
#' @param caption The figure caption.
#' @param label Figure label, used for cross-referencing
#' @param supplement Whether the figure is to be placed in supplementary material
#'     in the output. This only changes the Typst 'kind' parameter to
#'     `"suppl-image"` instead of `"image"`. Typst templates may make use of
#'     this to format the figure differently. Default is `FALSE.`
#' @param landscape Whether the figure should be placed on its own landscape
#'     page. Default is `FALSE`.
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
