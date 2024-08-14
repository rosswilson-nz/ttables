new_figure_opts <- function(width, height, placement, caption, label, supplement, landscape) {
  if (missing(width)) width <- auto()
  if (missing(height)) height <- auto()
  if (missing(placement)) placement <- auto()
  if (missing(caption)) caption <- character()
  if (missing(label)) label <- character()
  if (missing(supplement)) supplement <- FALSE
  if (missing(landscape)) landscape <- FALSE

  structure(
    list(width = width,
         height = height,
         placement = placement,
         caption = caption,
         label = label,
         supplement = supplement,
         landscape = landscape),
    class = "ttables_figure_opts"
  )
}

#' @export
print.ttables_figure_opts <- function(x, ...) {
  NextMethod()
}

#' Set Typst figre options
#'
#' @param x A Typst figure
#' @param width,height Figure width and height. Either `"auto"` or a Typst
#'     [length](https://typst.app/docs/reference/visualize/image/#parameters-width)
#'     specification.
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
set_figure_options <- function(x, width, height, placement, caption, label,
                               supplement, landscape) {
  stopifnot(inherits(x, "ttables_fig"))

  opts <- x$`_opts`

  if (!missing(width)) opts$width <- check_width(width)
  if (!missing(height)) opts$height <- check_height(height)
  if (!missing(placement)) opts$placement <- check_placement(placement)
  if (!missing(caption)) opts$caption <- check_caption(caption)
  if (!missing(label)) opts$label <- check_label(label)
  if (!missing(supplement)) opts$supplement <- check_supplement(supplement)
  if (!missing(landscape)) opts$landscape <- check_landscape(landscape)

  x$`_opts` <- opts
  x
}

check_width <- function(x) {
  if (rlang::is_scalar_character(x)) {
    switch(x,
           auto = auto(),
           as_relative(x))
  } else rlang::abort("Invalid width")
}

check_height <- function(x) {
  if (rlang::is_scalar_character(x)) {
    switch(x,
           auto = auto(),
           as_relative(x))
  } else rlang::abort("Invalid height")
}

collate_initial_figure_opts <- function(caption, label, placement, width, height) {
  caption <- check_caption(caption)
  label <- check_label(label)
  placement <- check_placement(placement)
  width <- check_width(width)
  height <- check_height(height)

  new_figure_opts(caption = caption,
                  label = label,
                  placement = placement,
                  width = width,
                  height = height)
}
