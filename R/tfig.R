#' Create figures in Typst format
#'
#' This wraps the path to a saved image in a Typst figure environment with caption and label.
#'
#' @param x Path to a saved image in SVG, PNG, JPEG, or GIF format.
#' @param caption The figure caption.
#' @param label Figure label, used for cross-referencing
#' @param placement (optional) Figure placement. As in Typst's #figure() function.
#' @param width,height (optional) Image width and height. As in Typst's #image() function.
#' @param footnotes Footnotes to add below the table.
#'
#' @export
tfig <- function(x, caption = NULL, label = NULL, placement = "auto",
                 width = "auto", height = "auto", footnotes = NULL) {
  `_image` <- check_image_path(x)

  `_opts` <- collate_initial_figure_opts(caption = caption,
                                         label = label,
                                         placement = placement,
                                         width = width,
                                         height = height)
  `_footnotes` <- check_footnotes(footnotes)

  structure(list(
    `_image` = `_image`,
    `_opts` = `_opts`,
    `_footnotes` = `_footnotes`
  ), class = "ttables_fig")
}

check_image_path <- function(x) {
    if (is.character(x) && length(x) == 1 &&
      tolower(fs::path_ext(x)) %in% c("png", "jpeg", "jpg", "gif", "svg")) {
    `_image` <- fs::path(x)
  } else {
    stop("`x` must be the path to an image file in format SVG (preferred), PNG, JPEG, or GIF")
  }
}

check_footnotes <- function(x) {
  if (!is.null(x) && !is.character(x)) stop("'footnotes' must be a character vector")
  x
}
