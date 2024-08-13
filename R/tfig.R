## These are all to be revised, as per the corresponding functions in ttab.R

#' Create figures in Typst format
#'
#' This wraps the path to a saved image in a Typst figure environment with caption and label.
#'
#' @param x Path to a saved image in SVG, PNG, JPEG, or GIF format.
#' @param caption The figure caption.
#' @param label Figure label, used for cross-referencing
#' @param placement (optional) Figure placement. As in Typst's #figure() function.
#' @param width,height (optional) Image width and height. As in Typst's #image() function.
#' @param footnotes Footnotes to add below the table. Pass a vector for multiple
#'     footnotes. At this stage, footnote numbering needs to be added manually
#'     (as does the corresponding numbering in table cells).
#'
#' @export
tfig <- function(x, caption = NULL, label = NULL, placement = NULL,
                 width = NULL, height = NULL, footnotes = NULL) {
  if (is.character(x) && length(x) == 1 &&
      tolower(fs::path_ext(x)) %in% c("png", "jpeg", "jpg", "gif", "svg")) {
    `_image` <- fs::path_rel(x, "reports")
  } else {
    stop("`x` must be the path to an image file in format SVG (preferred), PNG, JPEG, or GIF")
  }
  if (!is.null(caption) && (!is.character(caption) || length(caption) > 1)) stop("'caption' must be a character scalar")
  if (!is.null(label) && (!is.character(label) || length(label) > 1)) stop("'label' must be a character scalar")
  if (!is.null(placement) && (!is.character(placement) || length(placement) > 1)) stop("'placement' must be a character scalar")
  if (!is.null(width) && (!(is.character(width) || is.numeric(width)) || length(width) != 1)) stop("'width' must be a character or numeric scalar")
  if (!is.null(height) && (!(is.character(height) || is.numeric(height)) || length(height) != 1)) stop("'height' must be a character or numeric scalar")
  if (!is.null(footnotes) && !is.character(footnotes)) stop("'footnotes' must be a character vector")

  `_opts` <- list(caption = caption,
                  label = label,
                  placement = placement,
                  width = width,
                  height = height)
  `_opts` <- collate_initial_figure_opts(caption = caption,
                                         label = label,
                                         placement = placement,
                                         width = width,
                                         height = height)
  `_footnotes` <- footnotes

  structure(list(
    `_image` = `_image`,
    `_opts` = `_opts`,
    `_footnotes` = `_footnotes`
  ), class = "ttables_fig")
}

collate_initial_figure_opts <- function(caption, label, placement, width, height) {
  list(caption = caption,
       label = label,
       placement = placement,
       width = width,
       height = height,
       supplement = FALSE,
       landscape = FALSE)
}

#' @export
as_typst.ttables_fig <- function(x, ...) {
  image <- x$`_image`
  opts <- x$`_opts`
  footnotes <- x$`_footnotes`

  kind <- if (opts$supplement) "\"suppl-image\"" else "image"

  # inner content (image)
  content <- paste0(
    "#image(\"", image, "\"",
    if (!is.null(opts$width)) paste0(",
            width: ", opts$width),
    if (!is.null(opts$height)) paste0(",
            height: ", opts$height), ")"
  )

  # additional styling, if needed
  if (!is.null(attr(x, "styling"))) { # additional styling, if needed
    content <- paste(paste("#", attr(x, "styling"), sep = "", collapse = "
"), content, sep = "
")
  }

  # wrap in (inner) figure(), with metadata, and (outer) figure(), with footnotes
  out <- paste0(
    "#figure(", if(!is.null(opts$placement)) paste0("placement: ", opts$placement, ", "), "kind: \"none\", supplement: none)[
  #figure(", if (!is.null(opts$caption)) paste0("caption: [", opts$caption, "],
          "),  "kind: ", kind, ",
          placement: none,
          [", content, "])", if (!is.null(opts$label)) paste0(" <", opts$label, ">"), "

  ",
    paste(footnotes, collapse = "

  "),
    "
]"
  )

  # wrap in blank lines to separate from surrounding content
  out <- paste0("
", out, "
")

  out
}
