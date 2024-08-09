#' Add a footnote to a Typst table
#'
#' @param x A Typst table
#' @param content The content of the footnote
#' @param type The labelling style for the footnote. Either `"general"` (no
#'     label), `"number"` (numeric labelling), `"alphabet"` (alphabetic
#'     numbering), or `"symbol"` (symbolic numbering).
#' @param location The cell(s) in which the footnote reference should be
#'     inserted. Should be a `cells_location` object created by [cells()]. Only
#'     if `type != "general"`.
#'
#' @returns A Typst table with the footnote added.
#' @export
add_footnote <- function(x, content, type = "general", location) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")
  if (!is.character(content)) stop("'content' must be a character vector")
  if (!is.character(type) || length(type) != 1 || !(type %in% c("general", "number", "alphabet", "symbol")))
    stop("'type' must be one of c('general', 'number', 'alphabet', 'symbol'")
  if (type != "general") {
    if (missing(location)) stop("'location' must be provided if 'type' is one of c('number', 'alphabet', 'symbol')")
    location <- resolve_location(location, x)
  } else {
    if (!missing(location)) stop("'location' cannot be provided if type is 'general'")
    location <- NULL
  }

  footnote <- tibble::tibble(location = list(location),
                             type = type,
                             content = content)

  x$`_footnotes` <- dplyr::bind_rows(x$`_footnotes`, footnote)
  x
}
