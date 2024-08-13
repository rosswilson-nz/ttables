new_table_opts <- function(widths = auto(), align, placement, caption, label, footnotes.order, footnotes.number,
                           footnotes.alphabet, footnotes.symbol, footnotes.direction,
                           supplement, landscape, na) {
  if (missing(widths)) widths <- auto()
  if (missing(align)) align <- auto()
  if (missing(placement)) placement <- auto()
  if (missing(caption)) caption <- character()
  if (missing(label)) label <- character()
  if (missing(footnotes.order)) footnotes.order <- c("general", "number", "alphabet", "symbol")
  if (missing(footnotes.number)) footnotes.number <- "arabic"
  if (missing(footnotes.alphabet)) footnotes.alphabet <- "lower"
  if (missing(footnotes.symbol)) footnotes.symbol <- "extended"
  if (missing(footnotes.direction)) footnotes.direction <- "horizontal"
  if (missing(supplement)) supplement <- FALSE
  if (missing(landscape)) landscape <- FALSE
  if (missing(na)) na <- "---"

  structure(
    list(widths = widths,
         align = align,
         placement = placement,
         caption = caption,
         label = label,
         footnotes.order = footnotes.order,
         footnotes.number = footnotes.number,
         footnotes.alphabet = footnotes.alphabet,
         footnotes.symbol = footnotes.symbol,
         footnotes.direction = footnotes.direction,
         supplement = supplement,
         landscape = landscape,
         na = na),
    class = "ttables_table_opts"
  )
}

#' @export
print.ttables_table_opts <- function(x, ...) {
  x
}

check_widths <- function(x, ncols) {
  if (rlang::is_scalar_character(x)) {
    switch(x,
           auto = as_width(rep("auto", ncols)),
           span = as_width(rep("1fr", ncols)),
           if (ncols == 1) as_width(x) else check_widths(rep(x, ncols), ncols))
  } else {
    stopifnot(length(x) == ncols)
    if (is.numeric(x)) x <- paste0(x, "fr")
    as_width(x)
  }
}

check_align <- function(x, ncols) {
  if (rlang::is_scalar_character(x)) {
    switch(x,
           auto = auto(),
           if (ncols == 1) vec_cast(x, alignment()) else check_align(rep(x, ncols), ncols))
  } else {
    stopifnot(length(x) == ncols)
    as_alignment(x)
  }
}

check_placement <- function(x) {
  if (rlang::is_scalar_character(x)) {
    if (x == "auto") return(auto())
    if (x == "none") return(none())
    x <- vec_cast(x, vert_alignment())
    if (x != "horizon") return(x)
  }
  rlang::abort("Invalid placement")
}

check_caption <- function(x) {
  if (is.null(x)) return(character())
  if (rlang::is_scalar_character(x)) return(x)
  rlang::abort("'caption' must be a character string")
}

check_label <- function(x) {
  if (is.null(x)) return(character())
  if (rlang::is_scalar_character(x) && !grepl("[^[:alnum:]_:.-]", x)) return(x)
  rlang::abort("'label' must be a character string representing a valid Typst label")
}

check_footnotes.order <- function(x) {
  if (rlang::is_character(x, 4) && setequal(x, c("general", "number", "alphabet", "symbol"))) return(x)
  rlang::abort("'footnotes.order' must be a permutation of c('general', 'number', 'alphabet', 'symbol')")
}

check_footnotes.number <- function(x) {
  if (rlang::is_scalar_character(x) && x %in% c("arabic", "roman", "Roman")) return(x)
  rlang::abort("'footnotes.number' must be one of c('arabic', 'roman', 'Roman')")
}

check_footnotes.alphabet <- function(x) {
  if (rlang::is_scalar_character(x) && x %in% c("lower", "upper")) return(x)
  rlang::abort("'footnotes.alphabet' must be one of c('lower', 'upper')")
}

check_footnotes.symbol <- function(x) {
  if (rlang::is_scalar_character(x) && x %in% c("standard", "extended")) return(x)
  if (rlang::is_character(x)) return(x)
  rlang::abort("'footnotes.symbol' must be one of c('standard', 'extended') to use the predefined sets, or a character vector of symbols")
}

check_footnotes.direction <- function(x) {
  if (rlang::is_scalar_character(x) && x %in% c("horizontal", "vertical")) return(x)
  rlang::abort("'footnotes.direction' must be one of c('horizontal', 'vertical')")
}

check_supplement <- function(x) {
  if (rlang::is_scalar_logical(x)) return(x)
  rlang::abort("'supplement' must be a logical scalar")
}

check_landscape <- function(x) {
  if (rlang::is_scalar_logical(x)) return(x)
  rlang::abort("'landscape' must be a logical scalar")
}

check_na <- function(x) {
  if (rlang::is_scalar_character(x)) return(x)
  rlang::abort("'na' must be a character scalar")
}

auto <- function() structure("auto", class = "auto")
#' @export
format.auto <- function(x, ...) "auto"
#' @export
print.auto <- function(x, ...) cat("auto")
is_auto <- function(x) inherits(x, "auto")
none <- function() structure("none", class = "none")
#' @export
format.none <- function(x, ...) "none"
#' @export
print.none <- function(x, ...) cat("none")
is_none <- function(x) inherits(x, "none")

collate_initial_table_opts <- function(widths, align, placement, caption, label, ncols) {
  widths <- check_widths(widths, ncols)
  align <- check_align(align, ncols)
  placement <- check_placement(placement)
  caption <- check_caption(caption)
  label <- check_label(label)

  new_table_opts(widths = widths,
                 align = align,
                 placement = placement,
                 caption = caption,
                 label = label)
}

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
#' @returns A Typst table with the specified options set.
set_table_options <- function(x, align, widths, placement, caption,
                              label, footnotes.order, footnotes.number,
                              footnotes.alphabet, footnotes.symbol, footnotes.direction,
                              supplement, landscape, na) {
  stopifnot(inherits(x, "ttables_tbl"))

  opts <- x$`_opts`

  if (!missing(align)) opts$align <- check_align(align, ncol(x$`_data`))
  if (!missing(widths)) opts$widths <- check_widths(widths, ncol(x$`_data`))
  if (!missing(placement)) opts$placement <- check_placement(placement)
  if (!missing(caption)) opts$caption <- check_caption(caption)
  if (!missing(label)) opts$label <- check_label(label)
  if (!missing(footnotes.order)) opts$footnotes.order <- check_footnotes.order(footnotes.order)
  if (!missing(footnotes.number)) opts$footnotes.number <- check_footnotes.number(footnotes.number)
  if (!missing(footnotes.alphabet)) opts$footnotes.alphabet <- check_footnotes.alphabet(footnotes.alphabet)
  if (!missing(footnotes.symbol)) opts$footnotes.symbol <- check_footnotes.symbol(footnotes.symbol)
  if (!missing(footnotes.direction)) opts$footnotes.direction <- check_footnotes.direction(footnotes.direction)
  if (!missing(supplement)) opts$supplement <- check_supplement(supplement)
  if (!missing(na)) opts$na <- check_na(na)
  if (!missing(landscape)) opts$landscape <- check_landscape(landscape)

  x$`_opts` <- opts
  x
}
