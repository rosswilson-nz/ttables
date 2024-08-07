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

print.ttables_table_opts <- function(x, ...) {
  str(x)
}

check_widths <- function(x, ncols) {
  if (rlang::is_scalar_character(x)) {
    switch(x,
           auto = auto(),
           span = check_widths(rep("1fr", ncols), ncols),
           if (ncols == 1) as_length(x) else check_widths(rep(x, ncols), ncols))
  } else if (rlang::is_scalar_integer(x) | rlang::is_scalar_double(x)) {
    if (ncols == 1) as_length(paste0(x, "fr")) else check_widths(rep(x, ncols), ncols)
  } else {
    stopifnot(length(x) == ncols)
    if (is.numeric(x)) x <- paste0(x, "fr")
    as_length(x)
  }
}

check_align <- function(x, ncols) {
  if (rlang::is_scalar_character(x)) {
    switch(x,
           auto = auto(),
           if (ncols == 1) vec_cast(x, alignment()) else check_align(rep(x, ncols), ncols))
  } else {
    stopifnot(length(x) == ncols)
    vec_cast(x, alignment())
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
format.auto <- function(x, ...) "auto"
print.auto <- function(x, ...) cat("auto")
none <- function() structure("none", class = "none")
format.none <- function(x, ...) "none"
print.none <- function(x, ...) cat("none")

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

set_table_options <- function(x, widths, align, placement, caption,
                              label, footnotes.order, footnotes.number,
                              footnotes.alphabet, footnotes.symbol, footnotes.direction,
                              supplement, landscape, na) {
  stopifnot(inherits(x, "typst_table"))

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
