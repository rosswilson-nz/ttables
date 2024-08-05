as_typst <- function(x) {
  widths <- print_widths(x$opts$columns.widths, ncol(x$header))
  kind <- if (isTRUE(attr(x, "supplement"))) "\"suppl-table\"" else "table"

  header <- lapply(seq_len(nrow(x$header)), \(i) lapply(x$header[i, ], \(j) as.list(j)))
  header <- paste0(
    sapply(header, \(hr) paste0(sapply(hr, \(hc) format_contents(hc[[1]], x$opts)), collapse = ",")),
    collapse = ",\n      "
  )

  body <- lapply(seq_len(nrow(x$body)), \(i) lapply(x$body[i, ], \(j) as.list(j)))
  body <- paste0(
    sapply(body, \(br) paste0(sapply(br, \(bc) format_contents(bc[[1]], x$opts)), collapse = ",")),
    collapse = ",\n      "
  )
  #footnotes <-
  #print_cell()
}

print_widths <- function(widths, nc) {
  widths <- as.character(widths)
  if (identical(widths, "auto")) {
    widths <- nc
  } else if (identical(widths, "span")) {
    widths <- rep("1fr", nc)
  }
  wrap_paren(widths)
}

wrap_paren <- function(x, open = "(", close = ")", collapse = ",") {
  paste0(open, paste(x, collapse = collapse), close)
}

format_contents <- function(x, opts) {
  out <- glue::glue(x[[1]])
  out <- add_footnote_refs(out, x, opts)
  out <- add_indents(out, x)
  out <- add_textstyle(out, x)

  out
}

print_cell <- function(x) {
  colspan <- if (!is.null(attr(x, "colspan"))) glue::glue("colspan: {colspan}",
                                                          colspan = attr(x, "colspan"))
  rowspan <- if (!is.null(attr(x, "rowspan"))) glue::glue("rowspan: {rowspan}",
                                                          rowspan = attr(x, "rowspan"))
  align <- if (!is.null(attr(x, "align"))) glue::glue("align: {align}",
                                                      align = attr(x, "align"))
  stroke <- if (!is.null(attr(x, "stroke"))) glue::glue("stroke: {stroke}",
                                                        stroke = attr(x, "stroke"))
  cellstyles <- glue::glue(colspan, rowspan, align, stroke, .sep = ", ", .null = NULL)
  if (length(cellstyles)) out <- glue::glue("table.cell({cellstyles})[{out}]", .null = NULL)

  glue::glue("[{out}]")
}

add_footnote_refs <- function(out, x, opts) {
  fns <- get_footnotes(x, opts)
  if (is.null(fns)) out else glue::glue(
    "{out}{fn}",
    fn = glue::glue("#super[", fn, "]"),
    .null = NULL
  )

}

get_footnotes <- function(x, opts) {
  fn_num <- if (!is.null(attr(x, "footnote_num"))) glue::glue_collapse(get_fn_num(attr(x, "footnote_num"), opts$table.footnotes.number), sep = ",")
  fn_alph <- if (!is.null(attr(x, "footnote_alph"))) glue::glue_collapse(get_fn_alph(attr(x, "footnote_alph"), opts$table.footnotes.alphabet), sep = ",")
  fn_sym <- if (!is.null(attr(x, "footnote_sym"))) glue::glue_collapse(get_fn_sym(attr(x, "footnote_sym"), opts$table.footnotes.symbol), sep = ",")
  fn <- glue::glue(fn_num, fn_alph, fn_sym, .sep = ",", .null = NULL)
  if (length(fn)) fn else NULL
}

get_fn_num <- function(num, style) {
  switch(style, arabic = num, roman = tolower(utils::as.roman(num)), Roman = utils::as.roman(num))
}

get_fn_alph <- function(num, style) {
  switch(style, lower = letters[num], upper = LETTERS[num])
}

get_fn_sym <- function(num, style) {
  switch(style, standard = syms_standard[num], extended = syms_extended[num])
}

syms_standard <- c("#sym.ast.op", "#sym.dagger", "#sym.dagger.double", "#sym.section")
syms_extended <- c(syms_standard, "#sym.bar.v.double", "#sym.pilcrow")

add_indents <- function(out, x) {
  if (is.null(attr(x, "indent"))) out else glue::glue(
    "{indent}{out}",
    indent = glue::glue("#h(", format(attr(x, "indent")), ")"),
    .null = NULL
  )
}

add_textstyle <- function(out, x) {
  bold <- if (!is.null(attr(x, "bold"))) glue::glue("weight: \"{bold}\"",
                                                    bold = if (attr(x, "bold")) "bold" else "regular")
  italic <- if (!is.null(attr(x, "italic"))) glue::glue("style: \"{italic}\"",
                                                        italic = if (attr(x, "italic")) "italic" else "normal")
  size <- if (!is.null(attr(x, "size"))) glue::glue("size: {size}",
                                                    size = glue::glue("{attr(x, 'size')}pt"))
  styles <- glue::glue(bold, italic, size, .sep = ", ", .null = NULL)
  if (length(styles) == 0) out else glue::glue("#text({styles})[{out}]", .null = NULL)
}
