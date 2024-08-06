as_typst <- function(x) {
  widths <- print_widths(x$opts$columns.widths, ncol(x$header))
  align <- print_align(x$opts$columns.align, ncol(x$header))
  kind <- if (isTRUE(attr(x, "supplement"))) "\"suppl-table\"" else "table"

  header <- print_typst_content(x$header, x$opts)
  body <- print_typst_content(x$body, x$opts)
  footnotes <- print_footnotes(x$footnotes, x$opts)

  header <- format_spans(header, x$opts)
  body <- format_spans(body, x$opts)

  header <- print_cells(header)
  body <- print_cells(body)

  print_table(widths, align, kind, header, body, footnotes, x$opts)
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

print_align <- function(align, nc) {
  if (length(align) == 1) align <- rep(align, nc)
  wrap_paren(align)
}

print_typst_content <- function(mat, opts) {
  apply(mat, 1:2, \(x) format_contents(x[[1]], opts))
}

print_footnotes <- function(df, opts) {
  fn_ref <- glue::glue_data(
    df, "#super[{ref}] ",
    ref = dplyr::case_match(type,
                            "number" ~ get_fn_num(ref, opts$table.footnotes.number),
                            "alphabet" ~ get_fn_alph(ref, opts$table.footnotes.alphabet),
                            "symbol" ~ get_fn_sym(ref, opts$table.footnotes.symbol)),
    .na = NULL
  )
  glue::glue("{fn_ref}{content}", content = df$content, .na = "")
}

format_spans <- function(mat, opts) {
  for (c in seq_len(ncol(mat))) for (r in seq_len(nrow(mat))) {
    # first loop to collate column spans
    cell <- mat[[r, c]]
    if (!is.null(attr(cell, "colspan")) && attr(cell, "colspan") > 1) {
      if (!is.na(attr(cell, "combine"))) {
        mat[[r, c]][[1]] <- paste(vapply(mat[r, c + seq_len(attr(cell, "colspan")) - 1], \(x) x[[1]], character(1)),
                                  collapse = attr(cell, "combine"))
      }
      for (cc in c + seq_len(attr(cell, "colspan") - 1)) mat[[r, cc]] <- list(NULL)
      if (!is.null(attr(cell, "rowspan")) && attr(cell, "rowspan") > 1) {
        for (rr in r + seq_len(attr(cell, "rowspan") - 1)) {
          attr(mat[[rr, c]], "colspan") <- attr(cell, "colspan")
          attr(mat[[rr, c]], "combine") <- attr(cell, "combine")
        }
      }
    }
  }
  for (c in seq_len(ncol(mat))) for (r in seq_len(nrow(mat))) {
    # second loop to collate row spans
    cell <- mat[[r, c]]
    if (!is.null(attr(cell, "rowspan")) && attr(cell, "rowspan") > 1) {
      if (!is.na(attr(cell, "combine"))) {
        mat[[r, c]][[1]] <- paste(vapply(mat[r + seq_len(attr(cell, "rowspan")) - 1, c], \(x) x[[1]], character(1)),
                                  collapse = attr(cell, "combine"))
      }
      for (rr in r + seq_len(attr(cell, "rowspan") - 1)) mat[[rr, c]] <- list(NULL)
    }
  }
  mat
}

wrap_paren <- function(x, open = "(", close = ")", collapse = ",") {
  paste0(open, paste(x, collapse = collapse), close)
}

format_contents <- function(x, opts) {
  out <- glue::glue(x[[1]], .na = opts$cells.na)
  out <- add_footnote_refs(out, x, opts)
  out <- add_indents(out, x, opts)
  out <- add_textstyle(out, x, opts)
  out <- restore_attributes(out, x)

  out
}

print_cells <- function(mat) {
  apply(mat, 1:2, print_cell)
}

print_cell <- function(x) {
  if (is.null(x)) return(glue::glue())
  colspan <- if (!is.null(attr(x, "colspan"))) glue::glue("colspan: {colspan}",
                                                          colspan = attr(x, "colspan"))
  rowspan <- if (!is.null(attr(x, "rowspan"))) glue::glue("rowspan: {rowspan}",
                                                          rowspan = attr(x, "rowspan"))
  align <- if (!is.null(attr(x, "align"))) glue::glue("align: {align}",
                                                      align = attr(x, "align"))
  stroke <- if (!is.null(attr(x, "stroke"))) glue::glue("stroke: {stroke}",
                                                        stroke = attr(x, "stroke"))
  cellstyles <- glue::glue(colspan, rowspan, align, stroke, .sep = ", ", .null = NULL)
  if (length(cellstyles)) x <- glue::glue("#table.cell({cellstyles})[{x}]", .null = NULL)

  glue::glue("[{x}]")
}

print_table <- function(widths, align, kind, header, body, footnotes, opts) {
  glue::glue(
  "
{landscape_head}#figure(
  [#table(
    columns: {widths},
    align: {align},
    table.hline(),
    table.header({header}),
    table.hline(),
    {body},
    table.hline()
  )\n
  {footnotes}],
  kind: {kind}{caption}{placement}
){label}{landscape_tail}
",
  header = glue::glue_collapse(apply(header, 1, \(x) print_row(x)), sep = ",\n    "),
  body = glue::glue_collapse(apply(body, 1, \(x) print_row(x)), sep = ",\n    "),
  footnotes = glue::glue_collapse(footnotes, sep = "\n\n  "),
  caption = if (length(opts$table.caption)) glue::glue(",\ncaption: figure.caption(position: top)[{opts$table.caption}]"),
  placement = if (length(opts$table.caption)) glue::glue(",\nplacement: {opts$table.placement}"),
  label = if (length(opts$table.label)) glue::glue(" <{opts$table.label}>"),
  landscape_head = if (opts$page.landscape) glue::glue("#page(flipped: true)[\n"),
  landscape_tail = if (opts$page.landscape) glue::glue("]"),
  .null = NULL,
  .trim = FALSE
  )
}

print_row <- function(row) {
  glue::glue_collapse(vapply(row, \(x) x[[1]], character(1)),
                      sep = ", ")
}

add_footnote_refs <- function(out, x, opts) {
  fns <- get_footnotes(x, opts)
  if (is.null(fns)) out else glue::glue(
    "{out}{fn}",
    fn = glue::glue("#super[", fn, "]"),
    .null = NULL,
    .na = opts$cells.na
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
  switch(style, arabic = as.character(num), roman = tolower(utils::as.roman(num)), Roman = utils::as.roman(num))
}

get_fn_alph <- function(num, style) {
  switch(style, lower = letters[num], upper = LETTERS[num])
}

get_fn_sym <- function(num, style) {
  switch(style, standard = syms_standard[num], extended = syms_extended[num])
}

syms_standard <- c("#sym.ast.op", "#sym.dagger", "#sym.dagger.double", "#sym.section")
syms_extended <- c(syms_standard, "#sym.bar.v.double", "#sym.pilcrow")

add_indents <- function(out, x, opts) {
  if (is.null(attr(x, "indent"))) out else glue::glue(
    "{indent}{out}",
    indent = glue::glue("#h(", format(attr(x, "indent")), ")"),
    .null = NULL,
    .na = opts$cells.na
  )
}

add_textstyle <- function(out, x, opts) {
  bold <- if (!is.null(attr(x, "bold"))) glue::glue("weight: \"{bold}\"",
                                                    bold = if (attr(x, "bold")) "bold" else "regular")
  italic <- if (!is.null(attr(x, "italic"))) glue::glue("style: \"{italic}\"",
                                                        italic = if (attr(x, "italic")) "italic" else "normal")
  size <- if (!is.null(attr(x, "size"))) glue::glue("size: {size}",
                                                    size = glue::glue("{attr(x, 'size')}pt"))
  styles <- glue::glue(bold, italic, size, .sep = ", ", .null = NULL)
  if (length(styles) == 0) out else glue::glue("#text({styles})[{out}]",
                                               .null = NULL, .na = opts$cells.na)
}

restore_attributes <- function(out, x, attrs = c("colspan", "rowspan", "combine", "align", "stroke")) {
  out <- list(out)
  for (a in attrs) attr(out, a) <- attr(x, a)
  out
}
