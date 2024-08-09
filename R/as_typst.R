#' Print a `typst_table` object as raw Typst
#'
#' @param x A Typst table
#' @returns A `glue` string containing the raw Typst output.
#'
#' @export
as_typst <- function(x) {
  x <- extract_table(x)

  widths <- print_widths(x$opts$widths, ncol(x$header))
  align <- print_align(x$opts$align, ncol(x$header))
  kind <- if (x$opts$supplement) "\"suppl-table\"" else "table"

  header <- print_typst_content(x$header, x$opts, x$footnotes)
  body <- print_typst_content(x$body, x$opts, x$footnotes)
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

print_typst_content <- function(mat, opts, fns) {
  apply(mat, 1:2, \(x) format_contents(x[[1]], opts, fns))
}

print_footnotes <- function(df, opts) {
  df <- df[order(factor(df$type, opts$footnotes.order), df$ref), ]
  ref <- dplyr::case_match(df$type,
                          "number" ~ get_fn_num(df$ref, opts$footnotes.number),
                          "alphabet" ~ get_fn_alph(df$ref, opts$footnotes.alphabet),
                          "symbol" ~ get_fn_sym(df$ref, opts$footnotes.symbol))

  glue::glue("{fn_ref}{content}",
             content = df$content,
             fn_ref = glue::glue("#super[{ref}] ", .na = NULL),
             .na = "")
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

format_contents <- function(x, opts, fns) {
  out <- glue::glue(x[[1]], .na = opts$na)
  out <- add_footnote_refs(out, x, fns, opts)
  out <- add_indents(out, x, opts)
  out <- add_textstyle(out, x, opts)
  out <- restore_attributes(out, x)

  out
}

print_cells <- function(mat) {
  apply(mat, 1:2, \(x) print_cell(x[[1]]))
}

print_cell <- function(x) {
  if (is.null(x[[1]])) return(glue::glue())
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
  caption = if (length(opts$caption)) glue::glue(",\ncaption: figure.caption(position: top)[{opts$caption}]"),
  placement = if (length(opts$caption)) glue::glue(",\nplacement: {opts$placement}"),
  label = if (length(opts$label)) glue::glue(" <{opts$label}>"),
  landscape_head = if (opts$landscape) glue::glue("#page(flipped: true)[\n"),
  landscape_tail = if (opts$landscape) glue::glue("]"),
  .null = NULL,
  .trim = FALSE
  )
}

print_row <- function(row) {
  drop <- vapply(row, \(x) length(x) == 0, logical(1))
  glue::glue_collapse(row[!drop], sep = ", ")
}

add_footnote_refs <- function(out, x, fns, opts) {
  fns <- get_footnotes(x, fns, opts)
  if (is.null(fns)) out else glue::glue(
    "{out}{fn}",
    fn = glue::glue("#super[", fns, "]"),
    .null = NULL,
    .na = opts$na
  )

}

get_footnotes <- function(x, fns, opts) {
  fn_num <- if (!is.null(attr(x, "footnote_num"))) {
    idxs <- fns[fns$type == "number", ]$ref
    glue::glue_collapse(get_fn_num(idxs[attr(x, "footnote_num")], opts$footnotes.number), sep = ",")
  }
  fn_alph <- if (!is.null(attr(x, "footnote_alph"))) {
    idxs <- fns[fns$type == "alphabet", ]$ref
    glue::glue_collapse(get_fn_alph(idxs[attr(x, "footnote_alph")], opts$footnotes.alphabet), sep = ",")
  }
  fn_sym <- if (!is.null(attr(x, "footnote_sym"))) {
    idxs <- fns[fns$type == "symbol", ]$ref
    glue::glue_collapse(get_fn_sym(idxs[attr(x, "footnote_sym")], opts$footnotes.symbol), sep = ",")
  }
  fn <- glue::glue(fn_num, fn_alph, fn_sym, .sep = ",", .null = NULL)
  if (length(fn)) fn else NULL
}

get_fn_num <- function(num, style) {
  switch(style, arabic = as.character(num), roman = tolower(utils::as.roman(num)), Roman = utils::as.roman(num))
}

get_fn_alph <- function(num, style) {
  out <- character(length(num))
  out[num != 0] <- switch(style, lower = letters[num], upper = LETTERS[num])
  out
}

get_fn_sym <- function(num, style) {
  out <- character(length(num))
  out[num != 0] <- switch(style, standard = syms_standard[num], extended = syms_extended[num])
  out
}

syms_standard <- c("#sym.ast.op", "#sym.dagger", "#sym.dagger.double", "#sym.section")
syms_extended <- c(syms_standard, "#sym.bar.v.double", "#sym.pilcrow")

add_indents <- function(out, x, opts) {
  if (is.null(attr(x, "indent")) || vec_cast(attr(x, "indent"), character()) == "0") out else glue::glue(
    "{indent}{out}",
    indent = glue::glue("#h(", format(attr(x, "indent")), ")"),
    .null = NULL,
    .na = opts$na
  )
}

add_textstyle <- function(out, x, opts) {
  bold <- if (!is.null(attr(x, "bold"))) glue::glue("weight: \"{bold}\"",
                                                    bold = if (attr(x, "bold")) "bold" else "regular")
  italic <- if (!is.null(attr(x, "italic"))) glue::glue("style: \"{italic}\"",
                                                        italic = if (attr(x, "italic")) "italic" else "normal")
  size <- if (!is.null(attr(x, "size"))) glue::glue("size: {size}",
                                                    size = glue::glue("{attr(x, 'size')}"))
  styles <- glue::glue(bold, italic, size, .sep = ", ", .null = NULL)
  if (length(styles) == 0) out else glue::glue("#text({styles})[{out}]",
                                               .null = NULL, .na = opts$na)
}

restore_attributes <- function(out, x, attrs = c("colspan", "rowspan", "combine", "align", "stroke")) {
  out <- list(out)
  for (a in attrs) attr(out, a) <- attr(x, a)
  out
}
