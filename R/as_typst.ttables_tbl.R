#' @export
as_typst.ttables_tbl <- function(x) {
  x <- extract_table(x)

  widths <- print_widths(x$opts$widths, ncol(x$header))
  align <- print_align(x$opts$align, ncol(x$header))
  gutter <- print_gutter(x$opts$gutter)

  header <- print_typst_content(x$header, x$opts, x$footnotes)
  body <- print_typst_content(x$body, x$opts, x$footnotes)
  footnotes <- print_footnotes(x$footnotes, x$opts, ncol(x$header))

  header <- format_spans(header, x$opts)
  body <- format_spans(body, x$opts)

  header <- print_cells(header)
  body <- print_cells(body)

  print_table(widths, align, gutter, header, body, footnotes)
}

print_widths <- function(widths, nc) {
  widths <- as.character(widths)
  if (identical(widths, "auto")) {
    widths <- nc
  } else if (identical(widths, "span")) {
    widths <- rep("1fr", nc)
  }
  sprintf("(%s)", paste(widths, collapse = ","))
}

print_align <- function(align, nc) {
  if (length(align) == 1) {
    align <- rep(align, nc)
  }
  sprintf("(%s)", paste(align, collapse = ","))
}

print_gutter <- function(gutter) {
  if (is.null(gutter)) {
    return(NULL)
  }
  g <- unclass(gutter)
  column <- if (!(is.null(g$column) | is.na(g$column))) {
    sprintf("column-gutter: %s", format(g$column))
  }
  row <- if (!(is.null(g$row) | is.na(g$row))) {
    sprintf("row-gutter: %s", format(g$row))
  }
  sep <- if (!is.null(column) && !is.null(row)) ", "
  sprintf("%s%s%s", column %||% "", sep %||% "", row %||% "")
}

print_typst_content <- function(mat, opts, fns) {
  apply(mat, 1:2, \(x) format_contents(x[[1]], opts, fns))
}

print_footnotes <- function(df, opts, nc) {
  if (nrow(df)) {
    df <- df[order(factor(df$type, opts$footnotes.order), df$ref), ]
    ref <- character(length(df$type))
    ref[df$type == "number"] <- get_fn_num(df$ref, opts$footnotes.number)[
      df$type == "number"
    ]
    ref[df$type == "alphabet"] <- get_fn_alph(df$ref, opts$footnotes.alphabet)[
      df$type == "alphabet"
    ]
    ref[df$type == "symbol"] <- get_fn_sym(df$ref, opts$footnotes.symbol)[
      df$type == "symbol"
    ]

    fn_ref <- sprintf("#super[%s]", ref)
    fn_ref[is.na(ref) | ref == ""] <- ""

    content <- df$content
    content[is.na(content)] <- ""

    footnote <- paste(sprintf("%s%s", fn_ref, content), collapse = "\n\n    ")
    sprintf(
      "table.cell(colspan: %s, text(size: %s)[\n    %s\n])",
      nc,
      opts$footnotes.size,
      footnote
    )
  }
}

format_spans <- function(mat, opts) {
  for (c in seq_len(ncol(mat))) {
    for (r in seq_len(nrow(mat))) {
      # first loop to collate column spans
      cell <- mat[[r, c]]
      if (!is.null(attr(cell, "colspan")) && attr(cell, "colspan") > 1) {
        if (!is.na(attr(cell, "combine"))) {
          mat[[r, c]][[1]] <- paste(
            unlist(mat[r, c + seq_len(attr(cell, "colspan")) - 1]),
            collapse = attr(cell, "combine")
          )
        }
        for (cc in c + seq_len(attr(cell, "colspan") - 1)) {
          mat[[r, cc]] <- list(NULL)
        }
        if (!is.null(attr(cell, "rowspan")) && attr(cell, "rowspan") > 1) {
          for (rr in r + seq_len(attr(cell, "rowspan") - 1)) {
            attr(mat[[rr, c]], "colspan") <- attr(cell, "colspan")
            attr(mat[[rr, c]], "combine") <- attr(cell, "combine")
          }
        }
      }
    }
  }
  for (c in seq_len(ncol(mat))) {
    for (r in seq_len(nrow(mat))) {
      # second loop to collate row spans
      cell <- mat[[r, c]]
      if (!is.null(attr(cell, "rowspan")) && attr(cell, "rowspan") > 1) {
        if (!is.na(attr(cell, "combine"))) {
          mat[[r, c]][[1]] <- paste(
            unlist(mat[r + seq_len(attr(cell, "rowspan")) - 1, c]),
            collapse = attr(cell, "combine")
          )
        }
        for (rr in r + seq_len(attr(cell, "rowspan") - 1)) {
          mat[[rr, c]] <- list(NULL)
        }
      }
    }
  }
  mat
}

format_contents <- function(x, opts, fns) {
  out <- x[[1]]
  if (is.na(out)) {
    out <- opts$na
  }

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
  if (is.null(x[[1]])) {
    return(character())
  }
  colspan <- if (!is.null(attr(x, "colspan"))) {
    sprintf("colspan: %s", attr(x, "colspan"))
  }
  rowspan <- if (!is.null(attr(x, "rowspan"))) {
    sprintf("rowspan: %s", attr(x, "rowspan"))
  }
  align <- if (!is.null(attr(x, "align"))) {
    sprintf("align: %s", attr(x, "align"))
  }
  stroke <- if (!is.null(attr(x, "stroke"))) {
    sprintf("stroke: %s", format(attr(x, "stroke")))
  }
  fill <- if (!is.null(attr(x, "fill"))) {
    sprintf("fill: %s", format(attr(x, "fill")))
  }
  attrs <- list(colspan, rowspan, align, stroke, fill)
  attrs <- attrs[sapply(attrs, \(x) !is.null(x))]
  cellstyles <- do.call(paste, append(attrs, list(sep = ", ")))
  if (length(cellstyles)) {
    x <- sprintf("#table.cell(%s)[%s]", cellstyles, x)
  }

  sprintf("[%s]", x)
}

print_table <- function(widths, align, gutter, header, body, footnotes) {
  columns <- sprintf("columns: %s", widths)
  align <- sprintf("align: %s", align)
  gutter <- if (!is.null(gutter)) sprintf(",\n    %s", gutter) else ""
  nh <- nrow(header)
  header <- paste(apply(header, 1, print_row), collapse = ",\n    ")
  header <- sprintf(
    "table.hline(),\n    table.header(%s),\n    table.hline(y: %s, position: bottom)",
    header,
    nh - 1
  )
  body <- paste(apply(body, 1, print_row), collapse = ",\n    ")
  footer <- if (!is.null(footnotes)) {
    sprintf(
      "table.hline(),\n    table.footer(repeat: false, %s)",
      footnotes
    )
  } else {
    "table.hline"
  }
  sprintf(
    "#table
    %s,
    %s%s,
    %s,
    %s,
    %s
)",
    columns,
    align,
    gutter,
    header,
    body,
    footer
  )
}

print_row <- function(row) {
  drop <- vapply(row, \(x) length(x) == 0, logical(1))
  paste(row[!drop], collapse = ", ")
}

add_footnote_refs <- function(out, x, fns, opts) {
  fns <- get_footnotes(x, fns, opts)
  if (is.null(fns)) {
    out
  } else {
    fn_ref <- sprintf("#super[%s]", fns)
    fn_ref[is.na(fns) | fns == ""] <- ""
    sprintf("%s%s", out, fn_ref)
  }
}

get_footnotes <- function(x, fns, opts) {
  fn_num <- if (!is.null(attr(x, "footnote_num"))) {
    idxs <- fns[fns$type == "number", ]$ref
    paste(
      get_fn_num(idxs[attr(x, "footnote_num")], opts$footnotes.number),
      collapse = ","
    )
  }
  fn_alph <- if (!is.null(attr(x, "footnote_alph"))) {
    idxs <- fns[fns$type == "alphabet", ]$ref
    paste(
      get_fn_alph(idxs[attr(x, "footnote_alph")], opts$footnotes.alphabet),
      collapse = ","
    )
  }
  fn_sym <- if (!is.null(attr(x, "footnote_sym"))) {
    idxs <- fns[fns$type == "symbol", ]$ref
    paste(
      get_fn_sym(idxs[attr(x, "footnote_sym")], opts$footnotes.symbol),
      collapse = ","
    )
  }
  sep1 <- if (!is.null(fn_num) && !is.null(fn_alph)) ","
  sep2 <- if (!is.null(fn_alph) && !is.null(fn_sym)) ","
  fn <- sprintf(
    "%s%s%s%s%s",
    fn_num %||% "",
    sep1 %||% "",
    fn_alph %||% "",
    sep2 %||% "",
    fn_sym %||% ""
  )
  if (fn == "") NULL else fn
}

get_fn_num <- function(num, style) {
  switch(style,
    arabic = as.character(num),
    roman = tolower(utils::as.roman(num)),
    Roman = utils::as.roman(num)
  )
}

get_fn_alph <- function(num, style) {
  out <- character(length(num))
  out[num != 0] <- switch(style,
    lower = letters[num],
    upper = LETTERS[num]
  )
  out
}

get_fn_sym <- function(num, style) {
  out <- character(length(num))
  out[num != 0] <- switch(style,
    standard = syms_standard[num],
    extended = syms_extended[num]
  )
  out
}

syms_standard <- c(
  "#sym.star.filled",
  "#sym.dagger",
  "#sym.dagger.double",
  "#sym.section"
)
syms_extended <- c(syms_standard, "#sym.bar.v.double", "#sym.pilcrow")

add_indents <- function(out, x, opts) {
  if (
    is.null(attr(x, "indent")) ||
      vec_cast(attr(x, "indent"), character()) == "0"
  ) {
    out
  } else {
    sprintf("#h(%s)%s", format(attr(x, "indent")), out)
  }
}

add_textstyle <- function(out, x, opts) {
  bold <- if (!is.null(attr(x, "bold"))) {
    sprintf("weight: \"%s\"", if (attr(x, "bold")) "bold" else "regular")
  }
  italic <- if (!is.null(attr(x, "italic"))) {
    sprintf("style: \"%s\"", if (attr(x, "italic")) "italic" else "normal")
  }
  size <- if (!is.null(attr(x, "size"))) sprintf("size: %s", attr(x, "size"))
  sep1 <- if (!is.null(bold) && !is.null(italic)) ", " else ""
  sep2 <- if (!is.null(italic) && !is.null(size)) ", " else ""
  styles <- sprintf(
    "%s%s%s%s%s",
    bold %||% "",
    sep1,
    italic %||% "",
    sep2,
    size %||% ""
  )
  if (length(styles) == 0 || styles == "") {
    out
  } else {
    sprintf("#text(%s)[%s]", styles, out)
  }
}

restore_attributes <- function(
  out,
  x,
  attrs = c("colspan", "rowspan", "combine", "align", "stroke", "fill")
) {
  out <- list(out)
  for (a in attrs) {
    attr(out, a) <- attr(x, a)
  }
  out
}
