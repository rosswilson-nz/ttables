#' @export
as_docx.ttables_tbl <- function(x) {
  x <- extract_table(x)

  tblPr <- get_docx_table_properties(x$opts$widths, x$opts$gutter, x$opts$caption)
  tblGrid <- get_docx_table_grid(x$opts$widths, ncol(x$header))
  body <- print_docx_content(rbind(x$header, x$body), x$opts, x$footnotes)
  body <- format_spans_docx(body, x$opts)
  body <- print_cells_docx(body)
  footnotes <- print_footnotes_docx(x$footnotes, x$opts)
  kind <- if (x$opts$supplement) "\"suppl-table\"" else "table"

  print_table_docx(tblPr, tblGrid, body, footnotes)
}

get_docx_table_properties <- function(widths, gutter, caption) {
  widths <- as.character(widths)
  gutter <- field(gutter, "column")

  Layout <- get_docx_table_layout(widths)
  Caption <- get_docx_table_caption(caption)
  CellSpacing <- get_docx_table_spacing(gutter)
  W <- get_docx_table_width(widths)

  glue::glue(
    "<x:tblPr>{tblLayout}{tblCaption}{tblCellSpacing}{tblW}\n</w:tblPr>",
    tblLayout = glue::glue("\n  <w:tblLayout w:type=\"{Layout}\"/>", .trim = FALSE),
    tblCaption = glue::glue("\n  <w:tblCaption w:val=\"{Caption}\"/>", .trim = FALSE),
    tblCellSpacing = glue::glue("\n  <w:tblCellSpacing w:w=\"{CellSpacing}\" w:type=\"dxa\"/>", .trim = FALSE),
    tblW = glue::glue("\n  <w:tblW w:w=\"{W$w}\" w:type=\"{W$type}\"/>", .trim = FALSE),
    .null = NULL
  )
}

get_docx_table_layout <- function(widths) if (identical(widths, "auto")) "autofit" else "fixed"

get_docx_table_caption <- function(caption) caption

get_docx_table_spacing <- function(gutter) if (is.null(gutter)) NULL else to_dxa(gutter)

get_docx_table_width <- function(widths) {
  if (identical(widths, "auto") || identical(widths, "span") || rlang::is_bare_numeric(x)) {
    list(type = "pct", w = "100%")
  } else {
    list(type = "dxa", w = round(sum(vapply(as_width(widths), to_dxa, numeric(1)))))
  }
}

get_docx_table_grid <- function(widths, nc) {
  if (identical(widths, "auto")) {
    widths <- rep(0, nc)
  } else if (identical(widths, "span")) {
    widths <- round(rep(5000 / nc, nc))
  } else if (rlang::is_bare_numeric(widths)) {
    widths <- round(5000 * widths / sum(widths))
  } else {
    widths <- round(vapply(as_width(widths), to_dxa, numeric(1)))
  }
  gridContent <- glue::glue("    <w:gridCol w:w=\"{widths}\"/>", .trim = FALSE)
  glue::glue("  <w:tblGrid>\n{content}\n  </w:tblGrid>",
             content = glue::glue_collapse(gridContent, sep = "\n"),
             .trim = FALSE)
}

print_docx_content <- function(mat, opts, fns) {
  apply(mat, 1:2, \(x) format_contents_docx(x[[1]], opts, fns))
}

format_contents_docx <- function(x, opts, fns) {
  out <- glue::glue("      <w:t>\n        {text}\n      </w:t>",
                    text = x[[1]], .na = opts$na, .trim = FALSE)
  out <- add_textstyle_docx(out, x, opts)
  out <- add_footnote_refs_docx(out, x, fns, opts)
  #out <- add_indents_docx(out, x, opts)
  out <- restore_attributes(out, x, attrs = c("colspan", "rowspan", "combine", "align", "stroke", "indent"))
  out
}

add_textstyle_docx <- function(out, x, opts) {
  bold <- if (!is.null(attr(x, "bold"))) glue::glue("        <w:b w:val=\"{bold}\"/>",
                                                    bold = if (attr(x, "bold")) "true" else "false",
                                                    .trim = FALSE)
  italic <- if (!is.null(attr(x, "italic"))) glue::glue("        <w:i w:val\"{italic}\"/>",
                                                        italic = if (attr(x, "italic")) "true" else "false",
                                                        .trim = FALSE)
  size <- if (!is.null(attr(x, "size"))) glue::glue("        <w:sz w:val=\"{size}\"/>",
                                                    size = to_dxa(attr(x, 'size')) / 10,
                                                    .trim = FALSE)
  styles <- glue::glue(bold, italic, size, .sep = "\n", .null = NULL, .trim = FALSE)
  glue::glue("<w:r>\n      <w:rPr>\n{styles}\n      </w:rPr>\n    {out}\n  </w:r>")
}

add_footnote_refs_docx <- function(out, x, fns, opts) {
  fns <- get_footnotes(x, fns, opts)
  if (is.null(fns)) out else glue::glue(
    "{out}{fn}",
    fn = glue::glue("<w:r><w:rPr><w:vertAlign w:val=\"superscript\"/></w:rPr><w:t>{fns}</w:t></w:r>"),
    .null = NULL, .na = opts$na
  )
}

format_spans_docx <- function(mat, opts) {
  for (c in seq_len(ncol(mat))) for (r in seq_len(nrow(mat))) {
    # first loop to collate column spans
    cell <- mat[[r, c]]
    if (!is.null(attr(cell, "colspan")) && attr(cell, "colspan") > 1) {
      if (!is.na(attr(cell, "combine"))) {
        mat[[r, c]][[1]] <- glue::glue_collapse(unlist(mat[r, c + seq_len(attr(cell, "colspan")) - 1]),
                                                glue::glue('<w:r><w:t>{attr(cell, "combine")}</w:t></w:r>'))
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
        mat[[r, c]][[1]] <- glue::glue_collapse(unlist(mat[r + seq_len(attr(cell, "rowspan")) - 1, c]),
                                                glue::glue('<w:r><w:t>{attr(cell, "combine")}</w:t></w:r>'))
      }
      for (rr in r + seq_len(attr(cell, "rowspan") - 1)) {
        mat[[rr, c]] <- list(NULL)
        attr(mat[[rr, c]], "rowspan") <- -1
      }
    }
  }
  mat
}

print_cells_docx <- function(mat) {
  apply(mat, 1:2, \(x) print_cell_docx(x[[1]]))
}

print_cell_docx <- function(x) {
  if (is.null(x[[1]])) {
    if (is.null(attr(x, "rowspan"))) return(glue::glue())
    x[[1]] <- glue::glue("<w:p/>")
  } else {
    if (!is.null(attr(x, "align")) && !is.na(field(attr(x, "align"), "x"))) {
      x[[1]] <- glue::glue("<w:pPr>\n      <w:jc w:val=\"{align}\"/>\n    </w:pPr>\n    {x}",
                      align = docx_halign(field(attr(x, "align"), "x")),
                      .trim = FALSE)
    }
    x[[1]] <- glue::glue("  <w:p>\n    {x}\n  </w:p>", .trim = FALSE)
  }
  colspan <- if (!is.null(attr(x, "colspan"))) glue::glue("    <w:gridSpan w:val=\"{colspan}\"/>",
                                                          colspan = attr(x, "colspan"), .trim = FALSE)
  rowspan <- if (!is.null(attr(x, "rowspan"))) glue::glue(
    "    <w:vMerge w:val=\"{rowspan}\"/>",
    rowspan = if (attr(x, "rowspan") > 0) "restart" else "continue",
    .trim = FALSE
  )
  align <- if (!is.null(attr(x, "align")) && !is.na(field(attr(x, "align"), "y")))
    glue::glue("    <w:vAlign w:val=\"{align}\"/>", align = docx_valign(field(attr(x, "align")), "y"),
               .trim = FALSE)
  stroke <- if (!is.null(attr(x, "stroke"))) glue::glue(
    "    <w:tcBorders>\n{stroke}\n    </w:tcBorders>",
    stroke = format_docx_stroke(attr(x, "stroke")),
    .trim = FALSE
  )
  indent <- if (!is.null(attr(x, "indent"))) glue::glue("    <w:left w:w=\"{indent}\" w:type=\"dxa\"/>",
                                                        indent = to_dxa(attr(x, "indent")),
                                                        .trim = FALSE)
  cellstyles <- glue::glue(colspan, rowspan, align, stroke, indent, .sep = "\n", .null = NULL, .trim = FALSE)
  if (length(cellstyles)) x[[1]] <- glue::glue("  <tcPr>\n{cellstyles}\n  </w:tcPr>\n  {x}",
                                               .null = NULL, .trim = FALSE)

  glue::glue("<w:tc>\n{x[[1]]}\n</w:tc>")
}

format_docx_stroke <- function(stroke, expand = TRUE) {
  UseMethod("format_docx_stroke", stroke)
}
#' @export
format_docx_stroke.ttables_stroke <- function(stroke, expand = TRUE) {
  if (expand) {
    format_docx_stroke(dictionary(list(top = stroke, bottom = stroke, left = stroke, right = stroke)))
  } else {
    glue::glue("w:val=\"{style}\" w:sz=\"{sz}\" w:space=\"{space}\" w:color=\"{color}\"",
               style = "single",
               sz = if (is.na(field(stroke, "thickness"))) 8 else as_pt(field(stroke, "thickness")) * 8,
               space = "0",
               color = if (is.na(field(stroke, "paint"))) "000000" else as_hex_colour(field(stroke, "paint")))
  }
}
#' @export
format_docx_stroke.ttables_dictionary <- function(stroke, expand) {
  stroke <- to_list(stroke)
  stroke <- vapply(
    seq_along(stroke),
    \(i) glue::glue("      <w:{nm} {fmt}>",
                    nm = to_border_name(names(stroke)[[i]]),
                    fmt = format_docx_stroke(stroke[[i]][[1]], FALSE),
                    .trim = FALSE),
    character(1)
  )
  glue::glue_collapse(stroke, sep = "\n")
}
docx_valign <- function(x) switch(x, horizon = "center", x)
docx_halign <- function(x) switch(x, left = "start", right = "end", x)

as_hex_colour <- function(paint) {
  switch(paint,
         black = "000000",
         gray = "AAAAAA",
         silver = "DDDDDD",
         white = "FFFFFF",
         navy = "001F3F",
         blue = "0074D9",
         aqua = "7FDBFF",
         teal = "39CCCC",
         eastern = "239DAD",
         purple = "B10DC9",
         fuchsia = "F012BE",
         maroon = "85144B",
         red = "FF4136",
         orange = "FF851B",
         yellow = "FFDC00",
         olive = "3D9970",
         green = "2ECC40",
         lime = "01FF70")
}

to_border_name <- function(x) switch(x, left = "start", right = "end", x)

print_footnotes_docx <- function(df, opts) {
  df <- df[order(factor(df$type, opts$footnotes.order), df$ref), ]
  ref <- dplyr::case_match(df$type,
                           "number" ~ get_fn_num(df$ref, opts$footnotes.number),
                           "alphabet" ~ get_fn_alph(df$ref, opts$footnotes.alphabet),
                           "symbol" ~ get_fn_sym_docx(df$ref, opts$footnotes.symbol))

  glue::glue("{fn_ref}{content}",
             content = glue::glue("<w:r><w:t>{df$content}</w:t><:w/r>"),
             fn_ref = glue::glue("<w:r><w:rPr><w:vertAlign w:val=\"superscript\"/></w:rPr><w:t>{ref}</w:t></w:r><w:r><w:t xml:space=\"preserve\"> </w:t><:w/r>", .na = NULL),
             .na = "")
}

get_fn_sym_docx <- function(num, style) {
  out <- character(length(num))
  out[num != 0] <- switch(style, standard = syms_standard_docx[num], extended = syms_extended_docx[num])
  out
}
syms_standard_docx <- c("*", "†", "‡", "§")
syms_extended_docx <- c(syms_standard_docx, "‖", "¶")

print_table_docx <- function(tblPr, tblGrid, body, footnotes) {
  glue::glue(
    "
<w:tbl>
    {tblPr}
    {tblGrid}
    {body}
</w:tbl>
<w:p>
    {footnotes}
</w:p>
    ",
    body = glue::glue_collapse(apply(body, 1, \(x) print_row_docx(x)), sep = ",\n    "),
    footnotes = glue::glue_collapse(footnotes, sep = "\n<w:br/>\n"),
    .null = NULL,
    .trim = FALSE
  )
}

print_row_docx <- function(row) {
  drop <- vapply(row, \(x) length(x) == 0, logical(1))
  glue::glue("<w:tr>\n{content}\n</w:tr>",
             content = glue::glue_collapse(row[!drop], sep = "\n"),
             .trim = FALSE)
}
