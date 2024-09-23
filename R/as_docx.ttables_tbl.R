#' @export
as_docx.ttables_tbl <- function(x) {
  x <- extract_table(x)

  kind <- if (x$opts$supplement) "\"suppl-table\"" else "table"

  caption <- print_docx_caption(x$opts$caption)
  tblPr <- get_docx_table_properties(x$opts$widths, x$opts$gutter, x$opts$caption)
  tblGrid <- get_docx_table_grid(x$opts$widths, ncol(x$header))

  header <- print_docx_content(x$header, x$opts, x$footnotes)
  body <- print_docx_content(x$body, x$opts, x$footnotes)
  footnotes <- print_footnotes_docx(x$footnotes, x$opts)

  header <- format_spans_docx(header, x$opts)
  body <- format_spans_docx(body, x$opts)

  header <- print_cells_docx(header, header = TRUE)
  body <- print_cells_docx(body)

  print_table_docx(caption, tblPr, tblGrid, header, body, footnotes, x$opts)
}

print_docx_caption <- function(caption) {
  if (!is.null(caption)) glue::glue(
    "
<w:p>
  <w:pPr><w:pStyle w:val=\"TableCaption\"/></w:pPr>
  <w:r><w:t>{caption}</w:t></w:r>
</w:p>
    "
  ) else NULL
}

get_docx_table_properties <- function(widths, gutter, caption) {
  widths <- as.character(widths)
  gutter <- field(gutter, "column")

  Layout <- get_docx_table_layout(widths)
  Caption <- get_docx_table_caption(caption)
  CellSpacing <- get_docx_table_spacing(gutter)
  W <- get_docx_table_width(widths)

  Borders <- glue::glue("\n    <w:top {border} />\n    <w:bottom {border} />",
                        border = "w:val=\"single\" w:sz=\"8\" w:space=\"0\" w:color=\"000000\"",
                        .trim = FALSE)

  glue::glue(
    "<w:tblPr>{tblBorders}{tblLayout}{tblCaption}{tblCellSpacing}{tblW}\n</w:tblPr>",
    tblBorders = glue::glue("\n  <w:tblBorders>{Borders}\n  </w:tblBorders>", .trim = FALSE),
    tblLayout = glue::glue("\n  <w:tblLayout w:type=\"{Layout}\"/>", .trim = FALSE),
    tblCaption = if (!is.null(Caption)) glue::glue("\n  <w:tblCaption w:val=\"{Caption}\"/>", .trim = FALSE),
    tblCellSpacing = glue::glue("\n  <w:tblCellSpacing w:w=\"{CellSpacing}\" w:type=\"dxa\"/>", .trim = FALSE),
    tblW = glue::glue("\n  <w:tblW w:w=\"{W$w}\" w:type=\"{W$type}\"/>", .trim = FALSE),
    .null = NULL
  )
}

get_docx_table_layout <- function(widths) if (identical(widths, "auto")) "autofit" else "fixed"

get_docx_table_caption <- function(caption) caption

get_docx_table_spacing <- function(gutter) if (is.null(gutter) || is_auto(gutter)) 0 else to_dxa(gutter)

get_docx_table_width <- function(widths) {
  w <- if (any(widths == "auto") || any(substring(widths, nchar(widths) - 1) == "fr")) {
    9070
  } else {
    sum(round(vapply(as_width(widths), to_dxa, numeric(1))))
  }
  list(type = "dxa", w = w)
}

get_docx_table_grid <- function(widths, nc) {
  au <- vapply(widths, is_auto, logical(1))
  widths[au] <- as_width("0pt")

  fr <- vapply(widths, is_fractional_length, logical(1))
  if (any(fr)) {
    excess <- 16 - sum(vapply(widths[!fr], as_cm, numeric(1)))
    frwidths <- vapply(widths[fr], as.numeric, numeric(1))
    frwidths <- abs_length(frwidths / sum(frwidths) * excess, "cm")
    widths[fr] <- as_width(frwidths)
  }
  widths <- round(vapply(widths, to_dxa, numeric(1)))
  gridContent <- glue::glue("    <w:gridCol w:w=\"{widths}\"/>", .trim = FALSE)
  glue::glue("  <w:tblGrid>\n{content}\n  </w:tblGrid>",
             content = glue::glue_collapse(gridContent, sep = "\n"),
             .trim = FALSE)
}

print_docx_content <- function(mat, opts, fns) {
  apply(mat, 1:2, \(x) format_contents_docx(x[[1]], opts, fns))
}

format_contents_docx <- function(x, opts, fns) {
  text <- escape_html(x[[1]])

  out <- glue::glue("      <w:t>{text}</w:t>",
                    .na = opts$na, .trim = FALSE)
  out <- add_textstyle_docx(out, x, opts)
  out <- add_footnote_refs_docx(out, x, fns, opts)
  #out <- add_indents_docx(out, x, opts)
  out <- restore_attributes(out, x, attrs = c("colspan", "rowspan", "combine", "align", "stroke", "indent"))
  out
}

escape_html <- function(x) {
  pattern <- "[<>&'\"]"
  text <- enc2utf8(as.character(x))
  if (!any(grepl(pattern, text, useBytes = TRUE))) return(text)
  specials <- list("&" = "&amp;",
                   "<" = "&lt;",
                   ">" = "&gt;",
                   "'" = "&#039;",
                   "\"" = "&quot;")
  for (chr in names(specials)) {
    text <- gsub(chr, specials[[chr]], text, fixed = TRUE,
                 useBytes = TRUE)
  }
  Encoding(text) <- "UTF-8"
  return(text)
}

add_textstyle_docx <- function(out, x, opts) {
  bold <- if (!is.null(attr(x, "bold"))) glue::glue("            <w:b w:val=\"{bold}\"/>",
                                                    bold = if (attr(x, "bold")) "true" else "false",
                                                    .trim = FALSE)
  italic <- if (!is.null(attr(x, "italic"))) glue::glue(
    "            <w:i w:val=\"{italic}\"/>",
    italic = if (attr(x, "italic")) "true" else "false",
    .trim = FALSE
  )
  size <- if (!is.null(attr(x, "size"))) glue::glue("            <w:sz w:val=\"{size}\"/>",
                                                    size = to_dxa(attr(x, 'size')) / 10,
                                                    .trim = FALSE)
  styles <- glue::glue(bold, italic, size, .sep = "\n", .null = NULL, .trim = FALSE)
  styles <- if (length(styles)) glue::glue("\n          <w:rPr>", styles, "          </w:rPr>",
                                           .sep = "\n", .trim = FALSE)
  glue::glue("    <w:r>{styles}\n    {out}\n        </w:r>", .trim = FALSE, .null = NULL)
}

add_footnote_refs_docx <- function(out, x, fns, opts) {
  fns <- get_footnotes_docx(x, fns, opts)
  if (is.null(fns)) out else glue::glue(
    "{out}{fn}",
    fn = glue::glue("<w:r><w:rPr><w:vertAlign w:val=\"superscript\"/></w:rPr><w:t>{fns}</w:t></w:r>"),
    .null = NULL, .na = opts$na
  )
}

get_footnotes_docx <- function(x, fns, opts) {
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
    glue::glue_collapse(get_fn_sym_docx(idxs[attr(x, "footnote_sym")], opts$footnotes.symbol), sep = ",")
  }
  fn <- glue::glue(fn_num, fn_alph, fn_sym, .sep = ",", .null = NULL)
  if (length(fn)) fn else NULL
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
        cs <- attr(mat[[rr, c]], "colspan")
        mat[[rr, c]] <- list(NULL)
        attr(mat[[rr, c]], "rowspan") <- -1
        attr(mat[[rr, c]], "colspan") <- cs # copy original colspan spec for cell sizing
      }
    }
  }
  mat
}

print_cells_docx <- function(mat, header = FALSE) {
  if (header) {
    out <- matrix(list(), nrow(mat), ncol(mat))
    for (r in seq_len(nrow(mat))) for (c in seq_len(ncol(mat))) {
      out[[r, c]] <- print_cell_docx(mat[[r, c]], r == nrow(mat))
    }
    out
  } else {
    apply(mat, 1:2, \(x) print_cell_docx(x[[1]]))
  }
}

print_cell_docx <- function(x, last_header = FALSE) {
  if (is.null(x[[1]])) {
    if (is.null(attr(x, "rowspan"))) return(glue::glue())
    x[[1]] <- glue::glue("    <w:p><w:pPr><w:pStyle w:val=\"Compact\" /></w:pPr></w:p>")
  } else {
    pPr <- glue::glue("\n      <w:pStyle w:val=\"Compact\" />", .trim = FALSE)
    if (!is.null(attr(x, "align")) && !is.na(field(attr(x, "align"), "x"))) {
      pPr <- glue::glue("{pPr}\n      <w:jc w:val=\"{align}\"/>",
                        align = docx_halign(field(attr(x, "align"), "x")),
                        .trim = FALSE)
    }
    x[[1]] <- glue::glue("    <w:p>\n        <w:pPr>{pPr}\n    </w:pPr>\n    {x}\n      </w:p>",
                         .trim = FALSE)
  }
  colspan <- if (!is.null(attr(x, "colspan"))) glue::glue("        <w:gridSpan w:val=\"{colspan}\"/>",
                                                          colspan = attr(x, "colspan"), .trim = FALSE)
  rowspan <- if (!is.null(attr(x, "rowspan"))) glue::glue(
    "        <w:vMerge w:val=\"{rowspan}\"/>",
    rowspan = if (attr(x, "rowspan") > 0) "restart" else "continue",
    .trim = FALSE
  )
  align <- if (!is.null(attr(x, "align")) && !is.na(field(attr(x, "align"), "y")))
    glue::glue("        <w:vAlign w:val=\"{align}\"/>", align = docx_valign(field(attr(x, "align"), "y")),
               .trim = FALSE)
  stroke <- if (!is.null(attr(x, "stroke"))) glue::glue(
    "        <w:tcBorders>\n{stroke}\n    </w:tcBorders>",
    stroke = format_docx_stroke(attr(x, "stroke")),
    .trim = FALSE
  ) else if (last_header) {
    glue::glue(
      "        <w:tcBorders>\n{stroke}\n    </w:tcBorders>",
      stroke = format_docx_stroke(dictionary(list(bottom = stroke("black", "1pt")))),
      .trim = FALSE
    )
  }
  indent <- if (!is.null(attr(x, "indent"))) glue::glue("        <w:left w:w=\"{indent}\" w:type=\"dxa\"/>",
                                                        indent = to_dxa(attr(x, "indent")),
                                                        .trim = FALSE)
  cellstyles <- glue::glue(colspan, rowspan, align, stroke, indent, .sep = "\n", .null = NULL, .trim = FALSE)
  if (length(cellstyles)) x[[1]] <- glue::glue("      <w:tcPr>\n{cellstyles}\n      </w:tcPr>\n  {x}",
                                               .null = NULL, .trim = FALSE)

  glue::glue("    <w:tc>\n{x[[1]]}\n    </w:tc>", .trim = FALSE)
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
    \(i) glue::glue("      <w:{nm} {fmt} />",
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

  fn_ref <- glue::glue("<w:t>{ref}</w:t>", .na = NULL)
  fn_ref_style <- glue::glue("<w:vertAlign w:val=\"superscript\"/>")
  if (!is.null(opts$fontsize) && !is.na(opts$fontsize)) {
    fn_ref_style <- glue::glue("<w:sz w:val=\"{size}\"/>{fn_ref_style}",
                               size = to_dxa(opts$fontsize) / 10)
    fn_space_style <- glue::glue("<w:rPr><w:sz w:val=\"{size}\"/></w:rPr>",
                                 size = to_dxa(opts$fontsize) / 10)
  } else fn_space_style <- NULL
  fn_space <- glue::glue("<w:r>{fn_space_style}<w:t xml:space=\"preserve\"> </w:t></w:r>",
                         .null = NULL)
  fn_ref <- glue::glue("<w:r><w:rPr>{fn_ref_style}></w:rPr>{fn_ref}</w:r>{fn_space}", .na = NULL)

  content <- glue::glue("<w:t>{df$content}</w:t>")
  if (!is.null(opts$fontsize) && !is.na(opts$fontsize)) {
    content <- glue::glue("<w:rPr><w:sz w:val=\"{size}\"/></w:rPr>{content}",
                          size = to_dxa(opts$fontsize) / 10)
  }
  content <- glue::glue("<w:r>{content}</w:r>")

  glue::glue("<w:p>\n  <w:pPr><w:pStyle w:val=\"BlockText\" /></w:pPr>{fn_ref}{content}\n</w:p>", .na = "")
}

get_fn_sym_docx <- function(num, style) {
  out <- character(length(num))
  out[num != 0] <- switch(style, standard = syms_standard_docx[num], extended = syms_extended_docx[num])
  out
}
syms_standard_docx <- c("*", "†", "‡", "§")
syms_extended_docx <- c(syms_standard_docx, "‖", "¶")

print_table_docx <- function(caption, tblPr, tblGrid, header, body, footnotes, opts) {
  landscape_head <- "  <w:p><w:pPr><w:sectPr/></w:pPr></w:p>\n"
  landscape_tail <- "
  <w:p><w:pPr><w:sectPr>
    <w:pgSz w:w=\"16838\" w:h=\"11906\" w:orient=\"landscape\"/>
  </w:sectPr></w:pPr></w:p>
"

  glue::glue(
    "
{landscape_head}
{caption}
<w:tbl>
  {tblPr}
  {tblGrid}
  {header}
  {body}
</w:tbl>
{footnotes}{landscape_tail}
",
    header = glue::glue_collapse(apply(header, 1, \(x) print_header_row_docx(x)), sep = ",\n    "),
    body = glue::glue_collapse(apply(body, 1, \(x) print_row_docx(x)), sep = ",\n    "),
    footnotes = glue::glue_collapse(footnotes, sep = "\n  "),
    landscape_head = if (opts$landscape) landscape_head,
    landscape_tail = if (opts$landscape) landscape_tail,
    .null = NULL, .trim = FALSE
  )
}

print_row_docx <- function(row) {
  drop <- vapply(row, \(x) length(x) == 0, logical(1))
  glue::glue("  <w:tr>\n{content}\n  </w:tr>",
             content = glue::glue_collapse(row[!drop], sep = "\n    "),
             .trim = FALSE)
}

print_header_row_docx <- function(row) {
  drop <- vapply(row, \(x) length(x) == 0, logical(1))

  glue::glue("  <w:tr>\n    <w:trPr><w:tblHeader w:val=\"on\" /></w:trPr>\n{content}\n  </w:tr>",
             content = glue::glue_collapse(row[!drop], sep = "\n    "),
             .null = NULL, .trim = FALSE)
}
