extract_table <- function(x) {
  if (!inherits(x, "typst_table")) stop("'x' must be a `typst_table` object")

  nc <- ncol(x$`_body`)
  nr <- nrow(x$`_body`)

  formats <- x$`_format`
  layout <- x$`_layout`
  footnotes <- x$`_footnotes`

  header <- extract_contents(x$`_header`, formats, layout, footnotes, "header")
  body <- extract_contents(x$`_body`, formats, layout, footnotes, "body")
  added_rows <- extract_contents(x$`_added_rows`, formats, layout, footnotes, "added_rows")

  body <- combine_table_sections(body, added_rows)
  footnotes <- extract_footnotes(footnotes, header, body, direction = x$`_opts`$table.footnotes.direction)
  list(header = header, body = body, footnotes = footnotes, opts = x$`_opts`)
}

extract_contents <- function(df, formats, layout, footnotes, location) {
  mat <- as.matrix(df[colnames(df) != "_insert_before"])
  out <- apply(mat, 1:2, \(x) as.list(as.character(x)))
  if (nrow(out) == 0) return(out)
  rows <- seq_len(nrow(out))
  columns <- seq_len(ncol(out))
  f_table <- dplyr::filter(formats, location == "table")
  for (r in seq_len(nrow(f_table))) {
    f <- f_table[r, ]
    if (!is.na(f$bold)) for (row in rows) for (cell in columns) attr(out[[row, cell]], "bold") <- f$bold
    if (!is.na(f$italic)) for (row in rows) for (cell in columns) attr(out[[row, cell]], "italic") <- f$italic
    if (!is.na(f$align)) for (row in rows) for (cell in columns) attr(out[[row, cell]], "align") <- f$align
    if (!is.na(f$indent)) for (row in rows) for (cell in columns) attr(out[[row, cell]], "indent") <- f$indent
    if (!is.na(f$size)) for (row in rows) for (cell in columns) attr(out[[row, cell]], "size") <- f$size
  }
  f_header <- formats[formats$location == location, ]
  for (r in seq_len(nrow(f_header))) {
    f <- f_header[r, ]
    if (!is.na(f$bold)) attr(out[[f$row, f$column]], "bold") <- f$bold
    if (!is.na(f$italic)) attr(out[[f$row, f$column]], "italic") <- f$italic
    if (!is.na(f$align)) attr(out[[f$row, f$column]], "align") <- f$align
    if (!is.na(f$indent)) attr(out[[f$row, f$column]], "indent") <- f$indent
    if (!is.na(f$size)) attr(out[[f$row, f$column]], "size") <- f$size
  }
  l <-layout[layout$location == location, ]
  for (r in seq_len(nrow(l))) {
    ll <- l[r, ]
    attr(out[[l$row, l$column]], "rowspan") <- l$size[[1]][[1]]
    attr(out[[l$row, l$column]], "colspan") <- l$size[[1]][[2]]
    attr(out[[l$row, l$column]], "combine") <- ll$combine
  }
  for (r in seq_len(nrow(footnotes))) {
    fn <- footnotes[r, ]
    if (fn$location[[1]][["location"]] == location) {
      loc <- expand_location(fn$location[[1]])
      for (l in seq_len(nrow(loc))) {
        if (fn$type == "number") attr(out[[loc$row[[l]], loc$column[[l]]]], "footnote_num") <- c(attr(out[[loc$row[[l]], loc$column[[l]]]], "footnote_num"), r)
        if (fn$type == "alphabet") attr(out[[loc$row[[l]], loc$column[[l]]]], "footnote_alph") <- c(attr(out[[loc$row[[l]], loc$column[[l]]]], "footnote_alph"), r)
        if (fn$type == "symbol") attr(out[[loc$row[[l]], loc$column[[l]]]], "footnote_sym") <- c(attr(out[[loc$row[[l]], loc$column[[l]]]], "footnote_sym"), r)
      }
    }
  }
  if (location == "added_rows") attr(out, "at") <- df$`_insert_before`
  out
}

combine_table_sections <- function(body, added_rows) {
  if (nrow(added_rows) == 0) return(body)
  at <- attr(added_rows, "at")
  added_rows <- added_rows[order(-at), ]
  at <- sort(at, decreasing = TRUE)
  idxs <- seq_len(nrow(body))
  for (a in seq_along(at)) {
    idxs <- append(idxs, nrow(body) + a, after = at[a])
  }

  rbind(body, added_rows)[idxs, ]
}

extract_footnotes <- function(footnotes, header, body, direction) {
  table <- rbind(header, body)
  if (direction == "horizontal") table <- t(table)
  footnote_num <- integer()
  footnote_alph <- integer()
  footnote_sym <- integer()
  for (cell in table) {
    footnote_num <- append(footnote_num, attr(cell, "footnote_num"))
    footnote_alph <- append(footnote_alph, attr(cell, "footnote_alph"))
    footnote_sym <- append(footnote_sym, attr(cell, "footnote_sym"))
  }
  footnotes$ref <- integer(nrow(footnotes))
  footnotes$ref[footnotes$type == "number"] <- unique(footnote_num)
  footnotes$ref[footnotes$type == "alphabet"] <- unique(footnote_alph)
  footnotes$ref[footnotes$type == "symbol"] <- unique(footnote_sym)
  footnotes
}
