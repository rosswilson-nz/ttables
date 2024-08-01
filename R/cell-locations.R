cells <- function(columns = everything(), rows = everything(), location = "body") {
  columns_expr <- rlang::enquo(columns)
  rows_expr <- rlang::enquo(rows)
  structure(
    list(columns = columns_expr, rows = rows_expr),
    class = c(switch(location, header = "header_cells", body = "body_cells", added_rows = "added_row_cells"), "cells_location")
  )
}

resolve_location <- function(location, x) {
  if (inherits(location, "header_cells")) {
    .data <- x$`_header`
    .location <- "header"
  } else if (inherits(location, "body_cells")) {
    .data <- x$`_body`
    .location <- "body"
  } else if (inherits(location, "added_row_cells")) {
    .data <- dplyr::select(x$`_added_rows`, -`_insert_after`)
    .location <- "added_rows"
  } else stop("Invalid location")
  cols <- tidyselect::eval_select(location$columns, data = .data)
  rows <- tidyselect::with_vars(rownames(.data), rlang::eval_tidy(location$rows, data = .data))
  list(list(columns = cols, rows = rows, location = .location))
}

