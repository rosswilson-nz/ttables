#' Specify cells in a Typst table
#'
#' @param columns Columns to select. Can use **`tidyverse`** selection helpers.
#' @param rows Rows to select. Can refer to table columns by name for
#'     conditional selection.
#' @param location Which part of the table to look in for the specified cells.
#'     Should be one of `"header"` (table header cells), `"body"` (main table
#'     body cells), `"added_rows"` (any new rows added to the table via e.g.
#'     [pack_rows()]).
#'
#' @returns An object inheriting from class `cells_location` specifying the
#'     table cells.
#' @export
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
    i <- which(colnames(x$`_added_rows`) == "_insert_before")
    .data <- x$`_added_rows`[, -i]
    .location <- "added_rows"
  } else if (identical(location, "table")) {
    return(list(columns = NA_integer_, rows = NA_integer_, location = "table"))
  } else stop("Invalid location")
  cols <- tidyselect::eval_select(location$columns, data = .data)
  rows <- tidyselect::with_vars(rownames(.data), rlang::eval_tidy(location$rows, data = .data))
  list(columns = cols, rows = rows, location = .location)
}

expand_location <- function(location) {
  columns <- if (is.logical(location$columns)) which(location$columns) else as.integer(location$columns)
  rows <- if (is.logical(location$rows)) which(location$rows) else as.integer(location$rows)
  tibble::tibble(tidyr::expand_grid(column = columns, row = rows), location = location$location)
}
