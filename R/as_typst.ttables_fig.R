#' @export
as_typst.ttables_fig <- function(x, ...) {
  opts <- x$`_opts`
  footnotes <- x$`_footnotes`
  image <- print_image(x$`_image`, opts)
  kind <- if (opts$supplement) "\"suppl-image\"" else "image"

  inner <- glue::glue(
    "  #figure({caption}kind: {kind}, placement: none,\n  [{image}]){label}{footer}",
    caption = glue::glue("caption: [{opts$caption}], "),
    label = glue::glue(" <{opts$label}>"),
    footer = glue::glue("\n\n  {footnotes}",
                        footnotes = glue::glue_collapse(footnotes, ",\n\n  "),
                        .trim = FALSE),
    .null = NULL, .trim = FALSE
  )
  glue::glue(
    '\n#figure({placement}kind: "none", supplement: none)[\n{inner}\n]\n',
    placement = glue::glue("placement: {opts$placement}, "),
    .null = NULL, .trim = FALSE
  )
}

print_image <- function(image, opts) {
  glue::glue('#image("{image}"{width}{height})',
             width = glue::glue(", width: {opts$width}"),
             height = glue::glue(", height: {opts$height}"),
             .null = NULL,
             .trim = FALSE)
}
