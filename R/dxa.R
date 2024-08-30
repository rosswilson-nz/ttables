to_dxa <- function(x) {
  if (inherits(x, "ttables_length")) {
    field(x, "abs") <- field(x, "abs") + abs_length(8, "pt") * as.numeric(field(x, "em"))
    field(x, "em") <- em_length(0)
  }
  as_pt(x) * 20
}
