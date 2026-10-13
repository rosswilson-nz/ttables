# format.relative() prints correctly

    Code
      relative(ttables_length(abs_length(c(1:3, NA, NA), c("pt", "mm", "cm", "in",
        "in")), em_length(c(2, NA, 3, 4, NA))), ratio(c(NA, 5, 6, 7, NA)))
    Output
      <relative[5]>
      [1] 1pt + 2em      5% + 2mm       6% + 3cm + 3em 7% + 4em       <NA>          

