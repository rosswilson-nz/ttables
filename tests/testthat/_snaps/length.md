# format.ttables_length() prints correctly

    Code
      ttables_length(abs_length(c(1:3, NA, NA), c("pt", "mm", "cm", "in", "in")),
      em_length(c(2, NA, 3, 4, NA)))
    Output
      <length[5]>
      [1] 1pt + 2em 2mm       3cm + 3em 4em       <NA>     

