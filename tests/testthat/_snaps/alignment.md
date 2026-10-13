# format.ttables_horiz_alignment() prints correctly

    Code
      horiz_alignment(c("right", "centre", NA, "start"))
    Output
      <horiz_alignment[4]>
      [1] right  center <NA>   start 

# format.ttables_vert_alignment() prints correctly

    Code
      vert_alignment(c("bottom", "horizon", NA, "top"))
    Output
      <vert_alignment[4]>
      [1] bottom  horizon <NA>    top    

# format.ttables_alignment() prints correctly

    Code
      alignment(c("left", NA, "right", "centre"), c("bottom", "horizon", NA, "top"))
    Output
      <alignment[4]>
      [1] left + bottom horizon       right         center + top 

