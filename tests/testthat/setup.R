df <- mtcars[1:10, 1:5] # example table to print
df$mpg[[4]] <- NA # to test NA printing

# To be moved into separate test files later
x0 <- ttab(
  df, caption = "Motor Trend Car Road Tests", label = "tab:mtcars", rownames = "model",
  colnames = c("Model", "MPG", "Cylinders", "Displacement", "HP", "Rear axle ratio"),
  align = c("left", "center", "centre", rep("right", 3)),
  widths = paste0(c(30, rep(12, 5)), "mm"),
  gutter = list(column = "2pt", row = "1pt"),
  placement = "none",
  fontsize = 8
)

x1 <- x0 %>%
  rowspan(cells(1, 1), 2) %>%
  colspan(cells(3, 2), 3, combine = ", ") %>%
  span(cells(4, 6), list(rows = 3, columns = 2))

x2 <- x1 %>%
  add_footnote("This is an unlabelled footnote") %>%
  add_footnote("This is a symbolic footnote", "symbol", cells(1, 1)) %>%
  add_footnote("Another numbered footnote", "number", cells(1, 7)) %>%
  add_footnote("A numbered footnote", "number", cells(3, location = "header"))

x3 <- x2 %>%
  add_header(c("", "", "Engine", ""), span = list(c(3, 3)))

x4 <- x3 %>%
  pack_rows(4, 2, "Hornet")

x5 <- x4 %>%
  format_cells(cells(c(model, disp), disp < 150), bold = TRUE, align = "centre", size = "12pt", stroke = list(bottom = "1pt + red"))
