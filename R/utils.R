is_wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
