


print.index <- function(x, ...) {
  cat(paste(attr(x, "vars")[1:2], collapse=" ~ "),"\n")
  zz <- data.frame(ID = round(x[1], 3), E_ID = round(x[2], 3))
  rownames(zz) <- ""
  print(format(zz, nsmall=3))
  cat("\n")
  if (!is.null(attr(x, "variance"))) {
    v <- attr(x, "variance")
    h <- attr(x, "holdback")
    zz <- data.frame("Pvariance" = v, "Holdback" = h)
    print(format(zz))
  }
}


summary.impacts <- function(x) {

  cat(attr(calcs, "vars"),"\n")
  lapply(x, summary)

}




