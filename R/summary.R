
print.index <- function(x, ...) {
  print(as.numeric(x))
}

summary.index <- function(x, ...) {
  cat(paste(attr(x, "vars"), collapse=" ~ "),"\n")
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


summary.impacts <- function(x, min = 101, max = NA, ...) {

  cat(attr(x, "vars"),"\n")
  lapply(x, shw, min = min, max = max)

}


shw <- function(x, min, max) {

  vv <- x$impact
  if(is.na(min)) min <- min(vv)
  if(is.na(max)) max <- max(vv)
  subset <- vv >= min & vv <= max
  xx <- x[subset,]
  xx <- xx[order(xx$impact),]

}
