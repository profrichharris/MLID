
print.index <- function(x, ...) {
  print(as.numeric(x))
}

summary.index <- function(x, ...) {
  cat("\nThe index of dissimilarity is",format(round(x[1], 3), nsmall = 3))
  cat("\nfor variables",attr(x, "vars"),"\n")
  if (length(x) == 2) cat("with an expected value under randomisation of",x[2])
  if (!is.null(attr(x, "variance"))) {
    v <- attr(x, "variance")
    txt <- "\nThe percentage of the variance at each level is:\n"
    for(i in 1:length(v)) {
      txt <- paste(txt, names(v[i]),"\t\t",format(round(v[i], 2), nsmall = 2),"\n",sep="")
    }
    cat(txt)
  }
  if (!is.null(attr(x, "holdback"))) {
    v <- attr(x, "holdback")
    txt <- "\nPercentage change in the ID if the contribution of the level is set to zero:\n"
    for(i in 1:length(v)) {
      txt <- paste(txt, names(v[i]),"\t\t",format(round(v[i], 2), nsmall = 2),"\n",sep="")
    }
    cat(txt)
  }
}


summary.impacts <- function(x, min = 100, max = NA, ...) {

  lapply(x, show, min = min, max = max)

}


show <- function(x, min, max) {

  vv <- x$impact
  if(is.na(min)) min <- min(vv)
  if(is.na(max)) max <- max(vv)
  subset <- vv >= min & vv <= max
  xx <- x[subset,]
  xx <- xx[order(xx$impact),]

}
