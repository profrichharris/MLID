#' Print values
#'
#' Prints output from the single or multi-level index of dissimilarity
#'
#' @param x output from \code{\link{id}}
#' @param ... other arguments
#' @seealso \code{\link{id}} \code{\link{holdback}}

print.index <- function(x, ...) {
  cat(paste(attr(x, "vars")[1:2], collapse=" ~ "),"\n")
  cat("ID:\t", round(x[1], 3), "\n", sep = "")
  if(length(x) == 1) cat("E(ID):\t(not calculated)\n", sep = "")
  if(length(x) == 2) cat("E(ID):\t", round(x[2], 3), " (",
                         round(x[2]/x[1]*100, 1), "%)\n", sep = "")
  cat("\n")
  if (!is.null(attr(x, "variance"))) {
    v <- attr(x, "variance")
    h <- attr(x, "holdback")
    zz <- data.frame("Pvariance" = v, "Holdback" = h)
    print(format(zz))
  }
}






