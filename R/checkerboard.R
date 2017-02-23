#' Checkerboard
#'
#' A demonstration of how the Multilevel Index of Dissimilarity measures spatial
#' clustering as well as unevenness
#'
#' A criticism of the standard Index of Dissimilarity (ID) is that it only
#' measures one of the two principal dimensions of segregation - unevenness but
#' not spatial clustering. Because of this, very different spatial patterns of
#' segregation can generate the same ID score but the ID is unable to
#' distinguish between them.
#'
#' In contrast, the multilevel index can detect the differences because
#' different patterns (scales) of segregation change the percentage of the
#' variance due to each level. The demonstation illustrates this using the
#' classic example of a checkerboard. The examples show how the percentage
#' of the total variance (labelled Pvariance) moves up the hierarchy with
#' the increase in spatial clustering at greater geographical cases. However,
#' the ID is always the same.
#'
#' The 'stray' cell in examples 2-4 is to allow the model to be fitted. With it
#' the model correctly identifies that some of the variation remains at the
#' base level)
#' @seealso \code{\link{id}}


checkerboard <- function() {

  if(!requireNamespace("raster", quietly = TRUE)) {
    warning("Requires the raster package to be installed")
    ifelse(readline("Install the package? \n1: Yes\n2: No\n") == 1,
          install.packages("raster"),
          stop())
  }
  if(!requireNamespace("sp", quietly = TRUE)) {
    warning("Requires the sp package to be installed")
    ifelse(readline("Install the package? \n1: Yes\n2: No\n") == 1,
           install.packages("sp"),
           stop())
  }

  x <- c(rep(c(1,0), times=8), rep(c(0,1), times=8))
  x <- matrix(x, nrow=16, ncol=16)
  y <- abs(1-x)

  n <- length(x)
  dd <- dim(x)
  rows <- 1:dd[1]
  cols <- 1:dd[2]
  grd <- expand.grid(cols, rows)
  r2 <- ceiling(grd/2)
  ID2 <- paste("A",r2$Var1,"-",r2$Var2, sep="")
  r4 <- ceiling(grd/4)
  ID4 <- paste("B",r4$Var1,"-",r4$Var2, sep="")
  r8 <- ceiling(grd/8)
  ID8 <- paste("C",r8$Var1,"-",r8$Var2, sep="")
  gridcodes <- data.frame(ID=1:n, TwoBy2 = ID2, FourBy4 = ID4, EightBy8 = ID8)

  grd <- raster::raster(x)
  print(sp::spplot(grd, colorkey = FALSE,
                   col.regions = colorRampPalette(c("white", "black"))))

  X <- as.vector(t(x/sum(x)))
  Y <- as.vector(t(y/sum(y)))
  mydata <- data.frame(gridcodes, X, Y)

  cat("\nExample 1 (see plot for pattern)\n")
  print(id(mydata, vars = c("Y","X"),
           levels = c("TwoBy2", "FourBy4", "EightBy8")))

  invisible(readline(prompt="\nPress [enter] to continue"))

  x <- rep(c(1,1,0,0), times=8)
  x <- c(x, rep(c(0,0,1,1), times=8))
  x <- matrix(x, nrow=16, ncol=16)
  x[min(which(x==0))] <- 1
  y <- abs(1-x)

  grd <- raster::raster(x)
  print(sp::spplot(grd, colorkey=FALSE,
                   col.regions = colorRampPalette(c("white", "black")),
                   border = "grey"))

  X <- as.vector(t(x/sum(x)))
  Y <- as.vector(t(y/sum(y)))
  mydata <- data.frame(gridcodes, X, Y)

  cat("\nExample 2 (see plot for pattern)\n")
  print(id(mydata, vars = c("Y","X"),
           levels = c("TwoBy2", "FourBy4", "EightBy8")))

  invisible(readline(prompt="\nPress [enter] to continue"))

  x <- rep(c(1,1,1,1,0,0,0,0), times=8)
  x <- c(x, rep(c(0,0,0,0,1,1,1,1), times=8))
  x <- matrix(x, nrow=16, ncol=16)
  x[min(which(x==0))] <- 1
  y <- abs(1-x)

  grd <- raster::raster(x)
  print(sp::spplot(grd, colorkey = FALSE,
                   col.regions = colorRampPalette(c("white", "black")),
                   border = "grey"))

  X <- as.vector(t(x/sum(x)))
  Y <- as.vector(t(y/sum(y)))
  mydata <- data.frame(gridcodes, X, Y)

  cat("\nExample 3 (see plot for pattern)\n")
  print(id(mydata, vars = c("Y","X"),
           levels = c("TwoBy2", "FourBy4", "EightBy8")))

  invisible(readline(prompt="\nPress [enter] to continue"))

  x <- rep(c(rep(1,8),rep(0,8)), times=8)
  x <- c(x, rep(c(rep(0,8),rep(1,8)), times=8))
  x <- matrix(x, nrow=16, ncol=16)
  x[min(which(x==0))] <- 1
  y <- abs(1-x)

  grd <- raster::raster(x)
  print(sp::spplot(grd, colorkey = FALSE,
                   col.regions = colorRampPalette(c("white", "black")),
                   border = "grey"))

  X <- as.vector(t(x/sum(x)))
  Y <- as.vector(t(y/sum(y)))
  mydata <- data.frame(gridcodes, X, Y)

  cat("\nExample 4 (see plot for pattern)\n")
  print(id(mydata, vars = c("Y","X"),
           levels = c("TwoBy2", "FourBy4", "EightBy8")))


}
