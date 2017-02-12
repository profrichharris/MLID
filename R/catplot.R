#' Confidence intervals for the multilevel index
#'
#' Calculates the confidence intervals for the residuals of the multilevel index at each level.
#' These can then be visualised in a caterpillar plot.
#'
#' \code{confint.index} is a wrapper to \code{lme4::ranef(mlm, condVar = T)} and is used to
#' calculate the confidence intervals for the locations and regions at each of the higher levels
#' of the model. In this way, places with an usually high (or low) share of population group Y
#' with respect to population group X can be identified, net of the effects of other levels
#' of the model. The width of the confidence interval is adjusted for a test of difference
#' between two means (see Statistical Rules of Thumb by Gerald van Belle, 2011, eq 2.18).
#' A 95 per cent confidence interval, for example, extends to 1.39 times the standard error
#' around the mean and not 1.96.
#'
#' @param index an object of class \code{index}: a multilevel index created using function \code{id}
#' @param level the confidence level required
#' @return an object of class \code{confint}, a list of length equal to the number
#' of levels in the index where each part of the list is a data frame giving the confidence
#' interval for the location
#' @examples
#' data("ethnicities")
#' index <- id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"), levels=c("LLSOA","MLSOA","LAD","RGN"))
#' ci <- confint(index)
#' catplot(ci)


confint.index <- function(index, level = 0.95) {
  if (class(index) != "index")
    stop("Object is not of class index")
  mlm <- attr(index, "mlm")
  if (is.null(mlm))
    stop("Object is not multilevel")

  cat("\nCalculating standard errors, please wait")
  vv <- lme4::ranef(mlm, condVar = T)

  # Statistical Rules of Thumb, Gerald van Belle (2011), eq 2.18
  zz <- (1 - level) / 2 + level
  zz <- qnorm(zz) * sqrt(2) / 2

  lvls <- 1:length(attr(index, "levels"))
  sigm <- sigma(attr(index, "ols"))

  cint <- lapply(lvls, function(x,
                                v = vv,
                                sigma = sigm,
                                z = zz) {
    se = sqrt(attr(v[[x]], "postVar")[1, ,])
    mn = v[[x]][, 1]
    upr <- mn + z * se
    lwr <- mn - z * se
    df <- data.frame(mn, lwr, upr) / sigma
    rownames(df) <- rownames(v[[x]])
    df <- round(df, 3)
    attr(df, "level") <-names(v)[x]
    return(df)
  })
  names(cint) <- attr(index, "levels")
  class(cint) <- "catplotdata"
  return(cint)

}


#' Caterpillar plot
#'
#' Draws a series of caterpillar plots, showing the residuals from the multilevel model
#' at each level and the estimates of their confidence interval
#'
#' A caterpillar plot is a visual way of looking at the variance of the residuals at each
#' level of a multilevel model. It can be used to see which places are contributing most to the
#' Index of Dissimilarity net of the effects of other scales.
#'
#' To aid the interpretability of the plots, the residuals are scaled by the standard error
#' of the residuals from the OLS estimate of the index. Additionally, to avoid over-plotting
#' only a maximum of 75 residuals are shown on each plot. These are the 10 highest and lowest
#' rank residuals and then a sample of 55 from the remaining residuals, chosen at the ones
#' with values that differ most from the residuals that precede them by ranking. In this way,
#' the plots aim to preserve the tails of the distribution as well as the most important
#' break points inbetween.
#'
#' @param confint an object containing the output from function \code{confint.index}
#' @param labels default is TRUE. If set to false, suppresses the automatic labelling
#' of residuals on the plots with a confidence interval that does not overlap with any other
#' @examples
#' data("ethnicities")
#' index <- id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"), levels=c("LLSOA","MLSOA","LAD","RGN"))
#' ci <- confint(index)
#' catplot(ci)

catplot <- function(confint, labels = T) {

  if(class(confint) != "catplotdata") stop("Object is of wrong type. Use output from confint.index()")
  plot.catplotdata(confint, labels)

}


plot.catplotdata <- function(confint, labels = T) {
  k <- length(confint)
  grd <- c(0, 0)
  grd[1] <- which.min(abs(1:20 - sqrt(k)))
  grd[2] <- ceiling(k / grd[1])
  if (grd[1] > grd[2])
    grd[c(1, 2)] <- grd[c(2, 1)]

  par(mfrow = c(grd[1], grd[2]))

  lapply(confint, function(y) {
    y <- y[order(y[, 1], decreasing = T), ]
    n <- nrow(y)
    y$rank <- 1:n

    if (n > 75) {
      ymid <- y[11:(n - 10),]
      while (nrow(ymid) > 55)
        ymid <- thin(ymid)
      y <- rbind(y[1:10,], ymid, y[(n - 9):n,])
    }

    n <- nrow(y)
    y$i <- 1:n

    plot(
      1:n,
      y$mn,
      ylim = c(min(y[, 1:3]), max(y[, 1:3])),
      xlab =  "Rank",
      ylab = "Scaled residual",
      las = 1,
      pch = 20,
      xaxt = "n",
      cex = 0.8,
      main = attr(y, "level")
    )
    subset <- as.integer(seq(1, n, length.out = 5))
    axis(1,
         at = y$i[subset],
         labels = y$rank[subset],
         cex.axis = 0.9)
    abline(h = 0, lty = "dotted")
    apply(y, 1, function(x)
      arrows(
        x0 = x[5],
        y0 = x[2],
        x1 = x[5],
        y1 = x[3],
        length = 0.01,
        angle = 90,
        code = 3
      ))
    xx <- 2:(n - 1)
    subset <- unlist(lapply(xx, function(x, yy = y)
      ifelse(yy$lwr[x] > yy$upr[x + 1] &
               yy$upr[x] < yy$lwr[x - 1], T, F)))
    subset <- c(y$lwr[1] > y$upr[2], subset, y$upr[n] < y$lwr[n - 1])
    if (length(subset[subset]) > 0 & labels)
      text(
        x = (1:n)[subset],
        y = y$mn[subset],
        rownames(y)[subset],
        cex = 0.6,
        pos = ifelse(y$i < median(y$i), 4, 2)[subset]
      )
  })
  return()

}



thin <- function(y) {

  d <- abs(diff(y$mn))
  y <- y[d > quantile(d, probs = 0.01),]
  return(y)

}
