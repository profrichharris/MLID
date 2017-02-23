#' Confidence intervals for the multilevel index
#'
#' Calculates the confidence intervals for the residuals of the multilevel
#' index at each level. These can then be visualised in a caterpillar plot.
#'
#' \code{confint.index} is a wrapper to \code{lme4::ranef(mlm, condVar = TRUE)}
#' and is used to calculate the confidence intervals for the locations and
#' regions at each of the higher levels of the model. In this way, places with
#' an usually high (or low) share of population group Y with respect to
#' population group X can be identified, net of the effects of other levels
#' of the model. The width of the confidence interval is adjusted for a test of
#' difference between two means (see Statistical Rules of Thumb by Gerald van
#' Belle, 2011, eq 2.18). A 95 per cent confidence interval, for example,
#' extends to 1.39 times the standard error around the mean and not 1.96.
#'
#' @param object an object of class \code{index}: a multilevel index created
#' using function
#' \code{\link{id}}
#' @param parm NA
#' @param level the confidence level required
#' @param ... other arguments
#' @return an object of class \code{confint}, a list of length equal to the
#' number of levels in the index where each part of the list is a data frame
#' giving the confidence interval for the location
#' @examples
#' \dontrun{
#' data(aggdata)
#' index <- id(aggdata, vars = c("Bangladeshi", "WhiteBrit"),
#' levels = c("MSOA","LAD","RGN"))
#' ci <- confint(index)
#' catplot(ci)
#' }
#' @seealso \code{\link{catplot}} \code{\link{id}} \code{\link[lme4]{ranef}}


confint.index <- function(object, parm, level = 0.95, ...) {
  if (class(object) != "index")
    stop("Object is not of class index")
  mlm <- attr(object, "mlm")
  if (is.null(mlm))
    stop("Object is not multilevel")

  vv <- lme4::ranef(mlm, condVar = TRUE)

  # Statistical Rules of Thumb, Gerald van Belle (2011), eq 2.18
  zz <- (1 - level) / 2 + level
  zz <- qnorm(zz) * sqrt(2) / 2

  lvls <- 1:length(attr(object, "levels"))
  sigm <- sigma(attr(object, "ols"))
  vr <- attr(object, "variance")

  cint <- lapply(lvls, function(x,
                                v = vv,
                                sigma = sigm,
                                z = zz, vrc = vr) {
    se = sqrt(attr(v[[x]], "postVar")[1, ,])
    mn = v[[x]][, 1]
    upr <- mn + z * se
    lwr <- mn - z * se
    df <- data.frame(mn, lwr, upr) / sigma
    rownames(df) <- rownames(v[[x]])
    df <- round(df, 3)
    attr(df, "level") <- names(v)[x]
    attr(df, "variance") <- vrc[x+1]
    return(df)
  })
  names(cint) <- attr(object, "levels")
  class(cint) <- "confintindex"
  return(cint)

}


#' Caterpillar plot
#'
#' Draws a series of caterpillar plots, showing the residuals from the
#' multilevel model at each level and the estimates of their confidence interval
#'
#' A caterpillar plot is a visual way of looking at the variance of the
#' residuals at each level of a multilevel model. It can be used to see which
#' places are contributing most to the Index of Dissimilarity net of the
#' effects of other scales.
#'
#' To aid the interpretability of the plots, the residuals are scaled by the
#' standard error of the residuals from the OLS estimate of the index.
#' Additionally, to avoid over-plotting only a maximum of 50 residuals are
#' shown on each plot. These are the 10 highest and lowest ranked residuals
#' and then a sample of 30 from the remaining residuals, chosen as the ones
#' with values that differ most from the residuals that precede them by ranking.
#' In this way, the plots aim to preserve the tails of the ranked distribution
#' as well as the most important break points inbetween.
#'
#' When \code{ann = TRUE} (the default) some outliers are labelled and the
#' percentage of the total variance due to each level is included. These
#' will not add up to 100% because the base level is not presented.
#' \code{catplot} is a wrapper to \code{\link{plot.confintindex}}
#'
#' @param confint an object containing the output from function
#' \code{\link{confint.index}}
#' @param ann default is TRUE. If set to false, suppresses the automatic
#' annotation of residuals on the plots with a confidence interval that does not
#' overlap with any other
#' @param grid arrange the plots in a grid? (Default is TRUE)
#' @examples
#' \dontrun{
#' data(aggdata)
#' index <- id(aggdata, vars = c("Bangladeshi", "WhiteBrit"),
#' levels = c("MSOA","LAD","RGN"))
#' ci <- confint(index)
#' catplot(ci, grid = TRUE)
#' # Plots for all levels above the base level
#' }
#' @seealso \code{\link{confint.index}} \code{\link{id}}

catplot <- function(confint, ann = TRUE, grid = FALSE) {

  if(class(confint) != "confintindex")
    stop("Object is of wrong type. Use output from confint.index()")
  plot.confintindex(confint, ann, grid)

}


#' Plot the confidence intervals for the multilevel residuals
#'
#' Plots the confidence intervals to produce a caterpillar plot
#'
#' @param x an object containing the output from function
#' \code{\link{confint.index}}
#' @param ann add annotation to the plot?
#' @param grid arrange the plots in a grid? (Default is TRUE)
#' @param ... other arguments
#' @seealso \code{\link{catplot}}

plot.confintindex <- function(x, ann = TRUE, grid = TRUE, ...) {
  k <- length(x)
  grd <- c(0, 0)
  grd[1] <- which.min(abs(1:20 - sqrt(k)))
  grd[2] <- ceiling(k / grd[1])
  if (grd[1] > grd[2])
    grd[c(1, 2)] <- grd[c(2, 1)]

  if(grid) par(mfrow = c(grd[1], grd[2]))

  lapply(x, function(y) {

    y <- y[order(y[, 1], decreasing = TRUE), ]
    n <- nrow(y)
    y$rank <- 1:n

    if (n > 50) {
      ymid <- y[11:(n - 10),]
      while (nrow(ymid) > 30)
        ymid <- .thin(ymid)
      y <- rbind(y[1:10,], ymid, y[(n - 9):n,])
    }

    n <- nrow(y)
    y$i <- 1:n

    plot(
      1:n,
      y$mn,
      ylim = c(min(y[, 2]), max(y[, 3])),
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
    subset <- unlist(lapply(xx, function(z, yy = y)
      ifelse(yy$lwr[z] > yy$upr[z + 1] &
               yy$upr[z] < yy$lwr[z - 1], TRUE, FALSE)))
    subset <- c(y$lwr[1] > y$upr[2], subset, y$upr[n] < y$lwr[n - 1])
    if (length(subset[subset]) > 0 & ann)
      text(
        x = (1:n)[subset],
        y = y$mn[subset],
        rownames(y)[subset],
        cex = 0.6,
        pos = ifelse(y$i < median(y$i), 4, 2)[subset]
      )
    txt <- paste0(attr(y, "variance"),"% of variance")
    if (ann) text(n, 0.9*max(y[, 3]), txt, pos = 2, cex = 0.7, offset = 0)
  })
  par(mfrow = c(1,1))
  return(invisible(NULL))

}



.thin <- function(y) {

  d <- abs(diff(y$mn))
  y <- y[d > quantile(d, probs = 0.01),]
  return(y)

}
