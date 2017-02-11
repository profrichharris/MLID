confint.index <- function(index, level = 0.95) {
  if (class(index) != "index")
    stop("Object is not of class index")
  mlm <- attr(index, "mlm")
  if (is.null(mlm))
    stop("Object is not multilevel")

  cat("\nCalculating variances, please wait")
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
    return(df)
  })
  names(cint) <- attr(index, "levels")
  class(cint) <- "confint"
  return(cint)

}


plot.confint <- function(confint) {
  k <- length(cint)
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
      cex = 0.8
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
    if (length(subset[subset]) > 0)
      text(
        x = (1:n)[subset],
        y = y$mn[subset],
        rownames(y)[subset],
        cex = 0.6,
        pos = ifelse(y$i < median(y$i), 4, 2)[subset]
      )
  })

}



thin <- function(y) {

  d <- abs(diff(y$mn))
  y <- y[d > quantile(d, probs = 0.01),]
  return(y)

}
