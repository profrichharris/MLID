

predict.index <- function(index, exclude) {

  mlm <- attr(index, "mlm")
  data <- slot(mlm, "frame")
  rr <- rvals(mlm)
  id <- index[1]

  drop <- lapply(exclude, function(x, df = data, rr = rr) {
    k <- which(apply(df, 2, function(y) any(y == x)))
    j <- which(df[, k] == x)
    return(list(k, j))
  })

  dummies <- matrix(0, ncol = ncol(rr), nrow = nrow(rr))
  subset <- rep(TRUE, nrow(rr))

  for(i in 1:length(drop)) {

    k <- drop[[i]][[1]]
    j <- drop[[i]][[2]]
    rr[j, k] <- 0
    dummies[j, k] <- 1
    subset[j] <- FALSE

  }

  newid <- 0.5 * sum(abs(rowSums(rr)))
  ols2 <- lm(data$y ~ 0, offset = data$"(offset)", subset = subset)
  newid2 <- 0.5 * sum(abs(residuals(ols2)))

  df <- data.frame(ID = c(id, newid, 0, 0), ID2 = c(id, newid2, 0, 0))
  rownames(df) <- c("before", "after", "difference", "% change")
  print(format(round(df,3), nsmall = 3))

  ols2 <- lm(data$y ~ 0 + dummies, offset = data$"(offset)")
  r2 <- summary(ols2)$r.squared

  cat("\nR-squared of",round(r2,3))

}
