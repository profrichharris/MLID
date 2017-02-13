

predict.index <- function(index, exclude) {

  mlm <- attr(index, "mlm")
  data <- slot(mlm, "frame")
  rr <- rvals(mlm)
  id <- attr(index, "id")

  drop <- lapply(exclude, function(x, df = data, rr = rr) {
    k <- which(apply(df, 2, function(y) any(y == x)))
    j <- which(df[, k] == x)
    return(list(j, k))
  })

  dummies <- matrix(0, ncol = ncol(rr), nrow = nrow(rr))

  for(i in 1:length(drop)) {

    j <- drop[[i]][[1]]
    k <- drop[[i]][[2]]
    rr[j, k] <- 0
    dummies[j, k] <- 1

  }

  ols2 <- lm(data$y ~ 0 + dummies, offset = data$"(offset)")
  r2 <- summary(ols2)$r.squared

  newid <- 0.5*sum(abs(rowSums(rr)))
  diff <- newid - id
  pcntchg <- (newid - id) / id * 100
  df <- data.frame(ID = c(id, newid, diff, pcntchg))
  rownames(df) <- c("before", "after", "difference", "% change")
  print(format(round(df,3), nsmall = 3))
  cat("\nR-squared of",round(r2,3))

}
