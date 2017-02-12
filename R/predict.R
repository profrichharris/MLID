

predict.index <- function(index, exclude) {

  mlm <- attr(index, "mlm")
  data <- slot(mlm, "frame")
  rr <- rvals(mlm)
  id <- 0.5*sum(abs(rowSums(rr)))

  drop <- lapply(exclude, function(x, df = data, rr = rr) {
    k <- which(apply(df, 2, function(y) any(y == x)))
    j <- which(df[, k] == x)
    return(list(j, k))
  })

  for(i in 1:length(drop)) {

    j <- drop[[i]][[1]]
    k <- drop[[i]][[2]]
    rr[j, k] <- 0

  }

  newid <- 0.5*sum(abs(rowSums(rr)))
  diff <- newid - id
  pcntchg <- (newid - id) / id * 100
  df <- data.frame(ID = c(id, newid, diff, pcntchg))
  rownames(df) <- c("before", "after", "difference", "% change")
  print(format(round(df,3), nsmall = 3))

}
