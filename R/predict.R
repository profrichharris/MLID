

predict.index <- function(index, places) {

  mlm <- attr(index, "mlm")
  data <- slot(mlm, "frame")
  rawdata <- slot(index, "data")
  rr <- rvals(mlm)
  id <- index[1]

  drop <- lapply(places, function(x, df = data, rr = rr) {
    k <- which(apply(df, 2, function(y) any(y == x)))
    j <- which(df[, k] == x)
    return(list(k, j))
  })

  dummies <- matrix(0, ncol = ncol(rr), nrow = nrow(rr))
  subset <- rep(F, nrow(rr))

  for(i in 1:length(drop)) {

    k <- drop[[i]][[1]]
    j <- drop[[i]][[2]]
    rr[j, k] <- 0
    dummies[j, k] <- 1
    subset[j] <- T

  }

  newid <- 0.5 * sum(abs(rowSums(rr)))

  ols <- lm(data$y ~ 0, offset = data$"(offset)", subset = subset)
  newid2 <- 0.5 * sum(abs(residuals(ols)))

  newid3 <- idx(rawdata[subset,], c(1, 2))[1]

  ols <- lm(data$y ~ 0 + dummies, offset = data$"(offset)")

  df <- data.frame(ID1 = c(id, newid), ID2 = c(id, newid2),
                   ID3 = c(id, newid3))

  impact <- sum(subset) / length(subset)
  impact <- (df[2, 2] / df[1, 2]) / impact * 100

  df <- round(df, 3)
  rownames(df) <- c("before", "after")

  attr(df, "Rsq") <- round(summary(ols)$r.squared, 3)
  attr(df, "impact") <- round(impact, 0)
  attr(df, "places") <- places
  attr(df, "vars") <- attr(index, "vars")
  class(df) <- "predictindex"
  return(df)

}


print.predictindex <- function(x) {
  cat(paste(attr(x, "vars")[1:2], collapse=" ~ "),"\n")
  df <- matrix(unlist(x), ncol = length(x))
  colnames(df) <- names(x)
  rownames(df) <- attr(x,"row.names")
  print(df)
  exclude <- attr(x,"places")
  cat("\nR-squared:", attr(x, "Rsq"), "   Impact", attr(x, "impact"))
  cat("\n\nNote:")
  cat("\nID1: if the residual effect of",exclude,"is set to zero")
  cat("\nID2: if the shares of", paste(attr(x, "vars")[1:2], collapse=" & "),
      "are equal everywhere except", exclude)
  cat("\nID3: the index value for", exclude, "alone")


}

