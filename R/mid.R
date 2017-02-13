## These are the functions for fitting the multilevel index using lme4,
## calculating the percentage of the variance at each level and
## also the holdback scores


mid <- function(data, vars = c(1, 2), levels, expected = F, nsims = 100) {
  if (is.character((levels))) {
    ifelse (all(levels %in% names(data)), levels <- match(levels, names(data)),
            stop("Higher level grouping variable not found"))
  }
  if (anyNA(data[,levels])) stop("Data contain NAs")
  id <- idx(data, vars, expected, nsims)
  ols <- attr(id, "ols")
  vv <- attr(id, "vars")
  df <- attr(id, "data")
  lvls <- names(data)[levels]
  data <- data.frame(y = data[, vars[1]], x = data[, vars[2]], data[, levels])
  data$y <- data$y / sum(data$y)
  data$x <- data$x / sum(data$x)
  f <- "y ~ 0"
  for(k in 3: ncol(data)) {
    f <- paste(f, "+", paste0("(1|", names(data)[k],")"))
  }
  mlm <- lme4::lmer(f, data=data, offset=x)
  attributes(id) <- list(ols = ols, vars = vv, levels = lvls,
                         mlm = mlm, variance = varshare(mlm), holdback = holdback(mlm),
                         data = df)
  class(id) <- "index"
  return(id)
}


mlvar <- function(mlm) {

  vv <- lme4::VarCorr(mlm)
  res <- attr(vv, "sc")

  for(i in 1: length(vv)) {

    res <- c(res,attr(vv[[i]], "stddev"))

  }

  res <- res^2
  names(res) <- c("Base",names(vv))
  return(res)
}


varshare <- function(mlm, mlvar=NULL) {

  if(is.null(mlvar)) mlvar <- mlvar(mlm)
  mlvar <- mlvar/sum(mlvar)*100
  return(round(mlvar,2))

}



holdback <- function(mlm=NULL, rvals=NULL) {
  if(is.null(rvals)) rvals <- rvals(mlm)
  k <- ncol(rvals)
  hb <- rep(NA, k)
  rwsm <- rowSums(rvals)
  ID <- sum(abs(rwsm))
  for(i in 1:k) {

    rwsm <- rowSums(rvals[,-i])
    hb[i] <-  sum(abs(rwsm))/ID - 1

  }
  names(hb) <- colnames(rvals)
  return(round(hb*100,2))

}

