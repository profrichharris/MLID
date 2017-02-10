mid <- function(mydata, vars = c(1, 2), levels, expected = F, nsims = 100) {
  if (is.character((levels))) {
    ifelse (all(levels %in% names(mydata)), levels <- match(levels, names(mydata)),
            stop("Higher level grouping variable not found"))
  }
  id <- idx(mydata, vars, expected, nsims)
  ols <- attr(id, "ols")
  vv <- attr(id, "vars")
  mydata <- data.frame(y = mydata[, vars[1]], x = mydata[, vars[2]], mydata[, levels])
  mydata$y <- mydata$y / sum(mydata$y)
  mydata$x <- mydata$x / sum(mydata$x)
  f <- "y ~ 0"
  for(k in 3: ncol(mydata)) {
    f <- paste(f, "+", paste0("(1|", names(mydata)[k],")"))
  }
  mlm <- lme4::lmer(f, data=mydata, offset=x)
  attributes(id) <- list(ols = ols, vars = vv, mlm = mlm, variance = varshare(mlm), holdback = holdback(mlm))
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

