##' Extract Model Residuals
##'
##' For the Index of Dissimilarity (ID), the residuals are the differences between the
##' share of the Y population and the share of the X population per neighbourhood.
##' For the multilevel index, the residuals are estimated at each level of the model.
##'
##' For the standard index, standardized (\code{rstandard}) and studentized (\code{rstudent})
##' residuals may also be calculated.
##'
##' @param x an object of class \code{index}
##' @return a numeric vector of matrix containing the residuals
##' @examples
##' data("ethnicities")
##' index <- id(ethnicities, vars = c("Indian", "WhiteBrit"))
##' resids <- residuals(index)
##'
##' index <- id(ethnicities, vars = c("Indian", "WhiteBrit"), levels=c("LLSOA","MLSOA","LAD","RGN"))
##' resids <- residuals(index)
##' head(resids)
##' ## London is different from other regions:
##' sort(tapply(resids[,5], ethnicities$RGN, mean))
##' ## But at a local authority scale it is Leicester that has the highest share of the
##' ## Indian population with respect to the White British:
##' tail(sort(tapply(resids[,4], ethnicities$LAD, mean)),5)
##' @seealso \code{\link{id}}

residuals.index <- function(x, ...) {

  if (!is.null(attr(x, "mlm"))) {

    vv <- attr(x, "mlm")
    return(rvals(vv))

  } else {

    vv <- attr(x, "ols")
    return(residuals(vv))

  }

}

rstandard.index <- function(x, ...) {

  vv <- attr(x, "ols")
  return(rstandard(vv))

}

rstudent.index <- function(x, ...) {

  vv <- attr(x, "ols")
  return(rstudent(vv))

}


rvals <- function(mlm) {

  resids <- residuals(mlm)
  rf <- lme4::ranef(mlm)
  results <- matrix(nrow=length(resids), ncol=length(rf)+1)
  rownames(results) <- names(resids)
  colnames(results) <- c("Base",names(rf))

  results[,1] <- resids
  mod.data <- slot(mlm, "frame")
  for(i in 1:length(rf)) {

    k <- which(names(mod.data) == names(rf)[i])
    mch <- match(mod.data[,k], rownames(rf[[i]]))
    results[,(i+1)] <- rf[[i]][mch,1]

  }
  return(results)

}

