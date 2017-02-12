##' Extract Model Residuals
##'
##' For the Index of Dissimilarity (ID), the residuals are the differences between the
##' share of the Y population and the share of the X population per neighbourhood.
##' For the multilevel index, the residuals are estimated at each level of the model.
##'
##' For the standard index, standardized (\code{rstandard}) and studentized (\code{rstudent})
##' residuals may also be calculated.
##'
##' @param index an object of class \code{index}
##' @return a numeric vector of matrix containing the residuals
##' @examples
##' data("ethnicities")
##' index <- id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"))
##' resids <- rstandard(index)
##' summary(resids)
##' ethnicities[which.max(resids),]
##'
##' index <- id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"), levels=c("LLSOA","MLSOA","LAD","RGN"))
##' resids <- residuals(index)
##' head(resids)
##'
##' ## London is different from other regions:
##' sort(tapply(resids[,5], ethnicities$RGN, mean))
##'
##' ## At the local authority scale it is Tower Hamlets and Newham that have the highest share of the
##' ## Bangladeshi population with respect to the White British:
##' tail(sort(tapply(resids[,4], ethnicities$LAD, mean)),5)

residuals.index <- function(index, ...) {

  if (!is.null(attr(index, "mlm"))) {

    vv <- attr(index, "mlm")
    return(rvals(vv))

  } else {

    vv <- attr(index, "ols")
    return(residuals(vv))

  }

}

rstandard.index <- function(index, ...) {

  vv <- attr(index, "ols")
  return(rstandard(vv))

}

rstudent.index <- function(index, ...) {

  vv <- attr(index, "ols")
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

