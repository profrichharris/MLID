#' Extract Model Residuals
#'
#' For the Index of Dissimilarity (ID), the residuals are the differences
#' between the share of the Y population and the share of the X population per
#' neighbourhood. For the multilevel index, the residuals are estimated at and
#' partitioned betweeneach level of the model.
#'
#' For the standard index, standardized (\code{rstandard}) and studentized
#' (\code{rstudent}) residuals also may be calculated.
#'
#' @param object an object of class \code{index}
#' @param ... other arguments
#' @return a numeric vector of matrix containing the residuals
#' @examples
#' data("ethnicities")
#' index <- id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"))
#' # The ID can be derived from the residuals
#' 0.5 * sum(abs(residuals(index)))
#' # which is the same as
#' index[1]
#'
#' # Extract the standardized and look for regions where the share of the
#' # Bangladeshi population is unusualy high with respect to the White British
#' # resids <- rstandard(index)
#' # table(ethnicities$RGN[resids > 2.58])
#'
#' # Residuals for a multilevel index
#' index <- id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"),
#' levels=c("LLSOA","MLSOA","LAD","RGN"))
#' resids <- residuals(index)
#' head(resids)
#' # Again, the ID can be derived from the residuals
#' 0.5 * sum(abs(rowSums(resids)))
#'
#' # Looking at the residuals, the London effect is different from other regions
#' sort(tapply(resids[,5], ethnicities$RGN, mean))
#'
#' # At the local authority scale it is Tower Hamlets and Newham
#' # (both in London) that have the highest share of the Bangladeshi population
#' # with respect to the White British:
#' tail(sort(tapply(resids[,4], ethnicities$LAD, mean)),5)

residuals.index <- function(object, ...) {

  if (!is.null(attr(object, "mlm"))) {

    vv <- attr(object, "mlm")
    return(rvals(vv))

  } else {

    vv <- attr(object, "ols")
    return(residuals(vv))

  }

}

rstandard.index <- function(model, ...) {

  vv <- attr(model, "ols")
  return(rstandard(vv))

}

rstudent.index <- function(model, ...) {

  vv <- attr(model, "ols")
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

