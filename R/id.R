#' (Multilevel) Index of dissimilarity
#'
#' \code{id} Returns either the standard index of dissimilarity (ID) or its multilevel equivalent
#'
#' If \code{y} is the number of population group Y living in each neighbourhood
#' and \code{x} is the number of population group X then \code{id} measures how unevenly
#' distributed are the two groups relative to one another and is a measure of segregation.
#' In addition, for geographically hierarchichal data, scale effects may be explored to
#' examine the scale of geographical clustering.
#'
#' @param mydata a data.frame with \code{ncol(mydata) >= 2}. Each row of the data represents
#' a neighbourhood or some other areal unit for which counts of population have been made.
#' @param vars a character or numeric vector of length 2 or 3 giving either the names
#' or columns positions of the variables in \code{mydata} in the following order
#' \enumerate{
#'    \item the number of population group Y in each neighbourhood
#'    \item the number of population group X in each neighbourhood
#'    \item (optional) The total population in each neighbourhood
#' }
#' @param levels a character or numeric vector of minimum length 1 identifying either the names
#' or columns positions of the variables in \code{mydata} that record to which higher-level grouping
#' each lower-lower level unit belongs. \code{length(levels) + 1} is the number of levels in the
#' model. If \code{levels = NA}, the default, then only the standard index of dissimilarity is
#' calculated.
#' @param expected a logical scaler. Should the expected value of the ID under randomisation
#' be calculated? Requires a measure of the total population in each neighbourhood. If omitted
#' from \code{vars} that total will be calculated as \code{sum(X + Y)}.
#' @param nsims a vector, the number of random draws to be used for calculating the expected value.
#' Default is 100.
#' @return An object of class \code{index}. This is a value between zero and one where 0 implies no
#' segreation, and 1 means that wherever group Y is located, X is not (and vice versa).
#' If \code{expected = T} the expected value under randomisation also is given.
#' In addition, the object contains the following attributes:
#' \enumerate{
#'    \item \code{attr(x, "ols")} an object of class \code{lm}. The OLS regression used to
#'    calculate the ID. Useful for identifying significant residuals (see Example below)
#'    \item \code{attr(x, "vars")} the names of Y and X in \code{mydata}
#' }
#' and, for a multilevel model,
#' \enumerate{
#'    \item \code{attr(x, "mlm")} an object of class \code{lmerMod}. Fitted using
#'    the \code{lme4} package
#'    \item \code{attr(x, "variance")} the percentage of the total variance due to each level
#'    of the model. This indicates the scale at which the segregation is most prominent
#'    \item \code{attr(x, "holdback")} records the percentage change in the ID that occurs if,
#'    at each level, its contribution to the ID is heldback (set to zero)
#' }
#' @examples
#' data("ethnicities")
#' head(ethnicities)
#'
#' ## Caculate the standard index value
#' id(ethnicities, vars = c("Indian", "WhiteBrit"))
#'
#' ## Calculate also the expected value under randomisation
#' id(ethnicities, vars = c("Indian", "WhiteBrit"), expected = T)
#' id(ethnicities, vars = c("Indian", "WhiteBrit", "Persons"), expected = T)
#'
#' ## The index is fitted as a standard OLS model so we can
#' ## extract the standardized residuals and, in this example, look for where the share
#' ## of the Indian population is unusualy high with respect to the White British
#' index <- id(ethnicities, vars = c("Indian", "WhiteBrit"))
#' resids <- rstandard(index)
#' head(ethnicities[resids > 2.58,])
#' table(ethnicities$RGN[resids > 2.58])
#'
#' ## A multilevel model
#' id(ethnicities, vars = c("Indian", "WhiteBrit"), levels=c("LLSOA","MLSOA","LAD","RGN"))
#' id(ethnicities, vars = c("Indian", "WhiteBrit", "Persons"),
#' levels=c("LLSOA","MLSOA","LAD","RGN"), expected = T)

id <- function(mydata, vars, levels = NA, expected = F, nsims = 100) {
  if (is.character((vars))) {
    ifelse (all(vars %in% names(mydata)), vars <- match(vars, names(mydata)),
                                          stop("Variable not found"))
  }
  if (!all(sapply(mydata[, vars], is.numeric))) stop("Variable is not numeric")
  ifelse (!is.na(levels), id <- mid(mydata, vars, levels, expected, nsims),
            id <- idx(mydata, vars, expected, nsims))
  return(id)
}

idx <- function(mydata, vars, expected = F, nsims = 100, include.model = F) {
  Y <- mydata[, vars[1]]
  X <- mydata[, vars[2]]
  x <- X / sum(X)
  y <- Y / sum(Y)
  ols <- lm(y ~ 0, offset = x)
  id <- round(0.5 * sum(abs(residuals(ols))), 3)
  if (expected) {
    if (length(vars) == 3) {
      N <- mydata[, vars[3]]
    } else {
      N <- X + Y
      warning("Calculated the expected value but the total population of each area was not specified")
    }
    id <- c(id, id.sim(X, Y, N, nsims))
  }
  attributes(id) <- list(ols = ols, vars = names(mydata)[vars])
  class(id) <- "index"
  return(id)
}

id.sim <- function(X, Y, N, nsims) {
  k <- length(X)
  pY <- sum(Y) / sum(N)
  pX <- sum(X) / sum(N)
  pN <- N / sum(N)
  results <- vector("numeric", nsims)
  for(i in 1: nsims) {
    Nrnd <- rbinom(k, sum(N), pN)
    Yrnd <- rbinom(k, Nrnd, pY)
    Xrnd <- rbinom(k, Nrnd, pX)
    y <- Yrnd / sum(Yrnd)
    x <- Xrnd / sum(Xrnd)
    results[i] <- 0.5 * sum(abs(y / sum(y) - x / sum(x)))
  }
  return(round(mean(results), 3))
}

