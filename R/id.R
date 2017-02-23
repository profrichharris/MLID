#' (Multilevel) index of dissimilarity
#'
#' Returns either the standard index of dissimilarity (ID) or its
#' multilevel equivalent
#'
#' If \code{Y} is the number of population group Y living in each neighbourhood
#' and \code{X} is the number of population group X then \code{id} measures how
#' unevenly distributed are the two groups relative to one another and is a
#' measure of segregation. In addition, for geographically hierarchichal data,
#' scale effects may be explored to examine the scale of geographical
#' clustering.
#'
#' The method works by treating the calculation of the ID as a
#' regression problem: if \code{Y} is recalculated as the share per
#' neighbourhood of the total count of population group Y
#' (i.e. \code{Y <- Y / sum(Y)}) and \code{X} is recalculated in the same way
#' for X, then fitting \code{ols <- lm(Y ~ 0, offset = X)} generates a set of
#' residuals, \code{e <- residuals(ols)} where each residual is the difference
#' in the share of Y and the share of X per neighbourhood, and the sum of the
#' absolute of those residuals can be used to obtain the id:
#' \code{id <- 0.5 * sum(abs(e))}.
#'
#' The advantage of calculating the ID in this
#' way is that it can be extended to consider geographic hierarchies, where
#' neighbourhoods at the base level can be grouped into larger regions at
#' the next level, and so forth. Then, for the multilevel index,
#' the residuals are estimated at and partitioned between each level of the
#' model \emph{net} of the other levels, allowing scale effects to be
#' explored.
#'
#' \code{print(index)} displays the ID value, the expected value of
#' the ID under randomisation (NA if not calculated), and, for a multilevel
#' model, the percentage share of the total variance due to each level
#' (a measure of the geographical scale of segregation: see the examples given
#' by \code{\link{checkerboard}}) and the holdback scores -
#' see \code{\link{holdback}}
#'
#' @param data a data frame with \code{ncol(data) >= 2}. Each row of the data
#' represents a neighbourhood or some other areal unit for which counts of
#' population have been made.
#' @param vars a character or numeric vector of length 2 or 3 giving either the
#' names or columns positions of the variables in \code{data} in the following
#' order:
#' \enumerate{
#'    \item the number of population group Y in each neighbourhood
#'    \item the number of population group X in each neighbourhood
#'    \item (optional) The total population in each neighbourhood
#' }
#' @param levels a character or numeric vector of minimum length 1 identifying
#' either the names or columns positions of the variables in \code{data} that
#' record to which higher-level grouping each lower-lower level unit belongs.
#' If \code{levels = NA}, the default, then only the standard index of
#' dissimilarity is calculated.
#' @param expected a logical scaler. Should the expected value of the ID under
#' randomisation be calculated? Requires a measure of the total population in
#' each neighbourhood. If omitted from \code{vars} that total will be calculated
#' as \code{sum(X + Y)}.
#' @param nsims a vector, the number of random draws to be used for calculating
#' the expected value. Default is 100.
#' @param omit (optional) a character vector containing the names of places to
#' search for in the data and to omit from the calculations
#' @return an object of class \code{index}. This is a value between zero and one
#' where 0 implies no segreation, and 1 means 'complete segregation' - wherever
#' group Y is located, X is not (and vice versa). If \code{expected = TRUE} the
#' expected value under randomisation also is given. In addition, the object
#' contains the following attributes:
#' \itemize{
#'    \item \code{attr(x, "ols")} an object of class \code{lm}. The OLS
#'    regression used to calculate the ID. Useful for identifying significant
#'    residuals (see Example below)
#'    \item \code{attr(x, "vars")} the names of Y and X in \code{data}
#'    \item \code{attr(x, "data")} a data frame with the population counts
#'    for Y and X
#' }
#' and also, for a multilevel model,
#' \itemize{
#'    \item \code{attr(index, "mlm")} an object of class \code{lmerMod}.
#'    Fitted using \code{\link[lme4]{lmer}}
#'    \item \code{attr(index, "variance")} the percentage of the total variance
#'    due to each level of the model. This indicates the scale at which the
#'    segregation is most prominent
#'    \item \code{attr(index, "holdback")} records the percentage change in the
#'    ID that occurs if, at each level, its contribution to the ID \emph{net} of
#'    other levels is heldback (set to zero)
#' }
#' @examples
#' data(ethnicities)
#' head(ethnicities)
#' # Calculate the standard index value
#' id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"))
#'
#' \dontrun{
#' # Calculate also the expected value under randomisation
#' id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"), expected = TRUE)
#' # will generate a warning because the total population per neighbourhood
#' # has not been specified
#' id(ethnicities, vars = c("Bangladeshi", "WhiteBrit", "Persons"),
#' expected = TRUE)
#' # The expected value is a high percentage of the actual value so
#' # aggregate it into a higher level geography...
#' aggdata <- sumup(ethnicities, sumby = "LSOA", drop = "OA")
#' head(aggdata)
#'
#' # Multilevel models
#' id(aggdata, vars = c("Bangladeshi", "WhiteBrit"),
#' levels = c("MSOA","LAD","RGN"))
#' id(aggdata, vars = c("Bangladeshi", "WhiteBrit"),
#' levels = c("MSOA","LAD","RGN"), omit = c("Tower Hamlets", "Newham"))
#' }
#' @seealso \code{\link{checkerboard}} \code{\link{print.index}}
#' \code{\link{holdback}} \code{\link{residuals.index}} \code{\link[lme4]{lmer}}
#' \code{\link{sumup}}
#'
#' Harris R (2017) Fitting a multilevel index of segregation in R:
#' using the MLID package \url{http://rpubs.com/profrichharris/MLID}
#'
#' Harris R (2017) Measuring the scales of segregation: Looking at the
#' residential separation of White British and other school children in England
#' using a multilevel index of dissimilarity \url{http://bit.ly/2lQ4r0n}

id <- function(data, vars, levels = NA, expected = FALSE,
               nsims = 100, omit = NULL) {
  if (is.character((vars))) {
    ifelse (all(vars %in% names(data)), vars <- match(vars, names(data)),
                                          stop("Variable not found"))
  }
  if (!all(sapply(data[, vars], is.numeric))) stop("Variable is not numeric")
  if (anyNA(data[,vars])) stop("Data contain NAs")
  if (!is.null(omit)) data <- .omit(data, omit)
  ifelse (!is.na(levels), id <- .mid(data, vars, levels, expected, nsims),
            id <- .idx(data, vars, expected, nsims))
  return(id)
}


.idx <- function(data, vars, expected = FALSE, nsims = 100,
                include.model = FALSE) {
  Y <- data[, vars[1]]
  X <- data[, vars[2]]
  x <- X / sum(X)
  y <- Y / sum(Y)
  ols <- lm(y ~ 0, offset = x, model = FALSE)
  id <- round(0.5 * sum(abs(residuals(ols))), 3)
  if (expected) {
    if (length(vars) == 3) {
      N <- data[, vars[3]]
    } else {
      N <- X + Y
      warning("Calculated the expected value but the total population of each
              area was not specified")
    }
    id <- c(id, .id.sim(X, Y, N, nsims))
  }
  attributes(id) <- list(ols = ols, vars = names(data)[vars],
                         data = data.frame(Y, X))
  class(id) <- "index"
  return(id)
}


.id.sim <- function(X, Y, N, nsims) {
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



.omit <- function(data, omit) {
  if(any(sapply(omit, is.numeric))) warning("Places to omit include
                                            numeric data")
  drop <- lapply(omit, function(x, df = data) {
    k <- which(apply(df, 2, function(y) any(y == x)))
    if(length(k) == 0) stop("Places to omit not found")
    j <- which(df[, k] == x)
    return(j)
  })
  drop <- unlist(drop)
  data <- data[-drop,]
  return(data)
}
