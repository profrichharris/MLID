#' (Multilevel) Index of dissimilarity
#'
#' \code{id} Returns either the standard index of dissimilarity (ID) or its
#' multilevel equivalent
#'
#' If \code{y} is the number of population group Y living in each neighbourhood
#' and \code{x} is the number of population group X then \code{id} measures how
#' unevenly distributed are the two groups relative to one another and is a
#' measure of segregation. In addition, for geographically hierarchichal data,
#' scale effects may be explored to examine the scale of geographical
#' clustering.
#'
#' \code{print(index)} displays the ID value, the expected value of the ID
#' under randomisation (NA if not calculated), and, for a multilevel model,
#' the percentage share of the variance at each level and the holdback scores.
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
#' }
#' and, for a multilevel model,
#' \itemize{
#'    \item \code{attr(index, "mlm")} an object of class \code{lmerMod}.
#'    Fitted using \code{\link[lme4]{lmer}}
#'    \item \code{attr(index, "variance")} the percentage of the total variance
#'    due to each level of the model. This indicates the scale at which the
#'    segregation is most prominent
#'    \item \code{attr(index, "holdback")} records the percentage change in the
#'    ID that occurs if, at each level, its contribution to the ID is heldback
#'    (set to zero)
#' }
#' @examples
#' data("ethnicities")
#' head(ethnicities)
#' # Calculate the standard index value
#' id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"))
#'
#' # Calculate also the expected value under randomisation
#' id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"), expected = TRUE)
#' id(ethnicities, vars = c("Bangladeshi", "WhiteBrit", "Persons"),
#' expected = TRUE)
#'
#' ## A multilevel model
#' id(ethnicities, vars = c("Bangladeshi", "WhiteBrit"),
#' levels=c("LLSOA","MLSOA","LAD","RGN"))
#' id(ethnicities, vars = c("Bangladeshi", "WhiteBrit", "Persons"),
#' levels=c("LLSOA","MLSOA","LAD","RGN"), expected = TRUE)
#' @seealso \code{\link{residuals.index}} \code{\link[lme4]{lmer}}

id <- function(data, vars, levels = NA, expected = FALSE, nsims = 100) {
  if (is.character((vars))) {
    ifelse (all(vars %in% names(data)), vars <- match(vars, names(data)),
                                          stop("Variable not found"))
  }
  if (!all(sapply(data[, vars], is.numeric))) stop("Variable is not numeric")
  if (anyNA(data[,vars])) stop("Data contain NAs")
  ifelse (!is.na(levels), id <- mid(data, vars, levels, expected, nsims),
            id <- idx(data, vars, expected, nsims))
  return(id)
}


idx <- function(data, vars, expected = FALSE, nsims = 100,
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
    id <- c(id, id.sim(X, Y, N, nsims))
  }
  attributes(id) <- list(ols = ols, vars = names(data)[vars],
                         data = data.frame(Y, X))
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

