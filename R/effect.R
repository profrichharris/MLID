#' Consider the effect of particular places upon the ID
#'
#' Evaluates the effect on the index of the named \code{places} under three
#' different scenarios.
#'
#' The three different scenarios considered are:
#' \enumerate{
#'   \item if the effects (the estimated residuals from the multilevel
#'   model) are set to zero for the named higher-level \code{places};
#'   \item if the shares of the two population groups are equal everywhere
#'   except within the named \code{places}; and
#'   \item if all but the neighbourhoods within the named \code{places}
#'   are omitted from the data and the index then recalculated
#'   using only those that remain.
#' }
#' The evaluation also includes:
#' \itemize{
#'    \item the impact of the chosen places upon the overall ID, where a value
#'    over 100 indicates a group of neighbourhoods with a disproportionately
#'    high (in the same way that \code{\link{impacts}} calculates it)
#'    \item an R-squared value. This is the proportion of the total variation in
#'    (Y - X) that is due to the chosen places, where (Y - X) are the
#'    differences between the share of population group Y and the share of
#'    population group X that are observed in each neighbourhood.
#' }
#' See \code{vignette("MLID")} for further details
#' @param object an object of class \code{index}: a multilevel index created
#' using function \code{\link{id}}
#' @param places a character vector containing the names of the places in any of
#' the higher-level geographies for which the evaluation will be made
#' @return an object, primarily a list containing the evaluated values
#' @examples
#' \dontrun{
#' data(aggdata)
#' index <- id(aggdata, vars = c("Bangladeshi", "WhiteBrit"),
#' levels = c("MSOA","LAD","RGN"))
#' ci <- confint(index)
#' catplot(ci)
#' # Note Tower Hamlets and Newham. Obtain the predictions for them:
#' effect(index, "Tower Hamlets")
#' effect(index, "Newham")
#' effect(index, c("Tower Hamlets","Newham"))
#' }
#' @seealso \code{\link{id}}
#'
#' Harris R (2017) Fitting a multilevel index of segregation in R:
#' using the MLID package \url{http://rpubs.com/profrichharris/MLID}

effect <- function(object, places) {

  mlm <- attr(object, "mlm")
  data <- slot(mlm, "frame")
  rawdata <- slot(object, "data")
  rr <- .rvals(mlm)
  id <- object[1]
  if(any(sapply(places, is.numeric))) warning("Places contain numeric data")

  drop <- lapply(places, function(x, df = data, rr = rr) {
    k <- which(apply(df, 2, function(y) any(y == x)))
    if(length(k) == 0) stop("Places not found")
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
    subset[j] <- TRUE

  }

  newid <- 0.5 * sum(abs(rowSums(rr)))

  ols <- lm(data$y ~ 0, offset = data$"(offset)", subset = subset)
  newid2 <- 0.5 * sum(abs(residuals(ols)))

  newid3 <- .idx(rawdata[subset,], c(1, 2))[1]

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
  attr(df, "vars") <- attr(object, "vars")
  class(df) <- "fxindex"
  return(df)

}

#' Print values
#'
#' Prints predicted changes to the index of dissimilarity under various
#' scenarios
#'
#' @param x output from \code{\link{effect}}
#' @param ... other arguments


print.fxindex <- function(x, ...) {
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
  cat("\nID3: the index value for", exclude, "alone\n")


}

