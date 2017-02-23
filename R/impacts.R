#' Impact calculations
#'
#' Calculates the total contribution to the index of dissimilarity of
#' neighbourhoods grouped by regions or other higher-level geographies
#'
#' When the index of dissimilarity (ID) is estimated as a regression model
#' the residuals from that model are the differences between the share of
#' population group Y and the share of population group X that are observed in
#' each neighbourhood. The \code{impacts} function summaries those differences
#' by higher-level geographies to consider which places or regions have the
#' neighbourhoods that contribute most to the ID. The measures are useful
#' for understanding where the seperations of the two population groups are
#' greatest. However, to look at scale effects, where the effect of each level
#' \emph{net} of the other levels is wanted, fit a multilevel index using
#' function \code{\link{id}}.
#'

#' @param data a data frame with \code{ncol(data) >= 2}. Each row of the data
#' represents a neighbourhood or some other areal unit for which counts of
#' population have been made.
#' @param vars a character or numeric vector of length 2 or 3 giving either
#' the names or columns positions of the variables in \code{data} in the
#' following order
#' \enumerate{
#'    \item the number of population group Y in each neighbourhood
#'    \item the number of population group X in each neighbourhood
#' }
#' @param levels a character or numeric vector of minimum length 1 identifying
#' either the names or columns positions of the variables in \code{data} that
#' record to which higher-level grouping each lower-lower level unit belongs
#' @param omit (optional) a character vector containing the names of places to
#' search for in the data and to omit from the calculations
#' @return A list of data.frames, each containing the impact calculations for
#' the higher-level geographies. The variables are
#' \itemize{
#'    \item \code{pcntID} The total contribution of the neighbourhoods within
#'    the region to the overall ID score, expressed as a percentage
#'    \item \code{pcntN} The number of neighbourhoods within the region,
#'    expressed as a percentage of the total number in \code{data}
#'    \item \code{impact} The ratio of \code{pcntID} to \code{pcntN} multiplied
#'    by 100. Values over 100 indicate a group of neighbourhoods that have a
#'    disproportionately high impact on the ID
#'    \item \code{scldMean} The average difference between the share of the Y
#'    population and the share of the X population, scaled by the standard error
#'    of the differences for the whole data set (to give a z-value). Positive
#'    values mean that, on average, the region has a greater share of the Y
#'    population than the X. Negative values mean it has less.
#'    \item \code{scldSD} A measure of how much the differences between the
#'    shares of the two populations vary within the region. It is the standard
#'    deviation of those differences scaled by the standard error for the whole
#'    data set. Higher values indicate greater variability within the region.
#'    \item \code{scldMin} The minimum difference between the share of the Y
#'    population and the share of the X for neighbourhoods within the region,
#'    scaled by the standard error
#'    \item \code{scldMax} The maximum difference between the share of the Y
#'    population and the share of the X for neighbourhoods within the region,
#'    scaled by the standard error
#'    \item \code{pNYgtrNX} The percentage of neighbourhoods within the region
#'    where the count of population group Y (as opposed to the share) is
#'    greater than the count of population group X
#' }
#' @examples
#' data(aggdata)
#' impx <- impacts(aggdata, c("Bangladeshi", "WhiteBrit"), c("LAD","RGN"))
#' head(impx)
#' # sorted by impact score
#' # For $RGN London has the greatest impact on the ID
#' # The 'excess' share of the Bangladeshi population is not especially
#' # significant (see scldMean) but there is a lot of variation between
#' # neighbourhoods (see scldSD)
#' # For $LAD note the impacts of Tower Hamlets and Newham


impacts <- function(data, vars, levels, omit = NULL) {

  if (is.character((vars))) {
    ifelse (all(vars %in% names(data)), vars <- match(vars, names(data)),
            stop("Variable not found"))
  }
  if (!all(sapply(data[, vars], is.numeric))) stop("Variable is not numeric")
  if (is.character((levels))) {
    ifelse (all(levels %in% names(data)), levels <- match(levels, names(data)),
            stop("Higher level grouping variable not found"))
  }
  if (!is.null(omit)) data <- .omit(data, omit)

  id <- .idx(data, vars)
  rr <- residuals(id)
  se <- sigma(attr(id, "ols"))

  levels <- as.list(levels)
  impx <- lapply(levels, .impact.calcs, data, rr, se, vars)
  names(impx) <- names(data[, unlist(levels)])
  attr(impx, "vars") <- paste(names(data[, vars]), collapse=" ~ ")
  class(impx) <- "impacts"
  return(impx)

}



.impact.calcs <- function(col, data, rr, se, vars) {

  t1 <- tapply(abs(rr), data[,col], sum) / sum(abs(rr)) * 100
  t2 <- tapply(abs(rr), data[,col], length) / length(rr) * 100

  impact <- t1/t2 * 100

  t3 <- (tapply(rr, data[,col], mean) - mean(rr)) / se
  t4 <- tapply(rr, data[,col], sd) / se
  t5 <- tapply(rr, data[,col], min) / se
  t6 <- tapply(rr, data[,col], max) / se

  Y <- data[,vars[1]]
  X <- data[,vars[2]]
  t7 <- tapply(1:length(Y), data[,col], function(x) {
    sum(Y[x] > X[x]) / length(x) * 100
  })

  df <- data.frame(pcntID = round(t1,2), pcntN = round(t2,2),
                   impact = round(impact, 0),
                   scldMean = round(t3,2), scldSD = round(t4,2),
                   scldMin = round(t5,2), scldMax = round(t6,2),
                   pNYgtrNX = round(t7, 1))

  return(df)
}

#' Return the lowest impact scores for each higher level area
#'
#' Returns the last parts of the set of impact calculations, ordered
#' by the impact score
#'
#' @param x an object of class \code{impacts} generated by the function
#' \code{impacts}
#' @param n a single integer giving the number of rows to show. Defaults
#' to 5.
#' @param ... other arguments
#' @seealso \code{\link{impacts}}

tail.impacts <- function(x, n = 5, ...) {

  cat(attr(x, "vars"),"\n")
  lapply(x, function(y) {
    y <- y[order(y$impact, decreasing = TRUE),]
    tail(y, n = n)
    })

}

#' Return the highest impact scores for each higher level area
#'
#' Returns the first parts of the set of impact calculations, ordered
#' by the impact score
#'
#' @param x an object of class \code{impacts} generated by the function
#' \code{impacts}
#' @param n a single integer giving the number of rows to show. Defaults
#' to 5.
#' @param ... other arguments
#' @seealso \code{\link{impacts}}

head.impacts <- function(x, n = 5, ...) {

  cat(attr(x, "vars"),"\n")
  lapply(x, function(y) {
    y <- y[order(y$impact, decreasing = TRUE),]
    head(y, n = n)
  })

}


#' Print values
#'
#' Prints the impact values
#'
#' @param x output from \code{\link{impacts}}
#' @param ... other arguments


print.impacts <- function(x, ...) {
  nms <- names(x)
  attributes(x) <- NULL
  names(x) <- nms
  print(x)
}
