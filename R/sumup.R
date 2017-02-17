#' Sum the data up into higher level groups
#'
#' Aggregates the data into higher level groups by calculating the sum
#' of all the numeric data columns by group
#'
#' Sometimes a population group is too few in number sensibly to be analysed at
#' the smallest area scale. An indication of this is when the expected value
#' under randomisation of the index of dissimilarity is a large fraction of the
#' observed value. In this case, the data can be aggregated into higher level
#' units, summing the population counts. Aggregating the data also can be used
#' to explore how the index changes with the scale of analysis.
#'
#' @inheritParams id
#' @param sumby a character or numeric vector of length 1 identifying either
#' the name or columns position of the variables in \code{data} that records the
#' higher-level group into which the data will be aggregated (summed)
#' @param omit a character or numeric vector identifying any variables to be
#' omitted from the aggregated data, such as lower-level names and identifiers
#' @return a data frame containing the aggregated data
#' @examples
#' data(ethnicities)
#' head(ethnicities)
#' id(ethnicities, vars = c("Arab","Other","Persons"), expected = TRUE)
#' # the expected value is very high relative to the ID
#'
#' aggdata <- sumup(ethnicities, sumby = "LSOA", omit = "OA")
#' head(aggdata)
#' id(aggdata, vars=c("Arab","Other","Persons"), expected = TRUE)
#' # Note the sensitivity of the ID to the scale of analysis
#'
#' aggdata <- sumup(ethnicities, sumby = "MSOA", omit = "LSOA")
#' id(aggdata, vars=c("Arab","Other","Persons"), expected = TRUE)
#' @seealso \code{\link{id}}

sumup <- function(data, sumby, omit = NA) {

  if (is.character((sumby))) {
    ifelse (all(sumby %in% names(data)), sumby <- match(sumby, names(data)),
            stop("Grouping variable not found"))
  }
  if (is.character((omit))) {
    ifelse (all(omit %in% names(data)), omit <- match(omit, names(data)),
            stop("Variables to omit not found"))
  }
  if (anyNA(data)) stop("Data contain NAs")

  cols <- sapply(data, is.numeric)
  if (!is.na(omit)) cols[omit] <- FALSE
  gpddata <- stats::aggregate(data[,cols], by = list(data[,sumby]), sum)
  names(gpddata)[1] <- names(data)[sumby]

  cols <- !cols
  if (!is.na(omit)) cols[omit] <- FALSE
  cols[sumby] <- FALSE

  mch <- match(gpddata[, 1], data[, sumby])
  gpddata <- data.frame(gpddata, data[mch, cols])
  return(gpddata)

}
