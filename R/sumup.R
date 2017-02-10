#' Sum the data up into higher level groups
#'
#' Aggregates the data into higher level groups by calculating the sum
#' of all the numeric data columns and preserving their links with other
#' non-numeric columns
#'
#' Sometimes a population group is too few in number sensibly to be analysed at the smallest
#' area scale. An indication of this is when the expected value under randomisation
#' of the Index of Dissimilarity is a large fraction of the observed value. In this case,
#' the data can be aggregated into higher level units, summing the population counts. Aggregating
#' the data can also be used to explore how the ID changes with the scale of analysis.
#'
#' @inheritParams id
#' @param groupby a character or numeric vector of length 1 identifying either the name
#' or columns position of the variables in \code{mydata} that records the higher-level group
#' into which the data will be aggregated (summed)
#' @param omit a character or numeric vector identifying any variables to be omitted from
#' the aggregated data, such as lower-level names and identifiers
#' @return a \code{data.frame} containing the aggregated data
#' @examples
#' data(ethnicities)
#' id(ethnicities, vars=c("Arab","Other","Persons"), expected = T)
#' # the expected value is very high relative to the ID
#'
#' aggdata <- sumup(ethnicities, groupby = "LLSOA", omit = "OA")
#' id(aggdata, vars=c("Arab","Other","Persons"), expected = T)
#' # Note the sensitivity of the ID to the scale of analysis
#'
#' aggdata <- sumup(ethnicities, groupby = "MLSOA", omit = "LLSOA")
#' id(aggdata, vars=c("Arab","Other","Persons"), expected = T)

sumup <- function(mydata, groupby, omit = NA) {

  if (is.character((groupby))) {
    ifelse (all(groupby %in% names(mydata)), groupby <- match(groupby, names(mydata)),
            stop("Grouping variable not found"))
  }
  if (is.character((omit))) {
    ifelse (all(omit %in% names(mydata)), omit <- match(omit, names(mydata)),
            stop("Variables to omit not found"))
  }

  cols <- sapply(mydata, is.numeric)
  if (!is.na(omit)) cols[omit] <- FALSE
  gpddata <- stats::aggregate(mydata[,cols], by = list(mydata[,groupby]), sum)
  names(gpddata)[1] <- names(mydata)[groupby]

  cols <- !cols
  if (!is.na(omit)) cols[omit] <- FALSE
  cols[groupby] <- FALSE

  mch <- match(gpddata[, 1], mydata[, groupby])
  gpddata <- data.frame(gpddata, mydata[mch, cols])
  return(gpddata)

}
