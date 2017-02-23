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
#' @param drop a character or numeric vector identifying any variables to be
#' dropped from the aggregated data, such as lower-level names and identifiers
#' @return a data frame containing the aggregated data
#' @examples
#' \dontrun{
#' data(ethnicities)
#' head(ethnicities)
#' id(ethnicities, vars = c("Arab","Other","Persons"), expected = TRUE)
#' # the expected value is very high relative to the ID
#' aggdata <- sumup(ethnicities, sumby = "LSOA", drop = "OA")
#' head(aggdata)
#' id(aggdata, vars=c("Arab","Other","Persons"), expected = TRUE)
#' # Note the sensitivity of the ID to the scale of analysis
#' }
#' data(aggdata)
#' head(aggdata)
#' moreagg <- sumup(ethnicities, sumby = "MSOA", drop = "LSOA")
#' head(moreagg)
#' @seealso \code{\link{id}}

sumup <- function(data, sumby, drop = NA) {

  if (is.character((sumby))) {
    ifelse (all(sumby %in% names(data)), sumby <- match(sumby, names(data)),
            stop("Grouping variable not found"))
  }
  if (is.character((drop))) {
    ifelse (all(drop %in% names(data)), drop <- match(drop, names(data)),
            stop("Variables to drop not found"))
  }
  if (anyNA(data)) stop("Data contain NAs")

  cols <- sapply(data, is.numeric)
  if (!is.na(drop)) cols[drop] <- FALSE
  gpddata <- stats::aggregate(data[,cols], by = list(data[,sumby]), sum)
  names(gpddata)[1] <- names(data)[sumby]

  cols <- !cols
  if (!is.na(drop)) cols[drop] <- FALSE
  cols[sumby] <- FALSE

  mch <- match(gpddata[, 1], data[, sumby])
  gpddata <- data.frame(gpddata, data[mch, cols])
  return(gpddata)

}
