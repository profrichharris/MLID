% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mid.R
\name{holdback}
\alias{holdback}
\title{Holdback scores}
\usage{
holdback(mlm)
}
\arguments{
\item{mlm}{an object of class \code{lmerMod} generated by the
\code{lme4} package}
}
\value{
a numeric vector containing the holdback scores
}
\description{
Calculates the holdback scores for a multilevel index of dissimilarity
}
\details{
For the index of dissimilarity (ID), the residuals are the differences
between the share of the Y population and the share of the X population per
neighbourhood. For the multilevel index, the residuals are estimated at and
partitioned between each level of the model. The holdback scores take each
level in the model in turn and set the residuals (the effects) at that
level to zero, then recalculating the ID on that basis and recording
the percentage change in the original value that occurs. The holdback
scores are calculated automatically as part of the function \code{\link{id}}
and can be viewed through \code{print(index)}, where \code{index} is the
object returned by the function, or as \code{attr(index, "holdback")}.
}
\seealso{
\code{\link{id}} \code{\link{print.index}}
}
