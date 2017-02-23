#' Aggregated population counts by ethnic group
#'
#' Ethnicity data for England and Wales from the 2011 Census. Each row in the
#' data set gives population counts for Lower Level Super Output Area (LSOA).
#' The census geography is hierarchical: LSOAs group into Middle Level Super
#' Output Areas (MSOAs), which group into Local Authority Districts (LADs)
#' and then into Government Regions (RGNs). These groupings are included in
#' the data.
#'
#' @format A \code{data.frame} with 19 columns:
#' \itemize{
#'    \item LSOA, the Census ID for the Output Area
#'    \item Persons, the residential population count for the OA
#'    \item columns 3-16, the number of people White British, Irish, of an other
#'    White ethnicity, of a mixed ethnicity, Indian, Pakistani, Bangladeshi,
#'    Chinese, of an other Asian ethnicity, Black African, Black Caribbean, of
#'    an other Black ethnicity, Arab or of an other ethnicity.
#'    \item columns 17-19, the ID codes for the higher-level geographies:
#'    MSOAs, LADs and RGNs
#' }
#' @source Office for National Statistics; National Records of Scotland;
#' Northern Ireland Statistics and Research Agency (2016): 2011 Census
#' aggregate data. UK Data Service (Edition: June 2016). DOI:
#' \url{http://dx.doi.org/10.5257/census/aggregate-2011-1}.
#' This information is licensed under the terms of the Open Government Licence
#' \url{http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3}.
#' The LSOA, MSOA, LAD and RGN codes are from \url{http://bit.ly/2lGMdkE} and
#' are supplied under the Open Government Licence: Contains National Statistics
#' data. Crown copyright and database right 2017.
#' @seealso \code{\link{ethnicities}}
"aggdata"
