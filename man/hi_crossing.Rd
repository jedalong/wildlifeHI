% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hi_crossing.R
\name{hi_crossing}
\alias{hi_crossing}
\title{Crossing analysis of human infrastructure}
\usage{
hi_crossing(move, osmdata, ...)
}
\arguments{
\item{move}{an object of the class \code{move2}. For more information on objects of this type see \code{help(move2)}.}

\item{osmdata}{an \code{sf} object containing human infrastructure data formatted similar to OSM data. 
See \code{?hi_get_osm}.}

\item{...}{additional parameters passed to \code{hi_get_osm}}
}
\value{
This function returns a \code{move2} object containing the original tracking
  data with three additional columns:
 - crossing_true: a logical variable indicating whether a segment crossed any linear features.
 - crossing_key: the OSM key of the feature that was crossed in \code{osmdata}
 - crossing_value: the OSM value of the feature that was crossed in \code{osmdata}.
 - crossing_count: the count of the number of crossing features in \code{osmdata}.
}
\description{
This function calculates whether a trajectory crosses a linear feature of human infrastructure.
}
\details{
This tool simply calculates whether crossing occurs from every segment of consecutive tracking fixes in relation to the input human infrastructure (assumed to be linear features).The default is to use any linear road/trail feature defined in the 'highway' key, but any OSM feature can be specified. See \code{?hi_get_osm}.
}
\examples{
\dontrun{
library(move)
library(move2)
data(fishers)
fishers2 <- mt_as_move2(fishers)
fishers_c <- hi_crossing(fishers2)
table(fishers_c$crossing_value)
}

}
