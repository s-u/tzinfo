\name{tzinfo}
\alias{tzinfo}
\alias{location2tz}
\alias{location2offset}
\alias{tzoffset}
\title{
  Functions for dealing with time zones
}
\description{
  \code{locaton2tz} determines the time zone at a given location

  \code{tzoffset} returns the offset (in seconds) from GMT for the given
  time zone

  \code{location2offset} determines the time zone at a given location
  and returns its offset
}
\usage{
location2tz(lat, lon)
location2offset(lat, lon, base="2017-01-01")
tzoffset(tzname, base="2017-01-01")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{numeric, latitudes of the points}
  \item{lon}{numeric, longitude of the points}
  \item{base}{string, base date/time to use for offset calculation -
  only relevant if DST is observed in the time zone}
  \item{tzname}{character vector, name of the time zones (as returned
    from \code{location2tz})}
}
\details{
  \code{location2tz} uses the time zone shape file to look up the time
  zone at the desired points.

  \code{tzoffset} uses the system facilities (essentially
  \code{as.POSIXct(base, tz)}) to determine the offset in the provided
  time zone.

  \code{location2offset} is essentially \code{tzoffset(location2tz(...))}.
}
\value{
  \code{location2tz}: character vector of time zone names or \code{NA}
  if the point is in international waters.

  \code{tzoffset}, \code{location2offset}: numeric vector with offset in
  seconds or \code{NA} if a time zone is not recognized by the system
  (or \code{location2tz} return \code{NA} for the points).
}
%\references{
%}
\author{
  Simon Urbanek
}
\note{
  The time zone shapefile is based on
  \url{https://github.com/evansiroky/timezone-boundary-builder},
  in particular 2017a release of the shapefile.
}
%\seealso{
%}
\examples{
  location2tz(c(41, 41), c(-90, -76))
  tzoffset("America/New_York")
  location2offset(c(41, 41), c(-90, -76)) ## without DST
  location2offset(c(41, 41), c(-90, -76), "2017-06-01") ## with DST
}
\keyword{manip}
