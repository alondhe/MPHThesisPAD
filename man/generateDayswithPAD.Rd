% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table1.R
\name{generateDayswithPAD}
\alias{generateDayswithPAD}
\title{Generates Days with PAD}
\usage{
generateDayswithPAD(webApiPrefix, webApiUseSsl, dcoList, cdmDb, scratchDb,
  scratchTablePrefix)
}
\arguments{
\item{webApiPrefix}{The URL prefix of the WebAPI instance}

\item{webApiUseSsl}{Does the WebAPI use HTTPS?}

\item{dcoList}{The list of drug-comparator-outcomes}

\item{cdmDb}{The CDM database object constructed with \code{buildDatabase}}

\item{scratchDb}{The scratch database object constructed with \code{buildDatabase}}

\item{scratchTablePrefix}{The prefix of all scratch tables created}
}
\details{
Generates Days with PAD
}
\author{
Ajit Londhe
}
