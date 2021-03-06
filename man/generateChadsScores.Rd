% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table1.R
\name{generateChadsScores}
\alias{generateChadsScores}
\title{Generates CHADS scores}
\usage{
generateChadsScores(dcoList, cdmDb, scratchDb, scratchTablePrefix,
  chadsTypes = c("Chads2", "Chads2Vasc"))
}
\arguments{
\item{dcoList}{The list of drug-comparator-outcomes}

\item{cdmDb}{The CDM database object constructed with \code{buildDatabase}}

\item{scratchDb}{The scratch database object constructed with \code{buildDatabase}}

\item{scratchTablePrefix}{The prefix of all scratch tables created}

\item{chadsTypes}{Which CHADS score types should be generated?}
}
\details{
Generates CHADS scores, both CHADS2 and CHADS2Vasc
}
\author{
Ajit Londhe, Jamie Weaver
}
