% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{summarizeFollowUpTime}
\alias{summarizeFollowUpTime}
\title{Get Median Days of Follow-up time}
\usage{
summarizeFollowUpTime(dcoList, cmOutputPath, reportsOutputPath)
}
\arguments{
\item{dcoList}{The list of drug-comparator-outcomes}

\item{cmOutputPath}{The path pointing to the folder that holds the CohortMethod assets}

\item{reportsOutputPath}{The path pointing to the folder that should hold charts and tables}
}
\details{
Parses StratPop files to calculate the median days of follow-up time
}
\author{
Ajit Londhe
}
