% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kobo_utils.R
\name{normaliseColNames}
\alias{normaliseColNames}
\title{Normalise column names of dataframe to make them R friendly}
\usage{
normaliseColNames(data)
}
\arguments{
\item{data}{The dataframe to be treated.}

\item{questions}{A dataframe with questions from a XLSform. Typically the 'survey' sheet.}
}
\value{
A dataframe without groups in headers.
}
\description{
Normalise column names of dataframe to make them R friendly by removing underscores at beggining of variables
}
\author{
Elliott Messeiller
}
