% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deck-flag.R
\name{needsNewDeck}
\alias{needsNewDeck}
\title{Check which students need skills assigned}
\usage{
needsNewDeck(minAllowed = retrieve_variable("Deck_Minimum_Threshold"))
}
\arguments{
\item{minAllowed}{Threshold for number of assigned skills. Students under
this threshold are returned.}

\item{date}{Date to use for data}
}
\value{
A data frame
}
\description{
Check which students need skills assigned
}
\examples{
# write later
}
