% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_prep.R
\name{two_digits}
\alias{two_digits}
\title{Two Digits}
\usage{
two_digits(data)
}
\arguments{
\item{data}{}
}
\description{
Rounds numbers two the second digit, and returns them as a \code{character} in two digits. Useful for number consistency during visualization.
}
\examples{
two_digits(4.5664)
"4.57"
two_digits(0.1)
"0.10"
}
