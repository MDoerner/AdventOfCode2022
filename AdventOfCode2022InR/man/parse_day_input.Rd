% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{parse_day_input}
\alias{parse_day_input}
\title{Parse Advent of Code Input}
\usage{
parse_day_input(input_string, day, part = 1)
}
\arguments{
\item{input_string}{A character vector of length 1 containing the input for the specified problem in AdventOfCode}

\item{day}{An integer between 1 and 25 specifying the day of AdventOfCode}

\item{part}{The integer 1 or 2 specifying the part of the \code{day} of AdventOfCode}
}
\value{
Some data structure suitable as input to the specified problem in AdventOfCode
}
\description{
Parses the \code{input_string} into a data structure suitable to solve the specified problem in AdventOfCode.
}
\examples{
parse_day_input("1000\n 2000\n 3000\n\n 4000", day = 1, part = 1)
}
