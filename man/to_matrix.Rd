% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_matrix.R
\name{to_matrix}
\alias{to_matrix}
\title{Build a Matrix from Measure and ID Variables}
\usage{
to_matrix(x, rows, cols, measure)
}
\arguments{
\item{x}{A data frame.}

\item{rows}{The ID variable(s)
used to distinguish rows in the matrix.}

\item{cols}{The ID variable(s)
used to distinguish columns in the matrix.}

\item{measure}{The measure variable, eg
rates or counts.}
}
\value{
A matrix
}
\description{
Build a matrix where the elements are values of
a measure variable, and the rows and columns
are formed by observed combinations of ID
variables. The ID variables picked
out by \code{rows} and \code{cols} must uniquely identify
cells. \code{to_matrix()}, unlike \code{stats::xtabs()},
does not sum across multiple combinations of
ID variables.
}
\examples{
x <- expand.grid(age = c(0, 1, 2),
                 sex = c("F", "M"),
                 region = c("A", "B"),
                 year = 2000:2001)
x$count <- 1:24

to_matrix(x,
          rows = c(age, sex),
          cols = c(region, year),
          measure = count)

to_matrix(x,
          rows = c(age, sex, region),
          cols = year,
          measure = count)

## cells not uniquely identified
try(
to_matrix(x,
          rows = age,
          cols = sex,
          measure = count)
)
}
