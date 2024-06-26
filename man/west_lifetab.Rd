% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{west_lifetab}
\alias{west_lifetab}
\title{Coale-Demeny West Model Life Tables}
\format{
A data frame with 1,050 rows and the
following variables:
\itemize{
\item \code{level}: Index for life table. Lower level implies
lower life expectancy.
\item \code{sex}: \code{"Female"}, and \code{"Male"}.
\item \code{age}: Age, in life table age groups, with an open age
group of 95+.
\item \code{mx}: Mortality rate.
\item \code{ax}: Average years lived in age interval by people
who die in that interval.
\item \code{qx}: Probability some alive at start of age interval
dies during interval.
\item \code{lx}: Number of people still alive at start of
age interval.
\item \code{dx}: Number of people dying during age interval.
\item \code{Lx}: Number of person-years lived during age interval.
\item \code{ex}: Expectation of life at start of age interval.
}
}
\source{
Coale A, Demeny P, and Vaughn B. 1983.
Regional model life tables and stable populations.
2nd ed. New York: Academic Press,
accessed via \code{demogR::cdmltw()}.
}
\usage{
west_lifetab
}
\description{
Life table quantities from the "West" family
of Coale-Demeny model life tables.
}
\keyword{datasets}
