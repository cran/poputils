% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age.R
\name{age_group_type}
\alias{age_group_type}
\title{Infer Age Label Type}
\usage{
age_group_type(x)
}
\arguments{
\item{x}{A vector of age labels}
}
\value{
\code{"single"}, \code{"five"}, or \code{"lt"}.
}
\description{
Determine whether a set of age labels
refer to one-year, five-year, or
life-table age groups.
}
\details{
The valid types of age labels are:
\itemize{
\item \code{"single"}. One-year age groups, eg
\code{"0"} or \code{"55"}, and possibly
an open age group, eg \code{"90+"}.
\item \code{"five"}. Five-year age groups, eg
\code{"0-4"} or \code{"55-59"}, and possibly
an open age group, eg \code{"100+"}.
\item \code{"lt"}. Life table age groups, eg
\code{"0"}, \code{"1-4"}, \code{"5-9"},
\code{"55-59"}, or \code{"80+"}.
}

If \code{x} does not fit any of these
descriptions, \verb{then age_group_type()} throws
an error.

If \code{x} could belong to
more than one type, then \code{age_group_type()}
prefers \code{"single"} to \code{"five"} and \code{"lt"},
and prefers \code{"five"} to \code{"lt"}.
}
\examples{
age_group_type(c("5-9", "0-4", "100+"))
age_group_type(c("2", "5", "1"))
age_group_type(c("0", "1-4"))

## could be any "single" or "lt"
age_group_type("0")

## could be "five" or "lt"
age_group_type("80-84")
}
