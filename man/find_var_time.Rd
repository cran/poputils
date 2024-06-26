% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find.R
\name{find_var_time}
\alias{find_var_time}
\title{Identify a Time Variable}
\usage{
find_var_time(nms)
}
\arguments{
\item{nms}{A character vector}
}
\value{
An element of \code{nms}, or \code{NULL}.
}
\description{
Find the element of \code{nms} that looks like an time variable.
If no elements look like a time variable, or if
two or more elements do,
then return \code{NULL}.
}
\examples{
find_var_time(c("Sex", "Year", "AgeGroup", NA)) ## one valid
find_var_time(c("Sex", "Region"))               ## none valid
find_var_time(c("time", "year"))                ## two valid
}
\seealso{
\code{\link[=find_var_age]{find_var_age()}}, \code{\link[=find_var_sexgender]{find_var_sexgender()}}
}
