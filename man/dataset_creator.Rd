% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_creator.R
\name{dataset_creator}
\alias{dataset_creator}
\title{Parse input vectors into a format for \code{{mgcv}}}
\usage{
dataset_creator(cases, reference_date, group, day_of_week, backend)
}
\arguments{
\item{cases}{A vector of non-negative incident case counts occurring on an
associated \code{reference_date}. Missing values (NAs) are not allowed.}

\item{reference_date}{The associated date on which the count of incident
\code{cases} occurred. Missing dates are not allowed and dates can only occur
once.}

\item{group}{The grouping variable for the case/reference-date pair. Not yet
implemented and a value other than \code{NULL} will throw an error.}

\item{day_of_week}{A boolean or a vector of custom values to be applied to the
model as a random effect. If \code{TRUE}, then \code{RtGam} will use the parsed
\code{reference_date} values to infer the day of week. If a vector of the same
length as \code{reference_date}, then the user-supplied values are used as the
day-of-week effect, overriding the actual day of week. This approach can
be used to, for example, adjust for atypical reporting due to a holiday.
If \code{FALSE} no day of week effect is applied.}

\item{backend}{One of \code{gam} or \code{bam}; defaults to \code{gam}. In general, models
should be fit with \code{\link[mgcv:gam]{mgcv::gam()}}. If \code{\link[mgcv:gam]{mgcv::gam()}} is too slow,
\code{\link[mgcv:bam]{mgcv::bam()}} converges more quickly but introduces some additional
numerical error. Note that the \code{bam} backend uses the \code{discrete = TRUE}
option for an additional speedup. See \code{\link[mgcv:bam]{mgcv::bam()}} for more information.}
}
\value{
A dataframe for mgcv
}
\description{
Parse input vectors into a format for \code{{mgcv}}
}
\keyword{internal}
