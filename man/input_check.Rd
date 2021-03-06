% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{input_check}
\alias{input_check}
\title{Set overwrite and extension parameters}
\usage{
input_check(overwrite, extensions)
}
\arguments{
\item{overwrite}{If set to `TRUE` (default) existing ArcGIS datasets can be
overwritten.}

\item{extensions}{Optional character vector listing ArcGIS extension that
should be enabled. This adds to any extensions that are eventually
detected by \code{rpygeo_required_extensions}.}
}
\description{
Internal helper function that sets the overwrite parameter for
 the ArcPy session and can check out multiple extensions.
}
\note{
This internal function is used by \code{rpygeo_geoprocessor} and
  by \code{rpygeo_build_env}.
}
\author{
Fabian Polakowski and Alexander Brenning
}
\keyword{internal}
