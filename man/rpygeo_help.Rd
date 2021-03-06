% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rpygeo_reticulate.R
\name{rpygeo_help}
\alias{rpygeo_help}
\title{Get help file for ArcPy function}
\usage{
rpygeo_help(arcpy_function)
}
\arguments{
\item{arcpy_function}{ArcPy module with function or class}
}
\description{
This function opens the help file for ArcPy function in viewer panel or if not available in the browser.
}
\examples{

\dontrun{
# Load the ArcPy module and build environment
arcpy <- arcpy_build_env(overwrite = TRUE, workspace = tempdir())

# Open help file
rpygeo_help(arcpy$Slope_3d)
}

}
\author{
Marc Becker
}
