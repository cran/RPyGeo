\name{ArcGIS Geoprocessing Tools}
\alias{rpygeo.EucDistance.sa}
\alias{rpygeo.Aspect.sa}
\alias{rpygeo.Slope.sa}
\alias{rpygeo.Hillshade.sa}
\alias{rpygeo.Delete.management}
\title{Wrappers for selected ArcGIS functions}
\description{Wrappers for a small selection of ArcGIS geoprocessing
functions based on the \code{rpygeo.geoprocessor}.}
\usage{
rpygeo.EucDistance.sa(in.data, out.raster, maxdist = NULL, 
    cellsize = NULL, out.direction.raster = NULL, 
    env = rpygeo.env, ...)
rpygeo.Hillshade.sa(in.raster, out.raster, azimuth = 315, 
    altitude = 45, model.shadows = c("NO_SHADOWS", "SHADOWS"), 
    z.factor = 1, ...)
rpygeo.Slope.sa(in.raster, out.raster, 
    unit = c("DEGREE", "PERCENT_RISE"), z.factor = 1, ...)
rpygeo.Aspect.sa(in.raster, out.raster, ...)
rpygeo.Delete.management(in.data, data.type = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in.raster, in.data, out.raster}{Names of ArcGIS raster or vector 
      datasets or feature classes in a geodatabase (relative to the
      current workspace defined in a \code{rpygeo.env} environment).
      Shapefiles must include the extension \dQuote{.shp}.}
  \item{env}{A list defining an RPyGeo working environment as built
      by \code{rpygeo.build.env}.}
  \item{maxdist, cellsize, out.direction.raster,}{  }
  \item{azimuth, altitude, model.shadows, z.factor,}{  }
  \item{unit, data.type}{Arguments to be passed to the Python
       geoprocessing function. See ArcGIS help files for information
       on the usage of scripting commands and their arguments.}
  \item{\dots}{Additional arguments to be passed to \code{rpygeo.geoprocessor}.}
}
\details{
These functions simply try to replicate the behaviour of the
ArcGIS/Python geoprocessing functions of the same name.
See \code{rpygeo.geoprocessor} for details on what happens behind
the scenes.
}
\value{
The function return \code{NULL} if no error occurred, otherwise
a character vector containing the error message.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Alexander Brenning}
%\note{}
\seealso{\code{\link{rpygeo.geoprocessor}}, \code{\link{rpygeo.build.env}}}
\examples{
# Allow ArcGIS to overwrite existing datasets:
\dontrun{rpygeo.env$overwriteoutput = 1}
# Calculate the slope of a DEM raster dataset
# in the current ArcGIS workspace:
\dontrun{rpygeo.geoprocessor("Slope_sa",c("dem","slope"))}
# Same:
\dontrun{rpygeo.geoprocessor("Slope_sa('dem','slope')")}
# Same, using the more convenient wrapper:
\dontrun{rpygeo.Slope.sa("dem","slope")}

# Three at a time or separately:
\dontrun{date()}
\dontrun{rpygeo.geoprocessor("Slope_sa('dem','slope')",
  "Aspect_sa('dem','aspect')", "Hillshade_sa('dem','hshd')")}
\dontrun{date()} # ~20 sec on my computer
\dontrun{rpygeo.Slope.sa("dem","slope")}
\dontrun{rpygeo.Aspect.sa("dem","aspect")}
\dontrun{rpygeo.Hillshade.sa("dem","hshd")}
\dontrun{date()} # ~50 sec
\dontrun{rpygeo.Delete.management("slope")}
\dontrun{rpygeo.Delete.management("aspect")}
\dontrun{rpygeo.Delete.management("hshd")}

# Calculate the Euclidian distance from railway lines
# up to a max. distance of 1000 map units:
\dontrun{rpygeo.geoprocessor("EucDistance_sa",
    args=list("rail.shp","raildist",1000))}
# Same:
\dontrun{rpygeo.EucDistance.sa("rail.shp","raildist",maxdist=1000)}

# Use MapAlgebra to calculate a distance-decay function:
\dontrun{rpygeo.geoprocessor("SingleOutputMapAlgebra_sa",
    args=c("exp( raildist / -100 )","distdecay"))}

# Or why not in just one step if you like MapAlgebra:
\dontrun{rpygeo.geoprocessor( "SingleOutputMapAlgebra_sa",
    args=c("exp( EucDistance( rail.shp, \#, \#, 1000 ) / -100 )","distdecay") )}
}
\keyword{interface}
\keyword{database}
