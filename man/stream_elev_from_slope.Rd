% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stream_elev_from_slope.R
\name{stream_elev_from_slope}
\alias{stream_elev_from_slope}
\title{Calculate streambed elevations given geometry & slope
Assumes a single stream, with segments properly ordered.}
\usage{
stream_elev_from_slope(swdf, slope, initial_elev, order_by_index = F, sp = 1)
}
\arguments{
\item{swdf}{DataFrame/Geometry of node barycentric coordinates as returned by
\code{\link{calc_stream_voronoi_weights}}}

\item{slope}{numeric, streambed slope}

\item{initial_elev}{streambed elevation at start of segment 1}
}
\value{
swdf DataFrame/Geometry with added seg1.elev and seg2.elev columns
}
\description{
Calculate streambed elevations given geometry & slope
Assumes a single stream, with segments properly ordered.
}
\examples{
#-- Read in shapefiles
str <- read_sf(system.file("extdata", "MehlandHill2010_stream.shp", package = "pbjr"))
tri <- read_sf(system.file("extdata", "720_triangles.shp", package = "pbjr"))
vor <- read_sf(system.file("extdata", "720_voronoi.shp", package = "pbjr"))
str <- line_explode(str)

#-- Calculate barycentric weight DF
swdf <- calc_stream_voronoi_weights(stream = str, voronoi = vor, triangles = tri)

#-- Calculate distances
swdf <- stream_elev_from_slope(swdf = swdf, slope = 0.0015, initial_elev = 50)

#-- Calculate conductances
swdf$Conductance <- calc_conductance_modflow(swdf, k_streambed = 1,
                                             str_width = 1, thickness = 0.5)
}
\author{
Leland Scantlebury
}
