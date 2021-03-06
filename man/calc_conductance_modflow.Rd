% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_conductance.R
\name{calc_conductance_modflow}
\alias{calc_conductance_modflow}
\title{Calculate classic MODFLOW-style stream conductance
Conductance = length x width x streambed hydraulic conductivity / streambed thickness}
\usage{
calc_conductance_modflow(swdf, k_str, str_width, str_thick)
}
\arguments{
\item{swdf}{DataFrame/Geometry of node barycentric coordinates as returned by
\code{\link{calc_stream_voronoi_weights}}}

\item{k_str}{numeric, streambed hydraulic conductivity}

\item{str_width}{numeric, stream width}

\item{str_thick}{numeric, streambed thickness (head loss zone)}
}
\value{
numeric, conductance
}
\description{
Calculate classic MODFLOW-style stream conductance
Conductance = length x width x streambed hydraulic conductivity / streambed thickness
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
swdf$Conductance <- calc_conductance_modflow(swdf, k_str = 1,
                                             str_width = 1, thickness = 0.5)
}
\author{
Leland Scantlebury
}
