% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.DRNpackage.R
\name{write.DRNpackage}
\alias{write.DRNpackage}
\title{Writes Modflow-USG Drain Package}
\usage{
write.DRNpackage(drndf, filename, nSPs, IDRNCB, SPwarnings = T)
}
\arguments{
\item{drndf}{DataFrame/Geometry of drain nodes as returned by \code{\link{calc_stream_drn}} with
conductances and elevation columns required for stress periods as 'cond[sp]' and 'elev[sp]'}

\item{filename}{character, name/location of output file}

\item{nSPs}{integer, number of stress periods in simulation}

\item{IDRNCB}{integer, CBB flow flag. See details (or PBJ package manual) for more info}

\item{SPwarnings}{T/F (optional) turn on (True) or off (False) warnings about reused or missing SP data}
}
\description{
Writes Modflow-USG Drain Package
}
\examples{
#-- Read in shapefiles
stream <- read_sf(system.file("extdata", "straight_river.shp", package = "pbjr"))
tri <- read_sf(system.file("extdata", "straight_triangles.shp", package = "pbjr"))
vor <- read_sf(system.file("extdata", "straight_voronoi.shp", package = "pbjr"))

#-- Explode polyline
stream <- line_explode(stream)

#-- Create DRNDF
drndf <- calc_stream_drn(stream = stream, voronoi = vor)

#-- Add conductance
drndf$cond1 <- calc_conductance_modflow(drndf, k_streambed = 1,
                                        str_width = 1, thickness = 0.5)
#-- Add elevation
drndf$elev1 <- 90.0

#-- Write file
write.DRNpackage(drndf, filename = paste0(tempdir(),'/straight_model.drn'), nSPs=2, IDRNCB=50)
}
\seealso{
\code{\link{calc_stream_drn}} \code{\link{write.PBJpackage}}
}
\author{
Leland Scantlebury
}
