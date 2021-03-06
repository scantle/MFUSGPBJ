% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.RIVpackage.R
\name{write.RIVpackage}
\alias{write.RIVpackage}
\title{Writes Modflow-USG River Package}
\usage{
write.RIVpackage(rivdf, filename, nSPs, IRIVCB, SPwarnings = T)
}
\arguments{
\item{rivdf}{DataFrame/Geometry of drain nodes as returned by \code{\link{calc_stream_drn}} with
conductances and elevation columns required for stress periods as 'cond[sp]', 'elev[sp]', and 'stage[sp]'}

\item{filename}{character, name/location of output file}

\item{nSPs}{integer, number of stress periods in simulation}

\item{IRIVCB}{integer, CBB flow flag. See details (or PBJ package manual) for more info}

\item{SPwarnings}{T/F (optional) turn on (True) or off (False) warnings about reused or missing SP data}
}
\description{
Writes Modflow-USG River Package
}
\examples{
#-- Read in shapefiles
stream <- read_sf(system.file("extdata", "straight_river.shp", package = "pbjr"))
tri <- read_sf(system.file("extdata", "straight_triangles.shp", package = "pbjr"))
vor <- read_sf(system.file("extdata", "straight_voronoi.shp", package = "pbjr"))

#-- Explode polyline
stream <- line_explode(stream)

#-- Create rivdf
rivdf <- calc_stream_drn(stream = stream, voronoi = vor)

#-- Add conductance
rivdf$cond1 <- calc_conductance_modflow(rivdf, k_streambed = 1,
                                        str_width = 1, thickness = 0.5)
#-- Add river bottom elevation
rivdf$elev1 <- 90.0

#-- Add stage as 1 m above river bottom
rivdf$stage1 <- rivdf$elev1 + 1

#-- Write file
write.DRNpackage(rivdf, filename = paste0(tempdir(),'/straight_model.drn'), nSPs=2, IRIVCB=50)
}
\seealso{
\code{\link{calc_stream_drn}} \code{\link{write.PBJpackage}}
}
\author{
Leland Scantlebury
}
