#-------------------------------------------------------------------------------------------------#
#' Calculate classic MODFLOW-style stream conductance
#' Conductance = length x width x streambed hydraulic conductivity / streambed thickness
#'
#' @param swdf DataFrame/Geometry of node barycentric coordinates as returned by
#'   \code{\link{calc_stream_voronoi_weights}}
#' @param k_streambed numeric, streambed hydraulic conductivity
#' @param str_width numeric, stream width
#' @param stream_thick numeric, streambed thickness (head loss zone)
#'
#' @return numeric, conductance
#' @author Leland Scantlebury
#' @export calc_conductance_modflow
#'
#' @examples
#' #-- Read in shapefiles
#' str <- read_sf(system.file("extdata", "MehlandHill2010_stream.shp", package = "MFUSGPBJ"))
#' tri <- read_sf(system.file("extdata", "720_triangles.shp", package = "MFUSGPBJ"))
#' vor <- read_sf(system.file("extdata", "720_voronoi.shp", package = "MFUSGPBJ"))
#' str <- line_explode(str)
#'
#' #-- Calculate barycentric weight DF
#' swdf <- calc_stream_voronoi_weights(stream = str, voronoi = vor, triangles = tri)
#'
#' #-- Calculate distances
#' swdf <- stream_elev_from_slope(swdf = swdf, slope = 0.0015, initial_elev = 50)
#'
#' #-- Calculate conductances
#' swdf$Conductance <- calc_conductance_modflow(swdf, k_streambed = 1,
#'                                              str_width = 1, thickness = 0.5)
calc_conductance_modflow <- function(swdf, k_streambed, str_width, thickness) {
  cond <- (k_streambed * swdf$Length * str_width) / thickness
  return(cond)
}
#-------------------------------------------------------------------------------------------------#

#' Calculate classic MODFLOW-style stream conductance (per unit length)
#' Per Unit Length Conductance = width x streambed hydraulic conductivity / streambed thickness
#'
#' @param k_streambed numeric, streambed hydraulic conductivity
#' @param str_width numeric, stream width
#' @param thickness numeric, streambed thickness (head loss zone)
#'
#' @return numeric, conductance
#' @author Leland Scantlebury
#' @export calc_conductance_modflow_perLen
#'
#' @examples
#' #-- Read in shapefiles
#' str <- read_sf(system.file("extdata", "MehlandHill2010_stream.shp", package = "MFUSGPBJ"))
#' tri <- read_sf(system.file("extdata", "720_triangles.shp", package = "MFUSGPBJ"))
#' vor <- read_sf(system.file("extdata", "720_voronoi.shp", package = "MFUSGPBJ"))
#' str <- line_explode(str)
#'
#' #-- Calculate barycentric weight DF
#' swdf <- calc_stream_voronoi_weights(stream = str, voronoi = vor, triangles = tri)
#'
#' #-- Calculate distances
#' swdf <- stream_elev_from_slope(swdf = swdf, slope = 0.0015, initial_elev = 50)
#'
#' #-- Calculate conductances
#' swdf$Conductance <- calc_conductance_modflow_perLen(k_streambed = 1, str_width = 1,
#'                                                     thickness = 0.5)
calc_conductance_modflow_perLen <- function(k_streambed, str_width, thickness) {
  cond <- (k_streambed * str_width) / thickness
  return(cond)
}
