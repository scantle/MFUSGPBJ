#-------------------------------------------------------------------------------------------------#
#' Calculate classic MODFLOW-style stream conductance
#' Conductance = length x width x streambed hydraulic conductivity / streambed thickness
#'
#' @param swdf DataFrame/Geometry of node barycentric coordinates as returned by
#'   \code{\link{calc_stream_voronoi_weights}}
#' @param k_str numeric, streambed hydraulic conductivity
#' @param str_width numeric, stream width
#' @param str_thick numeric, streambed thickness (head loss zone)
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
#' swdf$Conductance <- calc_conductance_modflow(swdf, k_str = 1,
#'                                              str_width = 1, thickness = 0.5)
calc_conductance_modflow <- function(swdf, k_str, str_width, str_thick) {
  cond <- (k_str * swdf$Length * str_width) / str_thick
  return(cond)
}
#-------------------------------------------------------------------------------------------------#

#' Calculate classic MODFLOW-style stream conductance (per unit length)
#' Per Unit Length Conductance = width x streambed hydraulic conductivity / streambed thickness
#'
#' @param k_str numeric, streambed hydraulic conductivity
#' @param str_width numeric, stream width
#' @param str_thick numeric, streambed thickness (head loss zone)
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
#' swdf$Conductance <- calc_conductance_modflow_perLen(k_str = 1, str_width = 1,
#'                                                     thickness = 0.5)
calc_conductance_modflow_perLen <- function(k_str, str_width, str_thick) {
  cond <- (k_str * str_width) / str_thick
  return(cond)
}

#' Calculate conductance using Mehl & Hill (2010) Equation 3
#'
#' Vectorized, non-constant values should be same length as swdf.
#'
#' @param swdf DataFrame/Geometry of node barycentric coordinates as returned by
#'   \code{\link{calc_stream_voronoi_weights}}
#' @param k_str numeric, streambed hydraulic conductivity
#' @param kv_node numeric, vertical hydraulic conductivity of the aquifer material
#' @param str_width numeric, stream width
#' @param str_thick numeric, streambed thickness (head loss zone)
#' @param node_thick numeric, thickness of finite difference cell (node)
#' @param node_area Planar cross-sectional area of cell (node)
#'
#' @return numeric, conductance
#' @author Leland Scantlebury
#' @export calc_conductance_MHEq3
#'
#' @examples
calc_conductance_MHEq3 <- function(swdf, k_str, kv_node, str_width, str_thick, node_thick, node_area) {
  area <- str_width * swdf$Length
  b2 <- node_thick / 2
  cond <- ((str_thick/(k_str*area)) + ((b2-str_thick)/(kv_node*node_area)))^-1
  return(cond)
}

#' Calculate conductance using Mehl & Hill (2010) Equation 4
#'
#' Vectorized, non-constant values should be same length as swdf.
#'
#' @param swdf DataFrame/Geometry of node barycentric coordinates as returned by
#'   \code{\link{calc_stream_voronoi_weights}}
#' @param k_str numeric, streambed hydraulic conductivity
#' @param kv_node numeric, vertical hydraulic conductivity of the aquifer material
#' @param str_width numeric, stream width
#' @param str_thick numeric, streambed thickness (head loss zone)
#' @param node_thick numeric, thickness of finite difference cell (node)
#'
#' @return numeric, conductance
#' @references Mehl, S. and Hill, M.C. (2010) Grid-size dependence of Cauchy boundary conditions used to
#' simulate stream-aquifer interactions. Advances in Water Resources 33. 430-442.
#' @author Leland Scantlebury
#' @export calc_conductance_MHEq4
#'
#' @examples
calc_conductance_MHEq4 <- function(swdf, k_str, kv_node, str_width, str_thick, node_thick) {
  area <- str_width * swdf$Length
  b2 <- node_thick / 2
  cond <- ((str_thick/(k_str*area)) + ((b2-str_thick)/(kv_node*area)))^-1
  return(cond)
}
