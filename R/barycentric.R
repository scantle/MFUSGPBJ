#-------------------------------------------------------------------------------------------------#
#' Calculate barycentric coordinates at point(s)
#'
#' @param x numeric (or array), target x-coordinate
#' @param y numeric (or array), target y-coordinate
#' @param x1 numeric, triangle corner one x-coordinate
#' @param y1 numeric, triangle corner one y-coordinate
#' @param x2 numeric, triangle corner two x-coordinate
#' @param y2 numeric, triangle corner two y-coordinate
#' @param x3 numeric, triangle corner three x-coordinate
#' @param y3 numeric, triangle corner three y-coordinate
#'
#' @return matrix of coordinates, b1, b2, b3
#' @author Leland Scantlebury
#' @export calc_barycentric_coords
#'
#' @examples
#' calc_barycentric_coords(x = c(523300,523400),
#'                         y = c(4925787.45,4925802.08),
#'                         x1 = 523232.75, y1 = 4925893,
#'                         x2 = 523448.87, y2 = 4925906.09,
#'                         x3 = 523353.60, y3 = 4925703.32)
calc_barycentric_coords <- function(x, y, x1, y1, x2, y2, x3, y3) {
  #-- MATH
  b1 <- ((y2-y3)*(x-x3)+(x3-x2)*(y-y3))/((y2-y3)*(x1-x3)+(x3-x2)*(y1-y3))
  b2 <- ((y3-y1)*(x-x3)+(x1-x3)*(y-y3))/((y2-y3)*(x1-x3)+(x3-x2)*(y1-y3))
  b3 <- 1 - b1 - b2
  return(matrix(data = c(b1, b2, b3), nrow = length(x), ncol=3, byrow = F,
                dimnames = list(c(),c('b1','b2','b3'))))
}

#-------------------------------------------------------------------------------------------------#
#' Calculate barycentric coordinates at a given point
#'
#' @param x numeric (or array), target x-coordinate
#' @param y numeric (or array), target y-coordinate
#' @param x1 numeric, triangle corner one x-coordinate
#' @param y1 numeric, triangle corner one y-coordinate
#' @param x2 numeric, triangle corner two x-coordinate
#' @param y2 numeric, triangle corner two y-coordinate
#' @param x3 numeric, triangle corner three x-coordinate
#' @param y3 numeric, triangle corner three y-coordinate
#'
#' @return matrix of coordinates, b1, b2, b3
#' @author Leland Scantlebury
#' @export calc_barycentric_line_coords
#'
#' @examples
#' calc_barycentric_coords(x = c(523300,523400),
#'                         y = c(4925787.45,4925802.08),
#'                         x1 = 523232.75, y1 = 4925893,
#'                         x2 = 523448.87, y2 = 4925906.09,
#'                         x3 = 523353.60, y3 = 4925703.32)
calc_barycentric_line_coords <- function(lx1, ly1, lx2, ly2, x1, y1, x2, y2, x3, y3) {
  #-- MATH
  p1 <- calc_barycentric_coords(lx1, ly1, x1, y1, x2, y2, x3, y3)
  p2 <- calc_barycentric_coords(lx2, ly2, x1, y1, x2, y2, x3, y3)

  colnames(p1) <- c('seg1.a1','seg1.a2','seg1.a3')
  colnames(p2) <- c('seg2.a1','seg2.a2','seg2.a3')

  return(cbind(p1, p2))
}

#-------------------------------------------------------------------------------------------------#
#' Polyline segment & polygon triangle geometries to barycentric coordinates
#'
#' @param seg_geo geometry of line object
#' @param tri_geo geometry of polygon triangle
#'
#' @return matrix of coordinates, b1, b2, b3
#' @author Leland Scantlebury
#' @export geo_to_barycentric_coords
#'
#' @examples
#' # Read in shapefiles
#' str <- read_sf(system.file("extdata", "MehlandHill2010_stream.shp", package = "MFUSGPBJ"))
#' tri <- read_sf(system.file("extdata", "720_triangles.shp", package = "MFUSGPBJ"))
#'
#' # Run function with one segment, one triangle
#' geo_to_barycentric_coords(seg_geo = str[1,]$geometry, tri_geo = tri[120,]$geometry)
geo_to_barycentric_coords <- function(seg_geo, tri_geo) {
  #-- Get coords
  tri_coords <- unique(st_coordinates(tri_geo))
  seg_coords <- st_coordinates(seg_geo)

  #-- Get barycentric coords
  bary <- calc_barycentric_coords(seg_coords[,1], seg_coords[,2],
                                  tri_coords[1,'X'], tri_coords[1,'Y'],
                                  tri_coords[2,'X'], tri_coords[2,'Y'],
                                  tri_coords[3,'X'], tri_coords[3,'Y'])
  return(bary)
}
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#' Polyline segment & polygon triangle geometries to barycentric coordinates
#'
#' @param segments sf line object
#' @param triangles sf triangle polygons
#'
#' @return matrix of coordinates, b1, b2, b3
#' @author Leland Scantlebury
#' @export geo_to_barycentric_coords_vec
#'
#' @examples
#' # Read in shapefiles
#' str <- read_sf(system.file("extdata", "MehlandHill2010_stream.shp", package = "MFUSGPBJ"))
#' tri <- read_sf(system.file("extdata", "720_triangles.shp", package = "MFUSGPBJ"))
#'
#' # Run function with one segment, one triangle
#' geo_to_barycentric_coords(seg_geo = str[1,]$geometry, tri_geo = tri[120,]$geometry)
geo_to_barycentric_coords_vec <- function(segments, seg_triangles) {

  #-- Get coords
  tri_coords <- data.frame(st_coordinates(seg_triangles$geometry))
  seg_coords <- data.frame(st_coordinates(st_geometry(segments)))

  #-- Add cumulative count columns for grouping
  tri_coords$id <- ave(rep(1,length(tri_coords$L2)), tri_coords$L2, FUN = cumsum) # ...always four points, but robust
  tri_coords <- tri_coords[tri_coords$id < 4,]  # Assumption: fourth point is always same as first
  seg_coords$id <- ave(rep(1,length(seg_coords$L1)), seg_coords$L1, FUN = cumsum) # two points

  #-- Group up
  tri_coords <- reshape(tri_coords, idvar = 'L2', timevar = 'id', drop='L1', direction='wide')
  seg_coords <- reshape(seg_coords, idvar = 'L1', timevar = 'id', direction='wide')

  #-- Get barycentric coords
  bary <- calc_barycentric_line_coords(seg_coords[,'X.1'], seg_coords[,'Y.1'],
                                       seg_coords[,'X.2'], seg_coords[,'Y.2'],
                                       tri_coords[,'X.1'], tri_coords[,'Y.1'],
                                       tri_coords[,'X.2'], tri_coords[,'Y.2'],
                                       tri_coords[,'X.3'], tri_coords[,'Y.3'])
  return(bary)
}
#-------------------------------------------------------------------------------------------------#
