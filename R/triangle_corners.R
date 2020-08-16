#' Triangle polygon to triangle point sf dataframe
#'
#' @param triangles sf polygon object of triangle(s)
#'
#' @return dataframe of three sf point geometry objects, with ID column for reference
#' @author Leland Scantlebury
#' @export triangle_corners
#'
#' @examples
#' # Read in shapefile
#' tri <- read_sf(system.file("extdata", "720_triangles.shp", package = "MFUSGPBJ"))
#'
#' # Convert to corner coordinates
#' tricorner <- triangle_corners(tri)
triangle_corners <- function(triangles){
  #-- Get coords
  tri_coords <- data.frame(st_coordinates(triangles$geometry))
  tri_coords$id <- ave(rep(1,length(tri_coords$L2)), tri_coords$L2, FUN = cumsum) # ...always four points, but robust
  tri_coords <- tri_coords[tri_coords$id < 4,]  # Assumption: fourth point is always same as first
  #-- Group
  tri_coords <- reshape(tri_coords, idvar = 'L2', timevar = 'id', drop='L1', direction='wide')

  tri_coords_geo <- data.frame('ID'=triangles$ID,
                               'p1'=st_as_sf(tri_coords, coords=c('X.1','Y.1'))$geometry,
                               'p2'=st_as_sf(tri_coords, coords=c('X.2','Y.2'))$geometry,
                               'p3'=st_as_sf(tri_coords, coords=c('X.3','Y.3'))$geometry)
  return(tri_coords_geo)
}
