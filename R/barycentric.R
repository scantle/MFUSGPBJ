#-------------------------------------------------------------------------------------------------#
#' Title
#'
#' @param x 
#' @param y 
#' @param x1 
#' @param y1 
#' @param x2 
#' @param y2 
#' @param x3 
#' @param y3 
#'
#' @return
#' @export
#'
#' @examples
calc_barycentric_coords <- function(x, y, x1, y1, x2, y2, x3, y3) {
  #-- MATH
  b1 <- ((y2-y3)*(x-x3)+(x3-x2)*(y-y3))/((y2-y3)*(x1-x3)+(x3-x2)*(y1-y3))
  b2 <- ((y3-y1)*(x-x3)+(x1-x3)*(y-y3))/((y2-y3)*(x1-x3)+(x3-x2)*(y1-y3))
  b3 <- 1 - b1 - b2
  return(matrix(data = c(b1, b2, b3), nrow = 2, ncol=3, byrow = F, dimnames = list(c(),c('b1','b2','b3'))))
}

#-------------------------------------------------------------------------------------------------#
#' Title
#'
#' @param seg_geo 
#' @param tri_geo 
#'
#' @return
#' @export
#'
#' @examples
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