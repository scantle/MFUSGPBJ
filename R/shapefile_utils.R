#-------------------------------------------------------------------------------------------------#
#' Title
#'
#' @param polyline 
#'
#' @return
#' @export
#'
#' @examples
line_explode <- function(polyline) {
  line_points <- st_coordinates(polyline$geometry)
  line_list <- data.frame('ID'=1:(nrow(line_points)-1), 'geometry'=NA)
  for (i in 1:(nrow(line_points)-1)) {
    segment <- st_sfc(st_linestring(line_points[seq(i, i+1),c('X', 'Y')]))
    #line_list[i] <- table('ID'=i,'geometry'=segment[1])
    line_list[i,'geometry'] <- list(segment)
  }
  result <- st_sf(line_list, crs = st_crs(polyline))
  result$length <- st_length(result$geometry)
  return(result)
}
#-------------------------------------------------------------------------------------------------#

