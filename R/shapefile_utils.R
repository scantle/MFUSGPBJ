#-------------------------------------------------------------------------------------------------#
#' Convert single polyline into multipolyline segments, based on vertices
#' Or "explode" the line, as it is commonly referred to in GIS programs
#'
#' Warning: This function is NOT robust. Poorly-ordered shapefiles can easily confused it!
#' Plot the resulting object to ensure it has connected the lines properly. GIS programs
#' (e.g., QGIS) have much more robust explode processes that will do much better job.
#'
#' @param polyline sf polyline object
#'
#' @return sf multipolyline object
#' @author Leland Scantlebury
#' @export line_explode
#'
#' @examples
#' #-- Read in shapefiles
#' str <- read_sf(system.file("extdata", "MehlandHill2010_stream.shp", package = "pbjr"))
#'
#' #-- Explode polyline
#' str <- line_explode(str)
line_explode <- function(polyline) {
  #-- Get points
  line_points <- st_coordinates(polyline$geometry)
  line_list <- data.frame('ID'=1:(nrow(line_points)-1), 'geometry'=NA)

  #-- Make lines based upon sets of points
  for (i in 1:(nrow(line_points)-1)) {
    segment <- st_sfc(st_linestring(line_points[seq(i, i+1),c('X', 'Y')]))
    line_list[i,'geometry'] <- list(segment)
  }
  #-- Put together into one "multi" polyline
  result <- st_sf(line_list, crs = st_crs(polyline))
  result$length <- st_length(result$geometry)
  return(result)
}
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#' Reorder line segments after intersection
#'
#' @param src original line sf polyline object
#' @param parts intersected sf polygon object
#'
#' @return Re-ordered version of parts
#' @export reorder_segments
#' @details
#' Very briefly modificied from Spacedman's solution on StackExchange:
#' \url{https://gis.stackexchange.com/questions/295806/r-turn-off-automatic-ordering-of-linestrings-when-applying-sfst-intersection}
#'
#' @examples
#' #-- Read in shapefiles
#' str <- read_sf(system.file("extdata", "MehlandHill2010_stream.shp", package = "pbjr"))
#' tri <- read_sf(system.file("extdata", "720_triangles.shp", package = "pbjr"))
#'
#' # Intersect
#' tri_stream <- st_intersection(tri, str)
#'
#' #-- st_intersection can mess up segment order - it uses the triangle ID # to determine the order
#' tri_stream <- reorder_segments(str, tri_stream)
reorder_segments <- function(src, parts){

  joints = st_intersection(parts)
  joints = joints[st_is(joints,"POINT"),]
  jgraph = igraph::graph_from_edgelist(do.call(rbind, joints$origins), directed=FALSE)

  ends = which(igraph::degree(jgraph) == 1)

  sps = igraph::shortest_paths(jgraph, ends[1], ends[2])

  path = sps$vpath[[1]]
  return(parts[as.numeric(path),])
}
