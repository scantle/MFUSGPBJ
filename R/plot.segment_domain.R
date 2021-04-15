#-------------------------------------------------------------------------------------------------#
#' Create plot of segment
#' Useful for evaluating & examining results of \code{\link{calc_stream_voronoi_weights}}
#'
#' @param triangles sf polygon of delaunay triangulation corresponding to voronoi grid.
#' @param voronoi sf polygon of voronoi tesselation (unstructured model grid).
#' @param stream sf polyline, "exploded" into segments (see \code{\link{line_explode}})
#' @param seg_id integer, id of stream segment to be highlighted in plot
#' @param tri_id integer, id of triangle segment highlighted in plot
#' @param trim_seg (optional) whether segment should be trimmed to triangle extent (as is done in
#' \code{\link{calc_stream_voronoi_weights}}) Default: True
#' @param title (optional) T/F whether a title should be added to the plot identifying
#' Triangle/Segment IDs
#'
#' @export plot.segment_domain
#'
#' @examples
#' #-- Read in shapefiles
#' str <- read_sf(system.file("extdata", "MehlandHill2010_stream.shp", package = "pbjr"))
#' tri <- read_sf(system.file("extdata", "720_triangles.shp", package = "pbjr"))
#' vor <- read_sf(system.file("extdata", "720_voronoi.shp", package = "pbjr"))
#'
#' #-- Plot
#' plot.segment_domain(tri, vor, str, seg_id = 34, tri_id = 451, title = T)
plot.segment_domain <- function(triangles, voronoi, stream, seg_id, tri_id, trim_seg=T, title=F) {
  plot(triangles$geometry, border='grey80')
  plot(voronoi$geometry, border='grey50', add=T)
  #plot(voronoi[voronoi$ID %in% tri_vor_overlap[tri_vor_overlap$ID == seg$ID,]$ID.1,]$geometry, border = 'green', add=T)
  #plot(tri_vor_overlap[tri_vor_overlap$ID == seg$ID,]$geometry, col='green', add=T)
  plot(triangles[triangles$ID == tri_id,]$geometry, col=rgb(30,144,255,max=255,alpha=100), border=F, add=T)
  plot(stream$geometry, col='dodgerblue', lwd=2, add=T)
  if (trim_seg) {
    st_agr(triangles) = "constant"  # To avoid warnings
    st_agr(stream) = "constant"
    seg <- st_intersection(triangles[triangles$ID == tri_id,], stream[stream$ID == seg_id,])
    plot(seg[seg$ID == tri_id,]$geometry, col='orange', lwd=3, add=T)
  } else {
    plot(stream[stream$ID == seg_id,]$geometry, col='orange', lwd=3, add=T)
  }

  #-- Title
  if (title) {
    title(paste("Triangle:",tri_id,"Segment:",seg_id))
  }
}
#-------------------------------------------------------------------------------------------------#
