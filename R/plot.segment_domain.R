#-------------------------------------------------------------------------------------------------#
#' Title
#'
#' @param triangles
#' @param voronoi
#' @param stream
#' @param seg_id
#' @param tri_id
#' @param trim_seg
#' @param title
#'
#' @return
#' @export
#'
#' @examples
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
    seg <- st_intersection(triangles, stream[stream$ID == seg_id,])
    plot(seg[seg$ID == tri_id,]$geometry, col='orange', lwd=3, add=T)
  } else {
    plot(stream[stream$ID == seg_id,]$geometry, col='orange', lwd=3, add=T)
  }

  #-- Title
  if (title) {
    title(paste("Triangle:",seg$ID,"Segment:",seg$ID.1))
  }
}
#-------------------------------------------------------------------------------------------------#
