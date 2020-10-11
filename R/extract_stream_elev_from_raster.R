#' Extract Stream Elevations from Raster (DEM)
#'
#' Used to get segment start/end elevations from a raster digital elevation model (DEM)
#'
#' @param swdf DataFrame/Geometry of node barycentric coordinates as returned by
#'   \code{\link{calc_stream_voronoi_weights}}
#' @param dem Raster object
#' @param method character, 'simple' or 'bilinear', passed to raster::extract()
#'
#' @return swdf DataFrame/Geometry with added seg1.elev and seg2.elev columns
#' @export extract_stream_elev_from_raster
#'
#' @examples
extract_stream_elev_from_raster <- function(swdf, dem, method='simple') {

  #-- Create DF of start & end points, in different columns for easy lookup
  seg_coords <- st_coordinates(swdf$geometry)

  #-- Get values from raster
  elevs <- extract(dem, seg_coords[,c('X','Y')], method=method)

  #-- Reformat, return
  seg_coords <- as.data.frame(seg_coords)
  seg_coords$elev <- elevs
  seg_coords$id <- ave(rep(1,length(seg_coords$L1)), seg_coords$L1, FUN = cumsum)
  seg_coords <- reshape(seg_coords, idvar = 'L1', timevar = 'id', direction='wide')
  swdf$seg1.elev <- seg_coords$elev.1
  swdf$seg2.elev <- seg_coords$elev.2

  return(swdf)
}
