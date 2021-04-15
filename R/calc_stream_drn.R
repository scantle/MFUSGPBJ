#' Calculate polyline (e.g. stream) drain locations
#'
#' Intended to provide a MODFLOW-USG Drain package that can be compared to the results of the
#' PBJ package, given the same grid and stream.
#'
#' @param stream sf polyline, "exploded" into segments (see \code{\link{line_explode}})
#' @param voronoi sf polygon of voronoi tesselation (unstructured model grid). Shapefile ID field
#' will be used to determine node ID.
#' @param method character, method to select drain nodes. Only overlap with [voronoi] nodes is
#' currently supported (optional, default: 'node')
#' @param seg_min_length numeric, minimum length of segment to include in calculation (default 1e-7).
#' Generally just to weed out numerical errors.
#'
#' @return SF Object/DataFrame of nodes that can be used to gener
#' @seealso \code{\link{write.DRNpackage}}
#' @author Leland Scantlebury
#' @export calc_stream_drn
#'
#' @examples
#' #-- Read in shapefiles
#' stream <- read_sf(system.file("extdata", "straight_river.shp", package = "pbjr"))
#' tri <- read_sf(system.file("extdata", "straight_triangles.shp", package = "pbjr"))
#' vor <- read_sf(system.file("extdata", "straight_voronoi.shp", package = "pbjr"))
#'
#' #-- Explode polyline
#' stream <- line_explode(stream)
#'
#' #-- Create DRNDF
#' drndf <- calc_stream_drn(stream = stream, voronoi = vor)
calc_stream_drn <- function(stream, voronoi, str_id_col='ID.1', vor_id_col='ID', method='node',
                            correct_seg_order=T, seg_min_length=1e-7, keep_stream_cols=NULL) {

  st_agr(voronoi) <- 'constant'  # Silence useless spatial consistency error
  st_agr(stream)  <- 'constant'

  if (method=='node') {
    # Get overlap by cell
    vor_stream <- st_intersection(voronoi,stream)

    #-- st_intersection can mess up segment order - it uses the triangle ID # to determine the order
    #-- This correction won't work for multiple streams - they must be sequential
    #TODO add support for multiple seperate lines (e.g., multiple streams)
    if (correct_seg_order) {
      vor_stream <- reorder_segments(stream, vor_stream)
    }

    # Coerce into small DF (to be comparable to output of calc_stream_voronoi_weights)
    drndf <- data.frame(Node = array(st_drop_geometry(vor_stream[,vor_id_col])),
                        Segment = array(st_drop_geometry(vor_stream[,str_id_col])),
                        Length = as.numeric(st_length(vor_stream)),
                        geometry = vor_stream$geometry)
    if (!is.null(keep_stream_cols)) {
      drndf[,keep_stream_cols] <- st_drop_geometry(vor_stream[,keep_stream_cols])
    }
  } else {
    stop('Unsupported or unimplemented method')
  }

  #-- Remove tiny segments
  drndf <- drndf[drndf$Length >= seg_min_length,]

  return(drndf)
}
