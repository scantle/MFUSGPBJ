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
#' str <- read_sf(system.file("extdata", "straight_river.shp", package = "MFUSGPBJ"))
#' tri <- read_sf(system.file("extdata", "straight_triangles.shp", package = "MFUSGPBJ"))
#' vor <- read_sf(system.file("extdata", "straight_voronoi.shp", package = "MFUSGPBJ"))
#'
#' #-- Explode polyline
#' str <- line_explode(str)
#'
#' #-- Create DRNDF
#' drndf <- calc_stream_drn(stream = str, voronoi = vor)
calc_stream_drn <- function(stream, voronoi, method='node', seg_min_length=1e-7) {

  st_agr(voronoi) <- 'constant'  # Silence useless spatial consistency error
  st_agr(stream)  <- 'constant'

  if (method=='node') {
    # Get overlap by cell
    vor_stream <- st_intersection(voronoi,stream)

    # Coerce into small DF (to be comparable to output of calc_stream_voronoi_weights)
    drndf <- data.frame('Node' = vor_stream$ID,
                        'Segment' = vor_stream$ID.1,
                        'Length' = st_length(vor_stream),
                        'geometry' = vor_stream$geometry)
  } else {
    stop('Unsupported or unimplemented method')
  }

  #-- Remove tiny segments
  drndf <- drndf[drndf$Length >= seg_min_length,]

  return(drndf)
}
