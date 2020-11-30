#' Title
#'
#' @param stream
#' @param mfgrid
#' @param correct_seg_order
#'
#' @return
#' @export calc_stream_nodes
#'
#' @examples
calc_stream_nodes <- function(stream, mfgrid, correct_seg_order=T) {
  #-----------------------------------------------------------------------------------------------#
  #-- Get Intersections
  # Silence useless spatial consistency error
  st_agr(stream)    <- 'constant'
  st_agr(mfgrid)   <- 'constant'
  grid_stream <- st_intersection(mfgrid, stream)

  #-- st_intersection can mess up segment order - it uses the grid ID # to determine the order
  #-- This correction won't work for multiple streams - they must be sequential
  #TODO add support for multiple seperate lines (e.g., multiple streams)
  if (correct_seg_order) {
    grid_stream <- reorder_segments(stream, grid_stream)
  }

  #-- Track order
  grid_stream$Order <- 1:nrow(grid_stream)

  #-- Update lengths
  grid_stream$Length <- as.double(st_length(grid_stream))

  #-- Return
  return(grid_stream)

  #-----------------------------------------------------------------------------------------------#
}
