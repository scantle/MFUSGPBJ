#' fix_seg_elevs
#'
#' @param sdf
#' @param idcol
#' @param start_id
#' @param elev_col1
#' @param elev_col2
#' @param prev_segids
#' @param verbose
#'
#' @return
#' @export fix_seg_elevs
#'
#' @examples
fix_seg_elevs <- function(sdf, idcol, start_id, elev_col1='seg1.elev', elev_col2='seg2.elev', prev_segids=NULL, verbose=T) {

  prev_segids <- c(start_id, prev_segids)

  # Get "maximum" elevation of segment (hard to know if start or end is uphill, so we're assuming the higher one is uphill)
  maxelev <- max(sdf[sdf[idcol] == start_id, c(elev_col1, elev_col2)])

  # Find adjacent segment(s) that touch
  adj <- st_touches(sdf[sdf[idcol] == start_id,]$geometry, sdf$geometry)[[1]]

  # Ignore previous segments
  adj <- adj[!adj %in% prev_segids]

  if (verbose) { message(paste('SegID:',start_id,'| Adjacent:',length(adj),'| Done:',length(prev_segids))) }

  # Tree end condition
  if (length(adj) < 1) {
    if (verbose) { message(paste('* End found, SegID:',start_id)) }
  } else {

    # Loop over adjacent
    for (i in 1:length(adj)) {

      # Ensure adjacent ("uphill") segments are higher/level
      # If not, replace with current segment max
      if (sdf[sdf[idcol] == adj[i], elev_col1] < maxelev) {sdf[sdf[idcol] == adj[i], elev_col1] <- maxelev }
      if (sdf[sdf[idcol] == adj[i], elev_col2] < maxelev) {sdf[sdf[idcol] == adj[i], elev_col2] <- maxelev }

      # Make sure subsequent calls don't work on current adjacent segments (no dupes)
      cur_adj <- adj[adj != adj[i]]

      # Call this function on the adjacent segment ( R E C U R S I O N )
      # Pass it prev_segids with additional current adjacent segments
      sdf <- fix_seg_elevs(sdf, idcol, adj[i], elev_col1, elev_col2, c(prev_segids, cur_adj), verbose)
    }
  }

  # Return current version of SDF
  return(sdf)
}
