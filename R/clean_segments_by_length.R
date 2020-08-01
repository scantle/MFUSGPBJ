# Takes advantage of the fact that if it's a tiny piece it almost certainly came from
# a triangle corner, and triangle corners are completely in a cell.
# The segment it came from will have a start/end completely in that cell as well.
clean_segments_by_length <- function(swdf, min_seg_length=1e-4, forward_add=T, verbose=T) {

  #-- Get starting total length
  start_len <- sum(swdf$Length)

  #-- identify small segments
  small_segs <- as.numeric(rownames(swdf[swdf$Length < min_seg_length,]))

  addto <- 1
  if (!forward_add) {addto <- -1}

  if (verbose) {
    message(paste(length(small_segs), 'segments are shorter than the minimum length of ',min_seg_length))
  }

  for (iseg in small_segs) {

    #-- figure out which node the segment is weighted too
    start_node <- swdf[iseg, c("Node1", "Node2", "Node3")[max.col(swdf[iseg, c("seg1.a1", "seg1.a2", "seg1.a3")])]]
    end_node   <- swdf[iseg, c("Node1", "Node2", "Node3")[max.col(swdf[iseg, c("seg2.a1", "seg2.a2", "seg2.a3")])]]

    #-- Should be one node for both sides (because it should be tiny)
    if (end_node != start_node) {
      stop(paste('Segment', iseg, 'connects two nodes - min_seg_length may be too high'))
    }

    #-- Now it's our TARGET NODE
    tnode <- start_node

    #-- Find a connected piece in the direction specified
    nodedir <- addto
    if ((iseg + addto) %in% rownames(swdf)) {
      # There exists a segment in the direction requested
      start_node <- swdf[iseg + addto, c("Node1", "Node2", "Node3")[max.col(swdf[iseg + addto, c("seg1.a1", "seg1.a2", "seg1.a3")])]]
      end_node   <- swdf[iseg + addto, c("Node1", "Node2", "Node3")[max.col(swdf[iseg + addto, c("seg2.a1", "seg2.a2", "seg2.a3")])]]
    } else {
      nodedir <- -1 * addto
      if (verbose) {
        message(paste('- No more segments in direction specified - adding other direction for segment',iseg))
      }
      start_node <- swdf[iseg - addto, c("Node1", "Node2", "Node3")[max.col(swdf[iseg, c("seg1.a1", "seg1.a2", "seg1.a3")])]]
      end_node   <- swdf[iseg - addto, c("Node1", "Node2", "Node3")[max.col(swdf[iseg, c("seg2.a1", "seg2.a2", "seg2.a3")])]]
    }

    #-- If a piece that direction also attaches to that node, combine them
    if (start_node == tnode | end_node == tnode) {
      #-- Combine geometries
      newline <- st_line_merge(st_union(swdf[iseg, 'geometry'], swdf[iseg+nodedir, 'geometry']))
      swdf[iseg + nodedir,]$geometry <- newline

      #-- Correct Length
      swdf[iseg + nodedir, 'Length'] <- st_length(swdf[iseg + nodedir,]$geometry)
    } else {
      stop(paste('Segment', iseg, 'does not intersect following and/or preceding segment. Are segments out of order?'))
    }

  }

  #-- Drop short segments
  swdf <- swdf[swdf$Length >= min_seg_length,]

  if (verbose) {
    message(paste('Cleaning successful. Original length =',start_len, '| New length =', sum(swdf$Length)))
  }

  return(swdf)
}
