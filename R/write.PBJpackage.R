#' Title
#'
#' @param swdf
#' @param filename
#' @param seg_sort
#'
#' @return
#' @export
#'
#' @examples
write.PBJpackage <- function(swdf, filename, head_interp="GEO", seg_sort=T) {

  #-- Check for required columns
  if (ncol(swdf) < 12) {
    stop("Input data frame (swdf) contains too few columns. Please create using calc_stream_voronoi_weights")
  }
  if (!('Conductance' %in% colnames(swdf))) {
    stop("Input data frame (swdf) missing Conductance column. Please estimate using a calc_conductance_* function")
  }
  if (!('seg1.elev' %in% colnames(swdf)) & ('seg2.elev' %in% colnames(swdf))) {
    stop("Input data frame (swdf) missing elevation columns (seg1.elev, seg2.elev).")
  }


  #-- Order by Segments (if asked)
  if (seg_sort) {
    swdf <- swdf[order(swdf$Segment),]
  }

  #-- Open File
  f <- file(filename, "wt")

  #-- Write Comment Line
  writeLines("# Polyline Boundary Junction input file written by the MFUSGPBJ R Package", f)


  #-- Write Number of Segments
  #TODO Options
  writeLines(paste(nrow(swdf)), f)

  #-- Write Node Connections
  writeLines("INTERNAL  1.0  (FREE)  -1  Segment Nodes", f)
  write.table(wdf[,c('Node1','Node2','Node3')], f, row.names = F, col.names = F)

  #-- Write Barycentric Weights
  writeLines("INTERNAL  1.0  (FREE)  -1  Node Weights", f)
  write.table(wdf[,c('seg1.a1','seg1.a2','seg1.a3','seg2.a1','seg2.a2','seg2.a3')], f, row.names = F, col.names = F)

  #-- Write elevations
  writeLines("INTERNAL  1.0  (FREE)  -1  Segment Start/End Elevations", f)
  write.table(wdf[,c('seg1.elev','seg2.elev')], f, row.names = F, col.names = F)

  #-- Write conductances
  writeLines("INTERNAL  1.0  (FREE)  -1  Segment Conductance", f)
  write.table(wdf[,'Conductance'], f, row.names = F, col.names = F)

  #-- Close
  close(f)
}
