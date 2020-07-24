#' Writes Modflow-USG Polyline Boundary Junction Package
#'
#' @param swdf DataFrame/Geometry of node barycentric coordinates as returned by
#'   \code{\link{calc_stream_voronoi_weights}} with additional 'Conductance' and elevation
#'   ('seg1.elev' and 'seg2.elev') columns required
#' @param filename character, name/location of output file
#' @param seg_sort T/F (optional) whether swdf should be sorted by segment prior to output (default: True)
#'
#' @export
#'
#' @examples write.PBJpackage
#' #-- Read in shapefiles
#' str <- read_sf(system.file("extdata", "MehlandHill2010_stream.shp", package = "MFUSGPBJ"))
#' tri <- read_sf(system.file("extdata", "720_triangles.shp", package = "MFUSGPBJ"))
#' vor <- read_sf(system.file("extdata", "720_voronoi.shp", package = "MFUSGPBJ"))
#' str <- line_explode(str)
#'
#' #-- Calculate barycentric weight DF
#' swdf <- calc_stream_voronoi_weights(stream = str, voronoi = vor, triangles = tri)
#'
#' #-- Calculate distances
#' swdf <- stream_elev_from_slope(swdf = swdf, slope = 0.0015, initial_elev = 50)
#'
#' #-- Calculate conductances
#' swdf$Conductance <- calc_conductance_modflow(swdf, k_streambed = 1,
#'                                              str_width = 1, thickness = 0.5)
#' #-- Write package file
#' write.PBJpackage(swdf, filename = paste0(tempdir(),'/model720.pbj'))
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
  writeLines("INTERNAL  1    (FREE)  -1  Segment Nodes", f)
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
