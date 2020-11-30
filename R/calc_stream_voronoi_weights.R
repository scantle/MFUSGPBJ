#-------------------------------------------------------------------------------------------------#
#' Calculate polyline (e.g. stream) barycentric coordinates
#'
#' These coordinates are used as "weights" in the PBJ MODFLOW-USG package to interpolate heads and
#' distribute flows.
#'
#' The function can take a while to run
#'
#' @param stream sf polyline, "exploded" into segments (see \code{\link{line_explode}})
#' @param voronoi sf polygon of voronoi tesselation (unstructured model grid). Shapefile ID field
#' will be used to determine node ID.
#' @param triangles sf polygon of delaunay triangulation corresponding to voronoi grid.
#' @param addTo (optional) existing calc_stream_voronoi_weights() output new output should be added
#' to (False by default)
#' @param geometry (optional) T/F whether to include sf geometry in output dataframe (default: True)
#' @param correct_seg_order (optional) T/F to re-order the line segments after finding overlaps with
#' the triangle grid. Will crash if you have multiple seperate lines (e.g. two streams). (default:
#' True)
#' @param cutoff_value numeric, minimum barcentric coordinate value. Values below will be forced to
#' zero (1e-7 by default)
#' @param seg_min_length numeric, minimum length of segment to include in calculation (default 1e-7).
#' Generally just to weed out numerical errors.
#' @param keep_stream_cols character array, columns in stream segment dataframe to add to returned
#' dataframe.
#' @return DataFrame or sf object, if geometry = True. Each row is one segment-triangle overlap,
#' with six barycentric weights (three for segment end), the three voronoi shape IDs (model nodes)
#' connected by the triangle, and the segment length in the triangle.
#'
#' This the expected input of \code{\link{stream_elev_from_slope}} and
#' the \code{calc_conductance*} functions (e.g. \code{\link{calc_conductance_modflow}})
#' @author Leland Scantlebury
#' @export calc_stream_voronoi_weights
#'
#' @examples
#' #-- Read in shapefiles
#' str <- read_sf(system.file("extdata", "MehlandHill2010_stream.shp", package = "MFUSGPBJ"))
#' tri <- read_sf(system.file("extdata", "720_triangles.shp", package = "MFUSGPBJ"))
#' vor <- read_sf(system.file("extdata", "720_voronoi.shp", package = "MFUSGPBJ"))
#'
#' #-- Explode polyline
#' str <- line_explode(str)
#'
#' #-- Run the function
#' swdf <- calc_stream_voronoi_weights(stream = str, voronoi = vor, triangles = tri)
#'
#' #-- Example of addTo use (more likely run with new stream shapefile)
#' more_swdf <- calc_stream_voronoi_weights(stream = str, voronoi = vor, triangles = tri,
#'                                          addTo = swdf)
calc_stream_voronoi_weights <- function(stream, voronoi, triangles, addTo=NULL, geometry=T,
                                        correct_seg_order=T,
                                        cutoff_value=1e-7, seg_min_length=1e-7,
                                        keep_stream_cols=NULL) {
  #-----------------------------------------------------------------------------------------------#
  #-- Get Intersections
  st_agr(triangles) <- 'constant'  # Silence useless spatial consistency error
  st_agr(stream)    <- 'constant'
  st_agr(voronoi)   <- 'constant'
  tri_stream <- st_intersection(triangles, stream)

  #-- Remove segments below length threshold
  tri_stream <- tri_stream[as.numeric(st_length(tri_stream)) > seg_min_length,]

  #-- st_intersection can mess up segment order - it uses the triangle ID # to determine the order
  #-- This correction won't work for multiple streams - they must be sequential
  #TODO add support for multiple seperate lines (e.g., multiple streams)
  if (correct_seg_order) {
    tri_stream <- reorder_segments(stream, tri_stream)
  }

  tri_stream$Order <- 1:nrow(tri_stream)
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#
  # Extract segment triangles, keep order
  seg_triangles <- merge(st_drop_geometry(tri_stream[,c('Order','ID')]), triangles, by = 'ID')
  seg_triangles <- seg_triangles[order(seg_triangles$Order),]
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#
  #-- Report
  message(paste('Calculating barycentric coords for',nrow(tri_stream),'triangle-stream segment combinations'))
  #message('May take a bit...')
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#

  #-- Get Barycentric Coords
  bary_coords <- geo_to_barycentric_coords(segments = tri_stream, seg_triangles = seg_triangles)

  #-- Zero small coordinates, re-normalize
  bary_coords[bary_coords < cutoff_value] <- 0.0
  bary_coords[,1:3] <- t(apply(bary_coords[,1:3], 1, function(x)(x/sum(x))))
  bary_coords[,4:6] <- t(apply(bary_coords[,4:6], 1, function(x)(x/sum(x))))

  #-- Would love a simpler way to get voronoi-triangle corner mapping
  tri_corners <- triangle_corners(seg_triangles)

  for (p in 2:4) {
    pgeo <- st_as_sf(tri_corners[,p])
    pgeo$tID <- tri_corners$ID
    st_agr(pgeo) <- 'constant'
    st_crs(pgeo) <- st_crs(voronoi)
    pgeo$uniqueID <- 1:nrow(pgeo)
    vor_overlap <- st_intersection(voronoi,pgeo)

    #-- Check - sometimes a triangle point (usually at a border) is attached to multiple voronoi cells.
    #   POSSIBLY can be corrected for by checking if extra (duplicate) voronoi cells even intersect with relevant triangles, stream
    if (nrow(tri_corners) < nrow(vor_overlap)) {
      message('One of more triangle points intersect multiple voronoi cells. Attempting to correct...')
      vor_overlap <- point_multi_voronoi_intersect_fixer(tri_corners, vor_overlap, voronoi, triangles, stream)
    }

    #-- Move Vornoi IDs over to new Node column
    tri_corners <- cbind(tri_corners, vor_overlap$ID)
    names(tri_corners)[length(names(tri_corners))] <- paste0('Node',p-1)
  }

  #-- Assemble output
  weights <- data.frame('Order'=tri_stream$Order,
                        'Triangle'=tri_stream$ID,
                        'Segment'=st_drop_geometry(tri_stream[,length(names(tri))]),
                        'Length'=as.numeric(st_length(tri_stream)),  # Ignore "units" class, trust users
                        'Node1'=tri_corners$Node1,
                        'Node2'=tri_corners$Node2,
                        'Node3'=tri_corners$Node3,
                        'seg1.a1'=bary_coords[,1],
                        'seg1.a2'=bary_coords[,2],
                        'seg1.a3'=bary_coords[,3],
                        'seg2.a1'=bary_coords[,4],
                        'seg2.a2'=bary_coords[,5],
                        'seg2.a3'=bary_coords[,6], row.names = NULL)
  if (geometry) {
    weights$geometry <- tri_stream$geometry
  }
  if (!is.null(keep_stream_cols)) {
    weights <- cbind(weights, st_drop_geometry(tri_stream[, keep_stream_cols]))
  }

  #-----------------------------------------------------------------------------------------------#
  #-- Handle addTo if needed
  if (!is.null(addTo)) {
    weights <- rbind(addTo, weights)
    #TODO LIkely could use some error handling
  }
  #-----------------------------------------------------------------------------------------------#
  return(weights)
}

#-------------------------------------------------------------------------------------------------#

point_multi_voronoi_intersect_fixer <- function(tri_corners, vor_overlap, voronoi, triangles, stream) {
  # Create "super" unique column
  vor_overlap$superID <- 1:nrow(vor_overlap)

  # Find points that came from identical point intersections
  dupes <- vor_overlap[vor_overlap$uniqueID %in% vor_overlap[duplicated(vor_overlap$uniqueID),]$uniqueID,]

  # Remove cells without river cells
  vor_overlap_cells <- voronoi[voronoi$ID %in% vor_overlap$ID,]
  st_agr(vor_overlap_cells) <- 'constant'

  #-- Test one -  Remove cells that do not overlap the triangles in question
  vor_overlap_cells <- vor_overlap_cells[sapply(st_overlaps(vor_overlap_cells,triangles[triangles$ID %in% dupes$tID,]), length) > 0 ,]

  # Remove from dupes the cells that overlap a triangle
  dupes <- dupes[!dupes$ID %in% vor_overlap_cells$ID,]
  # Now, remove dupes from vor_overlap that are also in this list
  #test <- vor_overlap[!((vor_overlap$uniqueID %in% dupes$uniqueID)&(vor_overlap$ID %in% dupes$ID)),]
  test <- vor_overlap[!(vor_overlap$superID %in% dupes$superID),]

  # Did we do it??
  if (nrow(tri_corners) == nrow(test)) {
    # This one doesn't really need a warning
    return(test)
  }
  # Darn. Start over:
  # Find points that came from identical point intersections
  dupes <- vor_overlap[vor_overlap$uniqueID %in% vor_overlap[duplicated(vor_overlap$uniqueID),]$uniqueID,]
  # Remove cells without river cells
  vor_overlap_cells <- voronoi[voronoi$ID %in% vor_overlap$ID,]
  st_agr(vor_overlap_cells) <- 'constant'

  #-- Test two - Remove cells that do not intersect any part of the stream (can remove too many!)
  vor_overlap_cells <- vor_overlap_cells[sapply(st_intersects(vor_overlap_cells,stream), length) > 0 ,]

  # Remove from dupes the cells that contain a stream segment
  dupes <- dupes[!dupes$ID %in% vor_overlap_cells$ID,]
  # Now, remove dupes from vor_overlap that are also in this list
  #test <- vor_overlap[!((vor_overlap$uniqueID %in% dupes$uniqueID)&(vor_overlap$ID %in% dupes$ID)),]
  test <- vor_overlap[!(vor_overlap$superID %in% dupes$superID),]

  # Did we do it??
  if (nrow(tri_corners) == nrow(test)) {
    warning('Removed voronoi cell(s) with no stream segments that non-uniquely intersected with triangle point(s)')
    return(test)
  }

  # Fine. We'll do this by triangle, choosing just by area

  # Find points that came from identical point intersections
  dupes <- vor_overlap[vor_overlap$uniqueID %in% vor_overlap[duplicated(vor_overlap$uniqueID),]$uniqueID,]

  vor_overlap_cells <- voronoi[voronoi$ID %in% vor_overlap$ID,]
  st_agr(vor_overlap_cells) <- 'constant'

  result <- lapply(unique(dupes$tID), function(triID)  {
    vocs <- vor_overlap_cells[sapply(st_overlaps(vor_overlap_cells,triangles[triangles$ID==triID,]), length) > 0 ,]
    if (any(!unique(dupes[dupes$tID==triID,]$ID) %in% vocs$ID)) {
      # first test actually worked - but there were different answers for different triangles
      res <- dupes[(dupes$tID==triID)&(!dupes$ID %in% vocs$ID),]
      return(res)
    }
    # Find out which was cell has less intersection with the triangle
    vor_intersect_cells <- st_intersection(vor_overlap_cells,triangles[triangles$ID==triID,])
    vor_intersect_cells$inter_area <- st_area(vor_intersect_cells)
    res <- dupes[(dupes$tID==triID)&(dupes$ID != vor_intersect_cells[which.max(vor_intersect_cells$inter_area),]$ID),]
    return(res)
  })
  dupes <- do.call(rbind, result)

  #test <- vor_overlap[!((vor_overlap$uniqueID %in% dupes$uniqueID)&(vor_overlap$ID %in% dupes$ID)),]
  test <- vor_overlap[!(vor_overlap$superID %in% dupes$superID),]

  # I'm pretty sure that will do it. I suppose if ANOTHER try was needed we could just (with no tests) drop duplicates.
  if (nrow(tri_corners) == nrow(test)) {
    warning('Non-unique triangle point to voronoi cell relationships - largest intersecting cell used when necessary')
    return(test)
  }

  stop('Unable to rectify non-unique triangle point to voronoi relationship(s)')
}
