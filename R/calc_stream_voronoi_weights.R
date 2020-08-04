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
                                        cutoff_value=1e-7, seg_min_length=1e-7) {
  #-----------------------------------------------------------------------------------------------#
  #-- Get Intersections
  st_agr(triangles) <- 'constant'  # Silence useless spatial consistency error
  st_agr(stream)    <- 'constant'
  tri_stream <- st_intersection(triangles, stream)

  #-- Remove segments below length threshold
  tri_stream <- tri_stream[st_length(tri_stream) > seg_min_length,]

  #-- st_intersection can mess up segment order - it uses the triangle ID # to determine the order
  #-- This correction won't work for multiple streams - they must be sequential
  #TODO add support for multiple seperate lines (e.g., multiple streams)
  if (correct_seg_order) {
    tri_stream <- reorder_segments(stream, tri_stream)
  }

  tri_stream$order <- 1:nrow(tri_stream)
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#
  #-- Report
  message(paste('Calculating barycentric coords for',nrow(tri_stream),'triangle-stream segment combinations'))
  message('May take a bit...')
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#
  #-- Loop over stream triangles
  trilist <- split(tri_stream, tri_stream$ID)
  trilist <- lapply(trilist, function(itri) {
    #-- Loop over stream segments in triangle
    seglist <- split(itri, itri$ID.1)
    seglist <- lapply(seglist, function(seg) {

      #-- Get Barycentric Coords
      bary_coords <- geo_to_barycentric_coords(seg_geo = seg$geometry, tri_geo = triangles[triangles$ID == seg$ID,]$geometry)

      #-- Get voronoi IDs based on centers (should perfectly align to triangle sides)
      #tri_corner <- as.data.frame(unique(st_coordinates(triangles[triangles$ID == seg$ID,])[,c('X','Y')]))
      #tri_corner$order <- 1:3

      #-- Would love a simpler way to do this
      tri_corners <- st_as_sf(as.data.frame(st_coordinates(triangles[triangles$ID == seg$ID,])),
                              coords = c('X','Y'), crs=st_crs(voronoi))
      st_agr(voronoi)     <- 'constant'  # Silence useless spatial consistency error
      st_agr(tri_corners) <- 'constant'
      vor_overlap <- st_intersection(tri_corners, voronoi)
      vor_overlap <- vor_overlap[order(row.names(vor_overlap)), ] # Preserves original order (hopefully!)

      #vor_overlap <- st_join(triangles[triangles$ID == seg$ID,],voronoi)
      #tri_corner <- merge(tri_corner, vor_overlap[c('CentreX.y','CentreY.y','ID.y')],
      #                    by.x=c('X','Y'), by.y=c('CentreX.y','CentreY.y'))
      #tri_corner <- tri_corner[order(tri_corner$order),]  # To preserve order of corners, needed to match barycentric coords

      #-- Zero small coordinates, re-normalize
      bary_coords[bary_coords < cutoff_value] <- 0.0
      bary_coords[1,] <- bary_coords[1,] / sum(bary_coords[1,])
      bary_coords[2,] <- bary_coords[2,] / sum(bary_coords[2,])


      #-- Produce final data frame (single row)
      out <- data.frame('Order'=seg$order,
                        'Triangle'=seg$ID,
                        'Segment'=seg$ID.1,
                        'Length'=as.numeric(st_length(seg)),  # Ignore "units" class, trust users
                        'Node1'=vor_overlap[1,]$ID,
                        'Node2'=vor_overlap[2,]$ID,
                        'Node3'=vor_overlap[3,]$ID,
                        'seg1.a1'=bary_coords[1,'b1'],
                        'seg1.a2'=bary_coords[1,'b2'],
                        'seg1.a3'=bary_coords[1,'b3'],
                        'seg2.a1'=bary_coords[2,'b1'],
                        'seg2.a2'=bary_coords[2,'b2'],
                        'seg2.a3'=bary_coords[2,'b3'], row.names = NULL)
      if (geometry) {
        out$geometry <- seg$geometry
      }
      return(out)
    })
    #-- Combine all segments back into one dataframe
    test <- do.call(rbind, seglist)
    #return(do.call(rbind, seglist))
  })
  #-- Combine all Triangles back into one dataframe
  weights <- do.call(rbind, trilist)
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#
  #-- Reorder
  weights <- weights[order(weights$Order), ]
  rownames(weights) <- weights$Order
  #-- Drop, since order is now redundant with index
  weights <- weights[, -1]
  #-----------------------------------------------------------------------------------------------#

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
