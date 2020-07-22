#-------------------------------------------------------------------------------------------------#
#' Title
#'
#' @param stream
#' @param voronoi
#' @param triangles
#' @param addTo
#' @param geometry
#' @param cutoff_value
#'
#' @return
#' @export
#'
#' @examples
calc_stream_voronoi_weights <- function(stream, voronoi, triangles, addTo=FALSE, geometry=T,
                                        cutoff_value=1e-8) {
  #-----------------------------------------------------------------------------------------------#
  #-- Get Intersections
  st_agr(triangles) <- 'constant'  # Silence useless spatial consistency error
  st_agr(stream)    <- 'constant'
  tri_stream <- st_intersection(triangles, stream)

  #-- Assumption: st_intersection preserves order (and stream order was correct originally)
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
  #-- Handle addTo if needed
  if (addTo != F) {
    weights <- rbind(addTo, weights,)
    #TODO LIkely could use some error handling
  }
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#
  #-- Reorder
  weights <- weights[order(weights$Order), ]
  rownames(weights) <- weights$Order
  #-- Drop, since order is now redundant with index
  weights <- weights[, -1]
  #-----------------------------------------------------------------------------------------------#

  return(weights)
}
#-------------------------------------------------------------------------------------------------#
