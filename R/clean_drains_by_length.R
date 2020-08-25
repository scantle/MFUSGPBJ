#' Clean Drain Segments by Length & Combine Co-located Segments
#'
#'
#' @param drndf Output from \code{\link{calc_stream_drn}}. Must be ordered and contain geometry.
#' @param min_seg_length Minimum allowed segment length (default: 1e-4)
#' @param verbose T/F if information should be relayed through messages (default: True).
#'
#' @return
#' @export clean_drains
#'
#' @examples
#' #-- Read in shapefiles
#' str <- read_sf(system.file("extdata", "straight_river.shp", package = "MFUSGPBJ"))
#' vor <- read_sf(system.file("extdata", "straight_voronoi.shp", package = "MFUSGPBJ"))
#'
#' #-- Explode polyline
#' str <- line_explode(str)
#'
#' #-- Create drndf
#' drndf <- calc_stream_drn(stream = stream, voronoi = vor)
#'
#' #-- Clean up
#' drndf <- clean_drains(drndf, min_seg_length=0.5)
clean_drains <- function(drndf, min_seg_length=1e-4, verbose=T) {

  #-- Get starting total length & nrow
  start_len <- sum(drndf$Length)
  start_rws <- nrow(drndf)

  #-- Find "duplicate" nodes
  dupes <- unique(d$Node[duplicated(d$Node)])

  if (verbose) {
    message(paste(length(dupes), 'drains are co-located in the same node'))
    if (length(dupes) > 0) {message('Reparing...')}
  }

  #-- Loop over duplicates
  for (dupnode in dupes) {
    #-- Add segments together, length weighted average for elevation
    #-- get indeces
    isegs <- as.numeric(row.names(drndf[drndf$Node == dupnode,]))

    #-- Calc new elevation
    newelev <- calc_weighted_avg(drndf[isegs,]$elev1, drndf[isegs,]$Length)

    #-- Combine geometries
    #newline <- st_line_merge(st_union(drndf[isegs[1], 'geometry'], drndf[isegs[2], 'geometry']))
    newline <- st_combine(drndf[isegs,'geometry'])
    drndf[isegs[1],]$geometry <- newline

    #-- Change Elev & Length
    drndf[isegs[1],'elev1'] <- newelev
    drndf[isegs[1],'Length'] <- as.numeric(st_length(drndf[isegs[1],]$geometry))

    #-- Drop second node
    #LS Move to after all is processed to avoid issues  drndf <- drndf[row.names(drndf) != isegs[2],]
    #-- Instead, render line useless
    drndf[isegs[-1],'Node'] <- -999
  }

  #-- Drop duplicates, identified by -999 in Node
  drndf <- drndf[drndf$Node > 0,]

  #-- identify small segments
  small_segs <- rownames(drndf[drndf$Length <= min_seg_length,])

  if (verbose) {
    message(paste(length(small_segs), 'drains are shorter than the minimum length of ',min_seg_length))
    if (length(dupes) > 0) {message('Reparing...')}
  }

  #-- Loop over small segments
  for (seg in small_segs) {
    #-- Find closest to add to
    for (i in 1:(nrow(drndf)/2)){
      sumrow <- as.character(as.numeric(seg) + i)
      if (sumrow %in% rownames(drndf)) break
      sumrow <- as.character(as.numeric(seg) - i)
      if (sumrow %in% rownames(drndf)) break
    }

    #-- Calc new elevation
    newelev <- calc_weighted_avg(drndf[c(sumrow, seg),]$elev1, drndf[c(sumrow, seg),]$Length)

    #-- Combine Geometries
    newline <- st_line_merge(st_union(drndf[sumrow, 'geometry'], drndf[seg, 'geometry']))
    len2 <- st_length(drndf[sumrow, 'geometry'])
    drndf[sumrow,]$geometry <- newline

    #-- Correct Elevation & Length
    drndf[sumrow,'elev1'] <- newelev

    drndf[sumrow,'Length'] <- as.numeric(st_length(drndf[sumrow,]$geometry))
  }

  #-- Drop small segments
  drndf <- drndf[drndf$Length > min_seg_length,]

  if (verbose) {
    message(paste('Cleaning successful. Original length =',start_len, '| New length =', sum(drndf$Length)))
    message(paste('Starting nrows =',start_rws,'| Final nrows =',nrow(drndf)))
  }

  return(drndf)
}

calc_weighted_avg <- function(values, weights) {
  return(sum(values*weights)/sum(weights))
}
