#' Writes Modflow-USG Drain Package
#'
#' @param drndf DataFrame/Geometry of drain nodes as returned by \code{\link{calc_stream_drn}} with
#' conductances and elevation columns required for stress periods as 'cond[sp]' and 'elev[sp]'
#' @param filename character, name/location of output file
#' @param nSPs integer, number of stress periods in simulation
#' @param IPBJCB integer, CBB flow flag. See details (or PBJ package manual) for more info
#' @param SPwarnings T/F (optional) turn on (True) or off (False) warnings about reused or missing SP data
#'
#' @author Leland Scantlebury
#' @seealso \code{\link{calc_stream_drn}} \code{\link{write.PBJpackage}}
#' @export write.DRNpackage
#'
#' @examples
#' #-- Read in shapefiles
#' stream <- read_sf(system.file("extdata", "straight_river.shp", package = "MFUSGPBJ"))
#' tri <- read_sf(system.file("extdata", "straight_triangles.shp", package = "MFUSGPBJ"))
#' vor <- read_sf(system.file("extdata", "straight_voronoi.shp", package = "MFUSGPBJ"))
#'
#' #-- Explode polyline
#' stream <- line_explode(stream)
#'
#' #-- Create DRNDF
#' drndf <- calc_stream_drn(stream = stream, voronoi = vor)
#'
#' #-- Add conductance
#' drndf$cond1 <- calc_conductance_modflow(drndf, k_streambed = 1,
#'                                         str_width = 1, thickness = 0.5)
#' #-- Add elevation
#' drndf$elev1 <- 90.0
#'
#' #-- Write file
#' write.DRNpackage(drndf, filename = paste0(tempdir(),'/straight_model.drn'), nSPs=2, IPBJCB=50)
write.DRNpackage <- function(drndf, filename, nSPs, IPBJCB, SPwarnings=T) {
  #-----------------------------------------------------------------------------------------------#
  #-- Check inputs (drndf needs nodes, elevations, and conductances)
  if (!('elev1' %in% colnames(drndf)) & ('seg2.elev' %in% colnames(drndf))) {
    stop('At a minimum, an elevation must be specified for the first stress period (column "cond1")')
  }
  if (!('cond1' %in% colnames(drndf))) {
    stop('At a minimum, a conductance must be specified for the first stress period (column "cond1")')
  }
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#
  #-- Write!
  #-- Open File
  f <- file(filename, "wt")

  #-- Write Comment Line
  writeLines("# MODFLOW-USG Drain (DRN) package written by the MFUSGPBJ R Package", f)

  #-- Write Maximum Drains & IPBJCB
  writeLines(paste(nrow(drndf), IPBJCB), f)

  #-- SPs
  for (sp in 1:nSPs) {
    #-- Check if conductivity values exist for this SP
    target_col <- paste0('cond', sp)
    if (target_col %in% colnames(drndf)) {

      #-- Write out drains & number of parameters
      spdrn <- drndf[!is.na(drndf[target_col]),]
      spdrn$xyz <- 0
      ndrns <- nrow(spdrn)
      writeLines(paste(ndrns, 0, '   ', 'Stress Period',sp), f)

      #-- Write out drain values
      write.table(format(spdrn[,c('Node',
                           paste0('elev',sp),
                           paste0('cond',sp),
                           'xyz')], nsmall = 4),
                  sep='  ',
                  file = f,
                  row.names = F,
                  col.names = F,
                  quote = F)
    } else {
      #-- Repeat previous SP values
      writeLines(paste(-1, 0, '   ', 'Stress Period',sp), f)
      if (SPwarnings) {
        warning(paste0('No conductance for SP ',sp,'. Preceding SP values will be reused.'))
      }
    }
  }

  close(f)
  #-----------------------------------------------------------------------------------------------#
}
