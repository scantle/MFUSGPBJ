#' Writes Modflow-USG River Package
#'
#' @param rivdf DataFrame/Geometry of drain nodes as returned by \code{\link{calc_stream_drn}} with
#' conductances and elevation columns required for stress periods as 'cond[sp]', 'elev[sp]', and 'stage[sp]'
#' @param filename character, name/location of output file
#' @param nSPs integer, number of stress periods in simulation
#' @param IRIVCB integer, CBB flow flag. See details (or PBJ package manual) for more info
#' @param SPwarnings T/F (optional) turn on (True) or off (False) warnings about reused or missing SP data
#'
#' @author Leland Scantlebury
#' @seealso \code{\link{calc_stream_drn}} \code{\link{write.PBJpackage}}
#' @export write.RIVpackage
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
#' #-- Create rivdf
#' rivdf <- calc_stream_drn(stream = stream, voronoi = vor)
#'
#' #-- Add conductance
#' rivdf$cond1 <- calc_conductance_modflow(rivdf, k_streambed = 1,
#'                                         str_width = 1, thickness = 0.5)
#' #-- Add river bottom elevation
#' rivdf$elev1 <- 90.0
#'
#' #-- Add stage as 1 m above river bottom
#' rivdf$stage1 <- rivdf$elev1 + 1
#'
#' #-- Write file
#' write.DRNpackage(rivdf, filename = paste0(tempdir(),'/straight_model.drn'), nSPs=2, IRIVCB=50)
write.RIVpackage <- function(rivdf, filename, nSPs, IRIVCB, SPwarnings=T) {
  #-----------------------------------------------------------------------------------------------#
  #-- Check inputs (rivdf needs nodes, elevations, and conductances)
  if (!('elev1' %in% colnames(rivdf)) & ('seg2.elev' %in% colnames(rivdf))) {
    stop('At a minimum, an elevation must be specified for the first stress period (column "elev1")')
  }
  if (!('cond1' %in% colnames(rivdf))) {
    stop('At a minimum, a conductance must be specified for the first stress period (column "cond1")')
  }
  if (!('stage1' %in% colnames(rivdf))) {
    stop('At a minimum, a stage must be specified for the first stress period (column "stage1")')
  }
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#
  #-- Write!
  #-- Open File
  f <- file(filename, "wt")

  #-- Write Comment Line
  writeLines("# MODFLOW-USG River (RIV) package written by the MFUSGPBJ R Package", f)

  #-- Write Maximum Drains & IRIVCB
  writeLines(paste(nrow(rivdf), IRIVCB), f)

  #-- SPs
  for (sp in 1:nSPs) {
    #-- Check if conductivity values exist for this SP
    target_col <- paste0('cond', sp)
    if (target_col %in% colnames(rivdf)) {

      #-- Write out drains & number of parameters
      spriv <- rivdf[!is.na(rivdf[target_col]),]
      spriv$xyz <- 0
      nriv <- nrow(spriv)
      writeLines(paste(nriv, 0, '   ', 'Stress Period',sp), f)

      #-- Write out drain values
      write.table(format(spriv[,c('Node',
                                  paste0('stage',sp),
                                  paste0('cond',sp),
                                  paste0('elev',sp),
                                  'xyz')],
                         nsmall=c(0,4,4,4,0)),
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
