#' Writes Modflow-USG Polyline Boundary Junction Package
#'
#' @param swdf DataFrame/Geometry of node barycentric coordinates as returned by
#'   \code{\link{calc_stream_voronoi_weights}} with conductances and elevation columns required for segment
#'   start/ends (e.g. 'seg2.elev' & 'seg1.elev'; 'seg1.cond[sp]' and'seg2.cond[sp]')
#'   Conductance columns are used regardless of condtype (e.g. Leakance Coefficients for stress periods should
#'   be passed through Conductance columns). Stress period numbers should suffix all time varying column numbers,
#'   unless constant. See details for more info.
#' @param filename character, name/location of output file
#' @param nSPs integer, number of stress periods in simulation
#' @param IPBJCB integer, CBB flow flag. See details (or PBJ package manual) for more info
#' @param pbjmode character, PBJ package mode. See details (or PBJ package manual) for more info.
#'   One of "HEADSPEC", "DRAIN", or "EXTSTAGE". Default: 'DRAIN'
#' @param condtype character, conductance type to be used. See details (or PBJ package manual) for more info
#'   One of "CONDUCTANCE", "UNITCOND", or "LEAKCOEF". Default: 'UNITCOND'.
#' @param seg_sort T/F (optional) whether swdf should be sorted by segment prior to output (default: True)
#' @param SPwarnings T/F (optional) turn on (True) or off (False) warnings about reused or missing SP data
#' @param allowconst T/F (optional) allow SP arrays to be written using the CONSTANT flag (default: True)
#'
#' @details
#' Attention is required in the naming of the time-varying columns in relation to stress periods. Conductance,
#' 'Head', and 'Stage' columns must be passed with specific stress period numbers after their name
#' (e.g. 'Head5'). Missing stress periods (e.g. 'Stage4' followed by 'Stage6') are assumed to mean the
#' previous stress period value should be reused.
#'
#' Conductances (leakance coefficents, etc) are defined at both the segment start and end (like elevations)
#' by the columns 'seg1.cond[sp]' and'seg2.cond[sp]'.
#'
#' NAs in any time-variant column will be treated that segment is not active in the stress period. Consistency
#' with other SP parameters is NOT be checked. When using the external head-dependent boundary option (EXTSTAGE)
#' this means you'll want to make sure Stage and Conductance are inactive (NAs) for the same stress periods.
#'
#' If IPBJCB is > 0, cell-by-cell flow terms will be written to this unit number when “SAVE BUDGET” or a
#' non-zero value for ICBCFL is specified in the output control
#' If IPBJCB = 0, cell-by-cell flow terms are not written
#' If IPBJCB < 0, segment leakance for each segment will be written to the listing file when “SAVE BUDGET”
#' or a non-zero value for ICBCFL is specified in the output control
#'
#' \strong{Mode} specifies if the segments should act as a:
#' \itemize{
#' \item HEADSPEC: specified head boundary
#' \item DRAIN: drain (head-dependent, active only above elevation)
#' \item EXTSTAGE: external head-dependent flux boundary (e.g. stream elevation time series)
#' }
#'
#' \strong{Condtype} (conductance type) specifies if the stress period segment conductances are input as:
#' \itemize{
#' \item CONDUCTANCE: conductances [L2/T]
#' \item UNITCOND: conductances per unit length [L/T]
#' \item LEAKCOEF: leakance coefficients (1/T).}
#' The conductance type is ignored (not needed) if the mode is set to head-specified.
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
#' swdf$seg1.cond1 <- calc_conductance_modflow(swdf, k_streambed = 1,
#'                                             str_width = 1, thickness = 0.5)
#' swdf$seg2.cond1 <- calc_conductance_modflow(swdf, k_streambed = 1,
#'                                             str_width = 1, thickness = 0.5)
#' #-- Write package file
#' write.PBJpackage(swdf, filename = paste0(tempdir(),'/model720.pbj'), nSPs=2, IPBJCB=50)
write.PBJpackage <- function(swdf, filename, nSPs, IPBJCB, pbjmode='DRAIN', condtype='UNITCOND',
                             seg_sort=T, SPwarnings=T, allowconst=T) {

  #-----------------------------------------------------------------------------------------------#
  #-- INPUT ERROR HANDLING

  #-- Check for required columns
  if (ncol(swdf) < 12) {
    stop("Input data frame (swdf) contains too few columns. Please create using calc_stream_voronoi_weights")
  }
  if (!('seg1.elev' %in% colnames(swdf)) & ('seg2.elev' %in% colnames(swdf))) {
    stop("Input data frame (swdf) missing elevation columns (seg1.elev, seg2.elev).")
  }

  #-- Check settings strings are valid
  if (!all.equal(IPBJCB, as.integer(IPBJCB))) {
    stop("Non-integer CBC flow flag (IPBJCB). Must be a unit number, zero, or negative integer")
  }
  if (!(pbjmode %in% c("HEADSPEC", "DRAIN", "EXTSTAGE"))) {
    stop("Invalid PBJ mode (pbjmode).")
  }
  if (!(condtype %in% c("CONDUCTANCE", "UNITCOND", "LEAKCOEF"))) {
    stop("Invalid conductance type (condtype).")
  }

  #-- Check valid columns, given settings
  if(pbjmode == "HEADSPEC") {
    if (!('Head1' %in% colnames(swdf))) {
      stop('At a minimum, a conductance must be specified for the first stress period (column "Head1")')
    }
  } else {
    # All other modes require a conductance of some sort
    if (!('seg1.cond1' %in% colnames(swdf))) {
      stop('At a minimum, a conductance must be specified for the first stress period (column "seg1.cond1")
           Please estimate using a calc_conductance_* function')
    }
    if (!('seg2.cond1' %in% colnames(swdf))) {
      stop('At a minimum, a conductance must be specified for the first stress period (column "seg2.cond1")
           Please estimate using a calc_conductance_* function')
    }
    if (pbjmode == "EXTSTAGE") {
      if (!('Stage1' %in% colnames(swdf))) {
        stop('At a minimum, a conductance must be specified for the first stress period (column "Stage1")')
      }
    }
  }
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#
  #-- FILE WRITING

  #-- Order by Segments (if asked)
  if (seg_sort) {
    swdf <- swdf[order(swdf$Segment),]
  }

  #-- Open File
  f <- file(filename, "wt")

  #-- Write Comment Line
  writeLines("# Polyline Boundary Junction input file written by the MFUSGPBJ R Package", f)

  #-- Write Number of Segments & IPBJCB
  writeLines(paste(nrow(swdf), IPBJCB), f)

  #-- Write Mode & Condtype
  writeLines(paste(pbjmode, condtype), f)

  #-- Write Node Connections
  writeLines("INTERNAL  1    (FREE)  -1  Segment Nodes", f)
  write.table(swdf[,c('Node1','Node2','Node3')], f, row.names = F, col.names = F)

  #-- Write Barycentric Weights
  writeLines("INTERNAL  1.0  (FREE)  -1  Node Weights", f)
  write.table(format(swdf[,c('seg1.a1','seg1.a2','seg1.a3','seg2.a1','seg2.a2','seg2.a3')], digits=10),
              f, row.names = F, col.names = F, quote = F)

  #-- Write elevations
  writeLines("INTERNAL  1.0  (FREE)  -1  Segment Start/End Elevations", f)
  write.table(swdf[,c('seg1.elev','seg2.elev')], f, row.names = F, col.names = F)

  #-- Write Lengths
  if (condtype != 'CONDUCTANCE') {
    writeLines("INTERNAL  1.0  (FREE)  -1  Segment Lengths", f)
    write(swdf$Length, f, ncolumns = 10)
  }

  #-- Write Widths
  #TODO Implement this
  if (condtype == 'LEAKCOEF') {
    writeLines("INTERNAL  1.0  (FREE)  -1  Segment Widths", f)
    write.table(swdf[,c('seg1.width','seg2.width')], f, row.names = F, col.names = F)
  }

  #-- Write time-variant parameters
  for (sp in 1:nSPs) {
    if (pbjmode == "HEADSPEC") {
      pbj_write_sp_values(f, swdf, sp, 'Head', 'Specified Heads', SPwarnings, allowconst)
    } else {
      #-- Stage
      if (pbjmode == 'EXTSTAGE') {
        pbj_write_sp_values(f, swdf, sp, 'Stage', 'External Stage', SPwarnings, allowconst)
      }

      #-- Conductances
      if (condtype == "CONDUCTANCE") {
        pbj_write_sp_values(f, swdf, sp, c('seg1.cond','seg2.cond'), 'Conductances', SPwarnings, allowconst)
      } else if (condtype == 'UNITCOND') {
        pbj_write_sp_values(f, swdf, sp, c('seg1.cond','seg2.cond'), 'Unit Length Conductances', SPwarnings, allowconst)
      } else if (condtype == 'LEAKCOEF') {
        pbj_write_sp_values(f, swdf, sp, c('seg1.cond','seg2.cond'), 'Leakance Coefficients', SPwarnings, allowconst)
      }
    }
  }

  #-----------------------------------------------------------------------------------------------#

  #-- Close
  close(f)
}

pbj_write_sp_values <- function(f, swdf, sp, colstr, valuestr, SPwarnings, allowconst) {

  target_col <- paste0(colstr, sp)

  if (all(target_col %in% colnames(swdf))) {
    nsegments <- nrow(swdf[all(!is.na(swdf[target_col])),])
    writeLines(paste(nsegments, '   ', valuestr, 'Stress Period',sp), f)

    #-- Check if allowed to write as constant, check if constant
    #-- This is so messy - almost needs to be it's own function
    is_const <- T
    const_vals <- c()
    if (allowconst) {
      if (length(target_col)==1) {
        if (nrow(unique(swdf[target_col])) != 1) {
          is_const <- F
        } else {
          const_vals <- c(const_vals, unique(swdf[[target_col]]))
        }
      } else {
        for (i in 1:length(target_col)) {
          if (nrow(unique(swdf[target_col[i]])) != 1) {
            is_const <- F
            break
          } else {
            const_vals <- c(const_vals, unique(swdf[[target_col[i]]]))
          }
        }
      }
      #-- If constant
      if (is_const) {
        writeLines(paste('CONSTANT ', paste(format(const_vals, nsmall=4), collapse = ' ')), f)
      }
    } else {is_const <- F}

    if (is_const == F) {
      #-- Write Values as table, one segment per line
      write.table(format(swdf[all(!is.na(swdf[target_col])), target_col], nsmall=4),
                  f, row.names = T, col.names = F, quote = FALSE)
    }

  } else {

    #-- Reusing
    if (SPwarnings) {
      warning(paste0('No data for ',valuestr,' in SP ',sp,'. Preceding SP values will be reused.'))
    }
    writeLines(paste(-1, '   ', valuestr, 'Stress Period',sp), f)
  }
}
