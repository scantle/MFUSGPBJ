#' Title
#'
#' @param segnodedf
#' @param filename
#' @param nSPs
#' @param IPBJCB
#' @param pbjmode
#' @param condtype
#' @param NodeCol
#' @param SPwarnings
#'
#' @return
#' @export write.PBJpackage_single
#'
#' @examples
write.PBJpackage_single <- function(segnodedf, filename, nSPs, IPBJCB, pbjmode='DRAIN', condtype='UNITCOND',
                             NodeCol='node', SPwarnings=T) {

  #-----------------------------------------------------------------------------------------------#
  #-- INPUT ERROR HANDLING

  if (!('elev_a1' %in% colnames(segnodedf)) & ('elev_b1' %in% colnames(segnodedf))) {
    stop("Input data frame (segnodedf) missing elevation columns (elev_a1, elev_b1).")
  }

  #-- Check settings strings are valid
  if (!all.equal(IPBJCB, as.integer(IPBJCB))) {
    stop("Non-integer CBC flow flag (IPBJCB). Must be a unit number, zero, or negative integer")
  }
  if (!(pbjmode %in% c("DRAIN", "EXTSTAGE"))) {
    stop("Invalid PBJ mode (pbjmode).")
  }
  if (!(condtype %in% c("CONDUCTANCE", "UNITCOND", "LEAKCOEF"))) {
    stop("Invalid conductance type (condtype).")
  }

  #-- Check valid columns, given settings
  # All modes require a conductance of some sort
  if (!('cond_a1' %in% colnames(segnodedf))) {
    stop('At a minimum, a conductance must be specified for the first stress period (column "cond_a1")
         Please estimate using a calc_conductance_* function')
  }
  if (!('cond_b1' %in% colnames(segnodedf))) {
    stop('At a minimum, a conductance must be specified for the first stress period (column "cond_b1")
         Please estimate using a calc_conductance_* function')
  }
  if (pbjmode == "EXTSTAGE") {
    if (!('stage_a1' %in% colnames(segnodedf))) {
      stop('At a minimum, a stage must be specified for the first stress period (column "stage_a1")')
    }
    if (!('stage_b1' %in% colnames(segnodedf))) {
      stop('At a minimum, a stage must be specified for the first stress period (column "stage_b1")')
    }
  }
  #-----------------------------------------------------------------------------------------------#

  #-----------------------------------------------------------------------------------------------#
  #-- FILE WRITING

  #-- Open File
  f <- file(filename, "wt")

  #-- Write Comment Line
  writeLines("# Polyline Boundary Junction input file written by the MFUSGPBJ R Package", f)

  #-- Write Number of Segments & IPBJCB
  writeLines(paste(nrow(segnodedf), IPBJCB), f)

  #-- Write Mode & Condtype
  writeLines(paste(pbjmode, condtype), f)

  #-- Write Segments for each SP
  for (sp in 1:nSPs) {

    #-- Assemble columns
    sp_cols <- c(NodeCol,  paste0(c('elev_a','elev_b','cond_a','cond_b'),sp))
    if (condtype != "CONDUCTANCE") {
      sp_cols <- c(sp_cols, 'length')
    }
    if (pbjmode == 'EXTSTAGE') {
      sp_cols <- c(sp_cols, paste0(c('stage_a','stage_b'),sp))
    }

    if (sp == 1) {
      nsegments <- nrow(segnodedf[all(!is.na(segnodedf[,sp_cols])),])
      writeLines(paste(nsegments, '   SEGMENTS', '     #  Stress Period',sp), f)
      real_cols <- ncol(segnodedf[,sp_cols])-1
      outdf <- as.data.frame(st_drop_geometry(segnodedf[,sp_cols]))
      outdf[,NodeCol] <- format(outdf[,NodeCol], width=8)
      outdf[,sp_cols[2:real_cols]] <- format(outdf[,sp_cols[2:real_cols]], width=12)
      write.table(outdf, f, row.names = F, col.names = F, quote = FALSE)
    } else {
      # Subsequent SPs can be repeats. ALL must be present, otherwise previous SP is used
      if (all(sp_cols %in% colnames(segnodedf))) {
        nsegments <- nrow(segnodedf[all(!is.na(segnodedf[,sp_cols])),])
        writeLines(paste(nsegments, '   SEGMENTS', '     #  Stress Period',sp), f)
        real_cols <- ncol(segnodedf[,sp_cols])-1
        outdf <- as.data.frame(st_drop_geometry(segnodedf[,sp_cols]))
        outdf[,NodeCol] <- format(outdf[,NodeCol], width=8)
        outdf[,sp_cols[2:real_cols]] <- format(outdf[,sp_cols[2:real_cols]], width=12)
        write.table(outdf, f, row.names = F, col.names = F, quote = FALSE)
      } else {
        #-- Reusing
        if (SPwarnings) {
          message(paste0('Missing 1 (or more) columns for SP ',sp,'. Preceding SP values will be reused.'))
        }
        writeLines(paste(-1, '   REP_PREV', '     #  Stress Period',sp), f)
      }
    }
  }

  #-----------------------------------------------------------------------------------------------#

  #-- Close
  close(f)
}
