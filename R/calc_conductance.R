#-------------------------------------------------------------------------------------------------#
#' Title
#'
#' @param swdf
#' @param k_streambed
#' @param str_width
#' @param stream_thick
#'
#' @return
#' @export
#'
#' @examples
calc_conductance_modflow <- function(swdf, k_streambed, str_width, thickness) {
  cond <- (k_streambed * swdf$Length * str_width) / thickness
  return(cond)
}
#-------------------------------------------------------------------------------------------------#
