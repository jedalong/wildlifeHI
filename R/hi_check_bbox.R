# ---- roxygen documentation ----
#
#' @title Compare Bounding Boxes by trackId
#'
#' @description
#'  This function is a a checking tool to investigate the bounding boxes of each \code{trackId} within a \code{move2} object.
#'
#' @details
#'  This is a simple tool to look at the bounding boxes of each individual within a \code{move2} object. This is useful to explore how large different bounding boxes are prior to performing queries for OSM data using \code{hi_get_osm}. This is often useful when the overall study area is large, but the bounding boxes of individual trajectories are relatively small in comparison. 
#'
#' @param move an object of the class \code{move2}. For more information on objects of this type see \code{help(move2)}.
#'
#' @return
#'  This function returns a \code{sf} POLYGON object containing the bounding boxes for each individual \code{trackId}within the tracking data. 
#'
#' @examples
#' library(move)
#' library(move2)
#' data(fishers)
#' fishers2 <- mt_as_move2(fishers)
#' fishers_bb <- hi_check_bbox(fishers2) 
#' 
#' \dontrun{
#' library(ggplot2)
#' ggplot() + geom_sf(data=fishers_bb, aes(color = track)) + coord_sf()
#' }
#' 
#' 
#' @export
#
# ---- End of roxygen documentation ----


hi_check_bbox <- function(move){
  
  #check if move2
  if(!inherits(move, "move2")){
    print('Input data not of class move2. Returning NULL.')
    return(NULL)
  }
  
  #get unique ids and col names
  ids <- unique(move2::mt_track_id(move))
  col <- move2::mt_track_id_column(move)
  bbdf <- data.frame()
  sf_all <- NULL
  data_crs <- sf::st_crs(move)
  
  for (id in ids){
    #move2 is an sf object so select as such
    m1 <- move[move[[col]] == id,]
    bb <- sf::st_bbox(m1)
    
    bbsf <- sf::st_as_sfc(bb)
    a <- units::set_units(sf::st_area(bbsf),'ha')
    x <- c(as.numeric(bb),as.numeric(a))
    bbdf <- rbind(bbdf,x)
    
    sf_all <- c(sf_all,bbsf)
  }
  
  bbdf <- cbind(ids,bbdf)
  names(bbdf) <- c('trackId','xmin','ymin','xmax','ymax','bb_ha')
  
  sf_all <- sf::st_as_sf(bbdf, sf_all, crs=data_crs) 
  
  print(bbdf)
  return(sf_all)
  
}
