# ---- roxygen documentation ----
#
#' @title Compare Bounding Boxes by trackId
#'
#' @description
#'  This function is a a checking tool to investigate the bounding boxes of each \code{trackId} within a \code{MoveStack}.
#'
#' @details
#'  This is a simple tool to look at the bounding boxes of each individual within a \code{MoveStack}. This is useful to explore how large different bounding boxes are prior to performing queries for OSM data using \code{hi_get_osm}. This is often useful when the overall study area is large, but the bounding boxes of individual trajectories are relatively small in comparison. 
#'
#' @param move an object of the class \code{move}. For more information on objects of this type see \code{help(move)}.
#'
#' @return
#'  This function returns a \code{sf} POLYGON object containing the bounding boxes for each individual \code{trackId}within the tracking data. 
#'
#' @examples
#' library(move)
#' data(fishers)
#' fishers_bb <- hi_check_bbox(fishers) 
#' 
#' \dontrun{
#' library(mapview)
#' mapview(fishers_bb['trackId'])
#' }
#' 
#' 
#' @export
#
# ---- End of roxygen documentation ----



hi_check_bbox <- function(move){
  
  tz <- attr(timestamps(move),'tzone')
  #check input data type
  if (!inherits(move,'MoveStack')){
    if (inherits(move,'Move')){
      move <- moveStack(move, forceTz=tz) #fix this timestamp to correct time zone
    } else {
      print('Input Data not of class MoveStack. Returning NULL.')
      return(NULL)
    }
  }
  
  
  ids <- unique(trackId(move))
  bbdf <- data.frame()
  sf_all <- NULL
  data_crs <- st_crs(move)
  
  for (id in ids){
    m1 <- move[[id]]
    bb <- st_bbox(m1)
    
    bbsf <- st_as_sfc(bb)
    a <- set_units(st_area(bbsf),'ha')
    x <- c(as.numeric(bb),as.numeric(a))
    bbdf <- rbind(bbdf,x)
    
    sf_all <- c(sf_all,bbsf)
  }
  
  bbdf <- cbind(ids,bbdf)
  names(bbdf) <- c('trackId','xmin','ymin','xmax','ymax','bb_ha')
  
  sf_all <- st_as_sf(bbdf, sf_all, crs=data_crs) 
  
  print(bbdf)
  return(sf_all)
  
}
