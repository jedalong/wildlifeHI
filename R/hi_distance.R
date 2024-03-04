# ---- roxygen documentation ----
#
#' @title Distance analysis to human infrastructure
#'
#' @description
#'  This function calculates the distance to nearest piece of human infrastructure.
#'
#' @details
#'  This tool simply calculates the distance from every tracking fix to the nearest piece of human infrastructure. The default is to use any linear road/trail feature defined in the 'highway' key, but any OSM feature can be specified. See \code{?hi_get_osm}.
#'
#' @param move an object of the class \code{move2}. For more information on objects of this type see \code{help(move2)}.
#' @param osmdata an \code{sf} object containing human infrastructure data formatted similar to OSM data. See \code{?hi_get_osm}.
#' @param ... additional parameters passed to \code{hi_get_osm}
#'
#' @return
#'  This function returns either a \code{move2} object containing the original tracking data with three additional columns:
#'  - nearest_key: the key of the nearest feature in \code{osmdata}.
#'  - nearest_value: the value (type) of the nearest feature in \code{osmdata}.
#'  - nearest_distance: the distance to the nearest feature in \code{osmdata}.
#'
#' @examples
#' \dontrun{\
#' library(move)
#' library(move2)
#' data(fishers)
#' fishers2 <- mt_as_move2(fishers)
#' fishers_d <- hi_distance(fishers2)
#' ## boxplot(nearest_distance ~ nearest_value, fishers_d)
#' 
#' fishers_b <- hi_distance(fishers2,key='building',geom='polygon')
#' ## boxplot(nearest_distance ~ nearest_value, fishers_b)
#' }
#' 
#' @export
#
# ---- End of roxygen documentation ----


hi_distance <- function(move,osmdata,...){
  
  #check if move2
  if(!inherits(move, "move2")){
    print('Input data not of class move2. Returning NULL.')
    return(NULL)
  }
  
  if (missing(osmdata)){
    osmdata <- hi_get_osm(move, ...)
  }
  
  move$nearest_key <- NA
  move$nearest_value <- NA
  move$nearest_distance <- NA
  
  if (!is.null(osmdata)){
    
    #preserve move for end
    sf_pt <- move
    move2::mt_track_id(sf_pt) <- NULL
    sf_pt <- sf::st_as_sf(sf_pt)
    
    #distance to features
    nearest <- sf::st_nearest_feature(sf_pt, osmdata)
    
    #ASSUME 'OSM-like' data with columns named "key" and "value"
    move$nearest_key <- sf::st_drop_geometry(osmdata)[nearest,'key']
    move$nearest_value <- sf::st_drop_geometry(osmdata)[nearest,'value']
    move$nearest_distance <- sf::st_distance(sf_pt,osmdata[nearest,],by_element=TRUE)
  }
  

  return(move)
}