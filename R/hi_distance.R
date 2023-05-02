# ---- roxygen documentation ----
#
#' @title Distance analysis to human infrastructure
#'
#' @description
#'  This function calculates the distance to nearest piece of human infrastructure.
#'
#' @details
#'  This tool simply calculates the distance from every tracking fix to the nearest piece of human infrastructure.
#'  The default is to use any linear road/trail feature defined in the 'highway' key, but any OSM feature can be 
#'  specified. See \code{?hi_get_osm}.
#'
#' @param move an object of the class \code{move}. For more information on objects of this type see \code{
#'         help(move)}.
#' @param osmdata an \code{sf} object containing human infrastructure data formatted similar to OSM data. 
#' See \code{?hi_get_osm}.
#' @param value return; one of 'move' (default) or 'sf'. whether to return a \code{move} object or an \code{sf} point object. 
#' @param ... additional parameters passed to \code{hi_get_osm}
#'
#' @return
#'  This function returns either a \code{move} or a \code{sf} object containing the original tracking data with three additional columns:
#'  - nearest_key: the key of the nearest feature in \code{osmdata}.
#'  - nearest_value: the value (type) of the nearest feature in \code{osmdata}.
#'  - nearest_distance: the distance to the nearest feature in \code{osmdata}.
#'
#' @examples
#' data(fishers)
#' fishers_d <- hi_distance(fishers)
#' ## boxplot(nearest_distance ~ nearest_value, fishers_d)
#' 
#' fishers_b <- hi_distance(fishers,key='building',geom='polygon')
#' ## boxplot(nearest_distance ~ nearest_value, fishers_b)
#' 
#' @export
#
# ---- End of roxygen documentation ----


hi_distance <- function(move,osmdata,return='move',...){
  
  if (missing(osmdata)){
    osmdata <- hi_get_osm(move, ...)
  }
  
  sf_pt <- st_as_sf(move)
  sf_pt$trackId <- move@trackId
  
  #distance to features
  nearest <- st_nearest_feature(sf_pt, osmdata)
  
  #AASSUME 'OSM-like' data with columns named "key" and "value"
  if (return == 'move'){
    move$nearest_key <- st_drop_geometry(osmdata)[nearest,'key']
    move$nearest_value <- st_drop_geometry(osmdata)[nearest,'value']
    move$nearest_distance <- st_distance(sf_pt,osmdata[nearest,],by_element=TRUE)
    return(move)
  } else {
    sf_pt$nearest_key <- st_drop_geometry(osmdata)[nearest,'key']
    sf_pt$nearest_value <- st_drop_geometry(osmdata)[nearest,'value']
    sf_pt$nearest_distance <- st_distance(sf_pt,osmdata[nearest,],by_element=TRUE)
    return(sf_pt)
  }
}