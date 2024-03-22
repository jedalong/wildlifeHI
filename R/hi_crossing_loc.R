# ---- roxygen documentation ----
#
#' @title Get locations of crossings of human infrastructure
#'
#' @description
#'  This function calculates where a trajectory crosses a linear feature of human infrastructure.
#'
#' @details
#'  This tool simply calculates where crossings occurs from tracking fixes in relation to the input human infrastructure (assumed to be linear features). The default is to use any linear road/trail feature defined in the 'highway' key, but any OSM feature can be specified. See \code{?hi_get_osm()}.
#'
#' @param move an object of the class \code{move}. For more information on objects of this type see \code{help(move)}.
#' @param osmdata an \code{sf} object containing human infrastructure data formatted similar to OSM data. See \code{?hi_get_osm}.
#' @param crs_code (optional) a CRS code to "project" data prior to performing intersection analysis. Can be used to speed up the processing substantially due to \code{sf} using different libraries for geoprocessing.
#' @param ... additional parameters passed to \code{hi_get_osm}
#'
#' @return
#'  This function returns a \code{sf} POINT object containing the locations where a trajectory crosses linear features (based on connecting consecutive GPS fixes)
#'
#' @examples
#' \dontrun{
#' library(move)
#' library(move2)
#' data(fishers)
#' fishers2 <- mt_as_move2(fishers)
#' ## x1 <- hi_crossing_loc(fishers2)  #Takes 2270 seconds
#' x2 <- hi_crossing_loc(fishers2,crs_code=32618)  ## takes 12 seconds
#' 
#' ggplot() + geom_sf(data=x2, aes(color = value)) + coord_sf()
#' }
#' 
#' @export
#
# ---- End of roxygen documentation ----

hi_crossing_loc <- function(move,osmdata,crs_code,...){
  
  #check if move2
  if(!inherits(move, "move2")){
    print('Input data not of class move2. Returning NULL.')
    return(NULL)
  }
  
  #GET OSM Data and return Null if none exists.
  if (missing(osmdata)){
    osmdata <- hi_get_osm(move, ...)
  }
  if (is.null(osmdata)){ return(NULL)}
  
  #grab projection of data
  data_crs <- sf::st_crs(move)
  
  #Use a projected coordinate system if specified - MAKES INTERSECTION WORK WAY BETTER
  if (missing(crs_code)){ crs_code = data_crs }
  osmdata <- sf::st_transform(osmdata,crs=crs_code)
  
  # Create linestrings
  sf_ln <- internal_hi_move2line(move) |> 
    sf::st_transform(crs=crs_code)
  
  #get locations of crossings (lines/poly boundaries)
  #Check reverse ordering if slow...
  suppressWarnings(sf_int <- sf::st_intersection(osmdata,sf_ln))
  
  sf_int <- sf::st_transform(sf_int,data_crs)
  return(sf_int)
}
