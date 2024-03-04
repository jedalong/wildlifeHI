# ---- roxygen documentation ----
#
#' @title Analyze movement relative to buffers around human infrastructure features
#'
#' @description
#'  This function calculates when a trajectory enters, remains within, and exits a buffer around human infrastructure features.
#'
#' @details
#'  This tool computes when a trajectory enters, remains within, and exits a buffer around human infrastructure. It can be used to identify movement in proximity to features. When simply interested in the distance to features use \code{hi_distance}. The default is to use any linear road/trail feature defined in the 'highway' key, but any OSM feature can be specified. See \code{?hi_get_osm()}.
#'
#' @param move an object of the class \code{move2}. For more information on objects of this type see \code{help(move2)}.
#' @param r distance (in appropriate units) to buffer the human infrastructure data.
#' @param osmdata an \code{sf} object containing human infrastructure data formatted similar to OSM data. See \code{?hi_get_osm}.
#' @param crs_code (optional) a CRS code to "project" data prior to performing buffer analysis. Can be used to speed up the processing substantially due to \code{sf} using different libraries for geoprocessing projected vs geographic coordinate systems.
#' @param return one of 'move' (default) or 'buffer'. Default is to return a \code{move2} object of the trajectory. If return = 'buffer' a POLYGON with the buffer is returned. 
#' @param ... additional parameters passed to \code{hi_get_osm}

#' @return This function returns by default a \code{move2} object containing the tracking data with one additional column, \code{buf_code}, with the following levels:
#'  - buf_code = 'enters' - movement from outside to inside the buffer zone
#'  - buf_code = 'within' - movement within the buffer zone
#'  - buf_code = 'exists' - movement from inside to outside the buffer zone
#'  - buf_code = 'cross in' - movement starts/ends inside the buffer zone, but crosses outside the buffer zone
#'  - buf_code = 'cross out - movement starts/ends outside the buffer zone, but crosses the buffer zone
#'  - buf_code = NA - movement does not go inside the buffer zone
#'  
#'  Alternatively, this function will return the buffer as an \code{sf} POLYGON object.
#'  
#' @examples
#' \dontrun{
#' library(move)
#' library(move2)
#' data(fishers)
#' fishers2 <- mt_as_move2(fishers)
#' fishers_buf <- hi_buffer(fishers2,r=50,crs_code=32618)
#' 
#' library(ggplot2)
#' ggplot() + geom_sf(data=fishers_buf, aes(color = buf_code)) + coord_sf()
#' 
#' buf_50 <- hi_buffer(fishers2,r=50,crs_code=32618,return="buffer")
#' 
#' ggplot() + geom_sf(data=fishers_buf, aes(color = track)) + geom_sf(data=buf_50) + coord_sf()
#' }
#' 
#' @export
#
# ---- End of roxygen documentation ----

hi_buffer <- function(move,r=100,osmdata,crs_code,return='move',...){
  
  #check if move2
  if(!inherits(move, "move2")){
    print('Input data not of class move2. Returning NULL.')
    return(NULL)
  }
  
  #save original CRS
  data_crs <- sf::st_crs(move)
  
  # Get OSM data
  if (missing(osmdata)){
    osmdata <- hi_get_osm(move, ...)
  } 
  
  #If OSM data is Null return Move object
  if (is.null(osmdata)){
    move$buf_code <- NA
    return(move)
  }
  
  #Use a projected coordinate system if specified - MAKES BUFFER WORK WAY BETTER
  if (missing(crs_code)){ crs_code = data_crs }
  osmdata <- sf::st_transform(osmdata,crs=crs_code)
  
  #Create buffer
  buf <- sf::st_buffer(osmdata,r) |>
    sf::st_union()
  
  if (return == "buffer"){
    buf <- sf::st_transform(buf,data_crs)
    return(buf)
  }
  
  # Create linestrings need to fix to do by ID
  sf_ln <- internal_hi_move2line(move) |> 
    sf::st_transform(crs=crs_code)
  
  ## There is something about ordering that really matters in terms of these functions:
  ## https://github.com/r-spatial/sf/issues/1261
  #with <- st_within(sf_ln,buf,sparse=FALSE) #THIS IS SLOW!!
  with <- sf::st_contains(buf,sf_ln,sparse=FALSE)
  #into <- st_intersects(sf_ln,buf,sparse=FALSE) #This is slow also
  into <- sf::st_intersects(sf_ln,buf,sparse=FALSE)
  
  buf_code <- rep(NA,nrow(sf_ln))
  buf_code[into] <- 'intersects'
  buf_code[with] <- 'within'
  i_int <- which(buf_code == 'intersects')
  
  if (length(i_int) > 0){
    suppressWarnings(ln_pt <- sf::st_cast(sf_ln[i_int,],'POINT'))
    pt_int <- sf::st_intersects(buf,ln_pt,sparse=FALSE)
    
    for (i in 1:length(i_int)){
      z <- i_int[i]
      j <- 2*i-1
      k <- 2*i
      if (pt_int[j] == TRUE){
        if (pt_int[k] == TRUE){
          buf_code[z] <- 'cross_in'
        } else {
          buf_code[z] <- 'exit'
        }
      } else {
        if (pt_int[k] == TRUE){
          buf_code[z] <- 'enter'
        } else {
          buf_code[z] <- 'cross_out'
        }
      }
    }
  }
  
  sf_ln$buf_code <- buf_code
  
  ### Add buf code to move object
  indo <- which(move2::mt_track_id(move) != dplyr::lag(move2::mt_track_id(move)))
  for (j in indo){
    buf_code <- append(buf_code,NA,after=(j-1))
  }
  move$buf_code <- c(buf_code,NA)
  
  #Return "all" not noted in documentation but used in Move APp
  if (return == 'all'){
    sf_ln <- sf::st_transform(sf_ln,data_crs)
    buf <- sf::st_transform(buf,data_crs)
    ret_list <- list(move,sf_ln,buf)
    return(ret_list)
  } else {
    return(move)
  } 
}
