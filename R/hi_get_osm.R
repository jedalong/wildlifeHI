# ---- roxygen documentation ----
#
#' @title Get OSM data for Move object
#'
#' @description
#'  This function is a simple wrapper around the core functions of the \code{osmdata} package.
#'
#' @details
#'  This function is normally used internally to get OSM data for a specified tracking dataset, but can also be called directly to return the OSM data as an \code{sf} object. The parameters can be specified to choose which OSM features are returned. The default is to return all 'highway' (i.e., road/trail) line segments, but any OSM feature can be queried. For more information see: https://wiki.openstreetmap.org/wiki/Map_features
#'
#' @param move an object of the class \code{move2}. For more information on objects of this type see \code{help(move2)}.
#' @param key string; OSM key string. Default is 'highway'. (see details and \code{?add_osm_feature} from the osmdata package)
#' @param value string; OSM value strings for specified key. Default is all values for that key. (see details and \code{?add_osm_feature} from the \code{osmdata} package).
#' @param bbox user specified bbox. Default is bbox of input \code{move2} object +/- 10 percent.
#' @param geom string; the geometry type to return ('point', 'line', 'polygon' or combination thereof). Default is 'line'.
#' @param poly2line logical (default TRUE);  whether to convert polygon geometry to lines, which is useful in a variety of situations for example due to loops in many linear features, but also to look at border crossings.
#'
#' @return
#'  This function returns an \code{sf} object containing OSM data. If the OSM query times out, a note is printed on the screen and the function returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#' library(move)
#' library(move2)
#' data(fishers)
#' fishers2 <- mt_as_move2(fishers)
#' osmdata <- hi_get_osm(fishers2)
#' osmdata_railway <- hi_get_osm(fishers2,key='railway')
#' }
#' 
#' @export
#
# ---- End of roxygen documentation ----


hi_get_osm <- function(move,key='highway',value,bbox,geom="line",poly2line=TRUE){
  
  #check if move2
  if(!inherits(move, "move2")){
    print('Input data not of class move2. Returning NULL.')
    return(NULL)
  }
  
  if (missing(bbox)){
    bbox <- sf::st_bbox(move)
    x10 <- (bbox$xmax - bbox$xmin)*0.1
    y10 <- (bbox$ymax - bbox$ymin)*0.1
    bbox <- bbox + c(-x10,-y10,x10,y10)
  }
  
  ## Error handling for large BBOX
  ## ===============================================
  
  if (missing(value)) {
    osmdata <- try ({
      #could modify OSM values to facilitate larger bboxes (e.g., memsize or timeout)
      osmdata::opq (bbox = bbox) |>
        osmdata::add_osm_feature (key = key) |>
        osmdata::osmdata_sf ()
    })
  } else {
    osmdata <- try ({
      #could modify OSM values to facilitate larger bboxes (e.g., memsize or timeout)
      osmdata::opq (bbox = bbox) |>
        osmdata::add_osm_feature (key = key, value=value) |>
        osmdata::osmdata_sf ()
    })
  }

  
  if (class (osmdata) [1] == "try-error") {
    writeLines('wildlifeHI Issue: OSM data request timed out. Suggestions include: \n
               - Using the key and value parameters to choose less OSM features \n
               - Subset tracking data to smaller areas \n
               - Subset tracking data by individual(s) \n
               Please see the wildlifeHI documentation for further discussion of this issue.')
    return(NULL)
  } 
  

  
  osm_sf <- NULL
  if ('point' %in% geom) {
    temp <- osmdata$osm_points
    if (is.null(temp$osm_id)) { temp$osm_id <- row.names(temp)}
    if (length(temp)>0) {
      temp$key <- key
      temp$value <- sf::st_drop_geometry(temp)[,key]
      osm_sf <- rbind(osm_sf,temp[,c('osm_id','key','value')])
    }
  } 
  if ('line' %in% geom) {
    temp <- osmdata$osm_lines
    if (is.null(temp$osm_id)) { temp$osm_id <- row.names(temp)}
    if (length(temp)>0) {
      temp$key <- key
      temp$value <- sf::st_drop_geometry(temp)[,key]
      temp <- temp[,c('osm_id','key','value')]
      if (!is.null(osmdata$osm_multilines)){
        suppressWarnings(temp2 <- osmdata$osm_multilines |> sf::st_cast('LINESTRING'))
        temp2$key <- key
        temp2$value <- sf::st_drop_geometry(temp2)[,key]
        if (is.null(temp2$osm_id)) { temp2$osm_id <- row.names(temp2)}
        temp <- rbind(temp,temp2[,c('osm_id','key','value')])
      }
      osm_sf <- rbind(osm_sf,temp)
    }
  }
  if ('polygon' %in% geom) {
    temp <- osmdata$osm_polygons
    if (is.null(temp$osm_id)) { temp$osm_id <- row.names(temp)}
    if (length(temp)>0) {
      temp$key <- key
      temp$value <- sf::st_drop_geometry(temp)[,key]
      temp <- temp[,c('osm_id','key','value')]
      if (!is.null(osmdata$osm_multipolygons)){
        suppressWarnings(temp2 <- osmdata$osm_multipolygons |> sf::st_cast('POLYGON'))
        temp2$key <- key
        temp2$value <- sf::st_drop_geometry(temp2)[,key]
        if (is.null(temp2$osm_id)) { temp2$osm_id <- row.names(temp2)}
        temp <- rbind(temp,temp2[,c('osm_id','key','value')])
      }
      if (poly2line) {
        suppressWarnings(temp <- sf::st_cast(temp,"LINESTRING"))
      }
      osm_sf<- rbind(osm_sf, temp)
    }
  }
  if (is.null(osm_sf)){
    writeLines('wildlifeHI Issue: The requested OSM query returned NULL. Please check:
                  - The study area contains OSM features
                  - The specified key and value inputs are correct')
  }
  return(osm_sf)
  
}
