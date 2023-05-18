# ---- roxygen documentation ----
#
#' @title Get OSM data for tracking data bbox
#'
#' @description
#'  This function is a simple wrapper around the core functions of the \code{osmdata} package.
#'
#' @details
#'  This function is normally used internally to get OSM data for a specified tracking dataset,
#'  but can also be called directly to return the OSM data as an \code{sf} object. The parameters can be specified
#'  to choose  which OSM features are returned. The default is to return all 'highway' (i.e., road/trail) 
#'  line segments, but any OSM feature can be queried. For more information see: 
#'  https://wiki.openstreetmap.org/wiki/Map_features
#'
#' @param move an object of the class \code{move}. For more information on objects of this type see \code{
#'         help(move)}.
#' @param key string; OSM key string. Default is 'highway'. (see details and \code{?add_osm_feature} from 
#' the osmdata package)
#' @param value string; OSM value strings for specified key. Default is all values for that key. 
#' (see details and \code{?add_osm_feature} from the \code{osmdata} package).
#' @param bbox user specified bbox. Default is bbox of input \code{move} object +/- 10 percent.
#' @param geom string; the geometry type to return ('point', 'line', 'polygon' or combination thereof). 
#' Default is 'line'.
#' @param poly2line logical (default TRUE);  whether to convert polygon geometry to lines, which is useful 
#' in a variety of situations for example due to loops in many linear features, but also to look at border
#' crossings.
#'
#' @return
#'  This function returns an \code{sf} object containing OSM data. If the OSM query times out, a note is printed on the screen and the function returns \code{NULL}.
#'
#' @examples
#' data(fishers)
#' osmdata <- hi_get_osm(fishers)
#' osmdata_railway <- hi_get_osm(fishers,key='railway')
#' 
#' 
#' @export
#
# ---- End of roxygen documentation ----


hi_get_osm <- function(move,key='highway',value,bbox,geom="line",poly2line=TRUE){
  
  if (missing(bbox)){
    bbox <- st_bbox(move)
    x10 <- (bbox$xmax - bbox$xmin)*0.1
    y10 <- (bbox$ymax - bbox$ymin)*0.1
    bbox <- bbox + c(-x10,-y10,x10,y10)
  }
  
  ## Error handling for large BBOX
  ## ===============================================
  
  if (missing(value)) {
    osmdata <- try ({
      #could modify OSM values to facilitate larger bboxes (e.g., memsize or timeout)
      opq (bbox = bbox) |>
        add_osm_feature (key = key) |>
        osmdata_sf ()
    })
  } else {
    osmdata <- try ({
      #could modify OSM values to facilitate larger bboxes (e.g., memsize or timeout)
      opq (bbox = bbox) |>
        add_osm_feature (key = key, value=value) |>
        osmdata_sf ()
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
    temp_pt <- osmdata$osm_points
    if (length(temp_pt)>0) {
      temp_pt$key <- key
      temp_pt$value <- st_drop_geometry(temp_pt)[,key]
      osm_sf <- rbind(osm_sf,temp_pt[,c('osm_id','name','key','value')])
    }
  } 
  if ('line' %in% geom) {
    temp_ln <- osmdata$osm_lines
    if (length(temp_ln)>0) {
      temp_ln$key <- key
      temp_ln$value <- st_drop_geometry(temp_ln)[,key]
      temp_ln <- temp_ln[,c('osm_id','name','key','value')]
      if (!is.null(osmdata$osm_multilines)){
        suppressWarnings(temp_ln2 <- osmdata$osm_multilines |> st_cast('LINESTRING'))
        temp_ln2$key <- key
        temp_ln2$value <- st_drop_geometry(temp_ln2)[,key]
        temp_ln <- rbind(temp_ln,temp_ln2[,c('osm_id','name','key','value')])
      }
      osm_sf <- rbind(osm_sf,temp_ln)
    }
  }
  if ('polygon' %in% geom) {
    temp_po <- osmdata$osm_polygons
    if (length(temp_po)>0) {
      temp_po$key <- key
      temp_po$value <- st_drop_geometry(temp_po)[,key]
      temp_po <- temp_po[,c('osm_id','name','key','value')]
      if (!is.null(osmdata$osm_multipolygons)){
        suppressWarnings(temp_po2 <- osmdata$osm_multipolygons |> st_cast('POLYGON'))
        temp_po2$key <- key
        temp_po2$value <- st_drop_geometry(temp_po2)[,key]
        temp_po <- rbind(temp_po,temp_po2[,c('osm_id','name','key','value')])
      }
      if (poly2line) {
        suppressWarnings(temp_po <- st_cast(temp_po,"LINESTRING"))
      }
      osm_sf<- rbind(osm_sf, temp_po)
    }
  }
  if (is.null(osm_sf)){
    writeLines('wildlifeHI Issue: The requested OSM query returned NULL. Please check:
                  - The study area contains OSM features
                  - The specified key and value inputs are correct')
  }
  return(osm_sf)
  
}