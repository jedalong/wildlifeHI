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
#' data(fishers)
#' ## x1 <- hi_crossing_loc(fishers)  #Takes 2270 seconds
#' x2 <- hi_crossing_loc(fishers,crs_code=32618)  ## takes 12 seconds
#' 
#' libray(mapview)
#' mapview(x2['value'])
#' }
#' 
#' @export
#
# ---- End of roxygen documentation ----

hi_crossing_loc <- function(move,osmdata,crs_code,...){
  
  
  #check input data type
  if (class(move) != 'MoveStack'){
    if (class(move) == 'Move'){
      move <- moveStack(move, forceTz='UTC') #fix this timestamp to correct time zone
    } else {
      print('Input Data not of class MoveStack. Returning NULL.')
      return(NULL)
    }
  }
  
  #GET OSM Data and return Null if none exists.
  if (missing(osmdata)){
    osmdata <- hi_get_osm(move, ...)
  }
  if (is.null(osmdata)){ return(NULL)}
  
  
  #convert move to sf
  sf_pt <- st_as_sf(move)
  sf_pt$trackId <- trackId(move)
  sf_pt$jtime <- timestamps(move)
  data_crs <- st_crs(move)
  
  
  #Use a projected coordinate system if specified - MAKES INTERSECTION WORK WAY BETTER
  if (!missing(crs_code)){
    osmdata <- st_transform(osmdata,crs=crs_code)
    sf_pt <- st_transform(sf_pt,crs=crs_code)
  } else {
    crs_code = data_crs
  }
  
  # Create linestrings need to fix to do by ID
  n <- nrow(sf_pt)
  sf_p1 <- sf_pt[1:(n-1),]
  sf_p2 <- sf_pt[2:n,]
  
  id_df <- data.frame(trackId=sf_p1$trackId,
                      trackId2=sf_p2$trackId,
                      timestamp1=sf_p1$jtime,  
                      timestamp2=sf_p2$jtime)
  suppressWarnings({
    sf_ln <- st_sfc(mapply(
      function(a,b){st_cast(st_union(a,b),'LINESTRING')}
      ,sf_p1$geometry,sf_p2$geometry,SIMPLIFY=FALSE),crs=crs_code) |>
      st_sfc()
  })

  
  sf_ln <- st_sf(id_df,sf_ln)
  
  #Remove line segments between individuals
  ind <- which(sf_ln$trackId != sf_ln$trackId2)
  sf_ln <- sf_ln[-ind,]
  sf_ln <- subset(sf_ln, select = -trackId2)
  
  #get locations of crossings (lines/poly boundaries)
  suppressWarnings(sf_int <- st_intersection(sf_ln,osmdata))
  
  sf_int <- st_transform(sf_int,data_crs)
  return(sf_int)
}