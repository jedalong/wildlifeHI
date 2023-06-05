# ---- roxygen documentation ----
#
#' @title Crossing analysis of human infrastructure
#'
#' @description
#'  This function calculates whether a trajectory crosses a linear feature of human infrastructure.
#'
#' @details
#'  This tool simply calculates whether crossing occurs from every segment of consecutive tracking fixes in relation to the input human infrastructure (assumed to be linear features).The default is to use any linear road/trail feature defined in the 'highway' key, but any OSM feature can be specified. See \code{?hi_get_osm}.
#'
#' @param move an object of the class \code{move}. For more information on objects of this type see \code{help(move)}.
#' @param osmdata an \code{sf} object containing human infrastructure data formatted similar to OSM data. 
#' See \code{?hi_get_osm}.
#' @param ... additional parameters passed to \code{hi_get_osm}
#'
#' @return
#'  This function returns a \code{MoveStack} object containing the original tracking
#'   data with three additional columns:
#'  - crossing_true: a logical variable indicating whether a segment crossed any linear features.
#'  - crossing_key: the OSM key of the feature that was crossed in \code{osmdata}
#'  - crossing_value: the OSM value of the feature that was crossed in \code{osmdata}.
#'  - crossing_count: the count of the number of crossing features in \code{osmdata}.
#'
#' @examples
#' \dontrun{
#' library(move)
#' data(fishers)
#' fishers_c <- hi_crossing(fishers)
#' table(fishers_c$crossing_value)
#' }
#' 
#' @export
#
# ---- End of roxygen documentation ----


hi_crossing <- function(move,osmdata,...){
  
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
  
  #get osm data
  #osmdata <- hi_get_osm(move)
  if (missing(osmdata)){
    osmdata <- hi_get_osm(move, ...)
  } 
  
  #IF no OSM data is returned for query return NA values
  if (is.null(osmdata)){
    move$crossing_true <- NA
    move$crossing_key <- NA
    move$crossing_value <- NA
    move$crossing_count <- NA
    return(move)
  }
  
  #key <- names(osmdata)[2]
  #val <- names(osmdata)[3]
  
  sf_pt <- st_as_sf(move)
  sf_pt$trackId <- trackId(move)
  data_crs <- st_crs(move)
  
  # Create linestrings
  sf_ln <- internal_hi_move2line(move) 
  
  #make sure lines and osmdata same CRS
  osmdata <- st_transform(osmdata,data_crs)
  
  ## Get TRUE/FALSE intersections
  mat <- st_intersects(sf_ln,osmdata,sparse=FALSE)
  
  fun_key <- function(x){ 
    f <- factor(st_drop_geometry(osmdata)[x,'key'])
    levels(f)[which.max(tabulate(f))]
  }
  fun_val <- function(x){ 
    f <- factor(st_drop_geometry(osmdata)[x,'value'])
    levels(f)[which.max(tabulate(f))]
  }
  
  xtrue <- apply(mat,1,any)
  xkey <- apply(mat,1,fun_key)
  xval <- apply(mat,1,fun_val)
  xcount <- apply(mat,1,sum)
  
  ### crossing data to move object
  indo <- which(trackId(move) != dplyr::lag(trackId(move)))
  for (j in indo){
    xtrue <- append(xtrue,NA,after=(j-1))
    xkey <- append(xkey,NA,after=(j-1))
    xval <- append(xval,NA,after=(j-1))
    xcount <- append(xcount,NA,after=(j-1))
  }

  move$crossing_true <- c(xtrue,NA)
  move$crossing_key <- c(xkey,NA)
  move$crossing_value <- c(xval,NA)
  move$crossing_count <- c(xcount,NA)
  
  return(move)

}

