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
#' data(fishers)
#' fishers_c <- hi_crossing(fishers)
#' table(fishers_c$crossing_value)
#' 
#' @export
#
# ---- End of roxygen documentation ----


hi_crossing <- function(move,osmdata,...){
  
  
  #check input data type
  if (class(move) != 'MoveStack'){
    if (class(move) == 'Move'){
      move <- moveStack(move, forceTz='UTC') #fix this timestamp to correct time zone
    } else {
      print('Input Data not of class MoveStack. Returning NULL.')
      return(NULL)
    }
  }
  
  #get osm data
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
  
  key <- names(osmdata)[2]
  val <- names(osmdata)[3]
  
  sf_pt <- st_as_sf(move)
  sf_pt$trackId <- trackId(move)
  data_crs <- st_crs(move)
  
  # Create linestrings need to fix to do by ID
  n <- nrow(sf_pt)
  sf_p1 <- sf_pt[1:(n-1),]
  sf_p2 <- sf_pt[2:n,]
  id_df <- data.frame(trackId=sf_p1$trackId,
                      trackId2=sf_p2$trackId,
                      timestamp1=sf_p1$timestamp,  ##this might be wrong way to access times
                      timestamp2=sf_p2$timestamp)
  sf_ln <- st_sfc(mapply(
    function(a,b){st_cast(st_union(a,b),'LINESTRING')}
    ,sf_p1$geometry,sf_p2$geometry,SIMPLIFY=FALSE),crs=data_crs) |> 
    st_sfc()
  
  sf_ln <- st_sf(id_df,sf_ln)
  
  #Remove line segments between individuals
  ind <- which(sf_ln$trackId != sf_ln$trackId2)
  sf_ln <- sf_ln[-ind,]
  sf_ln <- subset(sf_ln, select = -trackId2)
  
  ## Get TRUE/FALSE intersections
  mat <- st_intersects(sf_ln,osmdata,sparse=FALSE)
  
  fun_key <- function(x){ 
    f <- factor(st_drop_geometry(osmdata)[x,key])
    levels(f)[which.max(tabulate(f))]
  }
  fun_val <- function(x){ 
    f <- factor(st_drop_geometry(osmdata)[x,val])
    levels(f)[which.max(tabulate(f))]
  }
  
  sf_ln$crossing_true <- apply(mat,1,any)
  sf_ln$crossing_key <- apply(mat,1,fun_key)
  sf_ln$crossing_value <- apply(mat,1,fun_val)
  sf_ln$crossing_count <- apply(mat,1,sum)
  

  #THIS IS VERY CLUNKY - Need to verify works with many individuals
  xtrue <- sf_ln$crossing_true
  xkey <- sf_ln$crossing_key
  xval <- sf_ln$crossing_value
  xcount <- sf_ln$crossing_count
    
  for (i in ind){
    xtrue <- append(xtrue,NA,after=i-1)
    xkey <- append(xkey,NA,after=i-1)
    xval <- append(xval,NA,after=i-1)
    xcount <- append(xcount,NA,after=i-1)
  }
    
  move$crossing_true <- c(xtrue,NA)
  move$crossing_key <- c(xkey,NA)
  move$crossing_value <- c(xval,NA)
  move$crossing_count <- c(xcount,NA)
  return(move)

}

