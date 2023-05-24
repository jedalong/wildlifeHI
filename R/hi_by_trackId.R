# ---- roxygen documentation ----
#
#' @title Process HI Analysis by trackId
#'
#' @description
#'  This function is a wrapper used to automate processing of HI functions by trackId.
#'
#' @details
#'  This tool is used to automate the processing of HI analysis by \code{trackId}. In particular, it is useful when the bounding box of individual tracks is relatively small compared to the overall study area. Processing by individual \code{trackId} is a way to reduce the sizes of queries for OSM data, see \code{?hi_get_osm}.
#'
#' @param move an object of the class \code{move}. For more information on objects of this type see \code{help(move)}.
#' @param fun a character string specifying which function to apply to each \code{trackId}. E.g., one of "hi_distance", "hi_crossing", "hi_buffer", etc. 
#' @param ... additional parameters passed to the function \code{fun} or to \code{hi_get_osm}. 
#'
#' @return
#'  This function returns either a \code{move} containing the original tracking data with analysis defined by 'fun' applied. 
#'
#' @examples
#' data(fishers)
#' fishers <- hi_by_trackId(fishers,fun="hi_distance",key="highway", value="track") 
#' 
#' @export
#
# ---- End of roxygen documentation ----


hi_by_trackId <- function(move,fun="hi_distance",...){
  
  #check input data type
  if (class(move) != 'MoveStack'){
    if (class(move) == 'Move'){
      move <- moveStack(move, forceTz='UTC') #fix this timestamp to correct time zone
    } else {
      print('Input Data not of class MoveStack. Returning NULL.')
      return(NULL)
    }
  }
  
  ids <- unique(trackId(move))
  data_crs <- st_crs(move)
  tz <- attr(timestamps(move),'tzone')
  
  hi_f <- match.fun(fun)
  
  for (id in ids){
    
    m1 <- move[[id]]
    t1 <- Sys.time()
    temp <- hi_f(m1,...)
    
    
    if (!is.null(out_move)){
      out_move <- moveStack(out_move,temp,forceTz=tz)
    } else {
      out_move <- temp
    }
    t2 <- Sys.time()
    tdiff <- c(tdiff,t2-t1)
    print(paste0('processing: ',id,'. Processing Time: ',tdiff))
  }
  
  row.names(out_move) <- row.names(move)
  return(out_move)
}

