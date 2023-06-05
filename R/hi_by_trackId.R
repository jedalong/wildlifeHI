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
#' \dontrun{
#' data(fishers)
#' fishers <- hi_by_trackId(fishers,fun="hi_distance",key="highway", value="track") 
#' }
#' @export
#
# ---- End of roxygen documentation ----


hi_by_trackId <- function(move,fun="hi_distance",...){
  
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
  
  ids <- unique(trackId(move))
  hi_f <- match.fun(fun)
  output <- NULL
  
  for (id in ids){
    m1 <- move[[id]]
    t1 <- Sys.time()
    
    #apply function here
    temp <- hi_f(m1,...)
    
    #If Null go to next one
    if (!is.null(temp)){
      #check if sf for hi_crossing_loc
      if (class(temp)[1]=='sf'){
        output <- rbind(output,temp)
      } else {  #Assume Move class
        output <- c(output,temp)
      }
    } 

    t2 <- Sys.time()
    tdiff <- round(t2-t1,digits=1)
    print(paste0('Processed: ',id,'. Processing Time: ',tdiff,' s'))
  }
  
  #Fix up if a Move object
  if (class(output)[1] != 'sf') { 
    output <- moveStack(output)
    row.names(output) <- row.names(move) 
    }
    
  return(output)
}

