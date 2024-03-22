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
#' @param move an object of the class \code{move2}. For more information on objects of this type see \code{help(move2)}.
#' @param fun a character string specifying which function to apply to each \code{trackId}. E.g., one of "hi_distance", "hi_crossing", "hi_buffer", etc. 
#' @param ... additional parameters passed to the function \code{fun} or to \code{hi_get_osm}. 
#'
#' @return
#'  This function returns either a \code{move2} containing the original tracking data with analysis defined by 'fun' applied. 
#'
#' @examples
#' \dontrun{
#' library(move2)
#' data(fishers)
#' fishers2 <- mt_as_move2(fishers)
#' fishers_d <- hi_by_trackId(fishers2,fun="hi_distance", key="highway", value="track") 
#' }
#' @export
#
# ---- End of roxygen documentation ----


hi_by_trackId <- function(move,fun="hi_distance",...){
  
  if(fun=="hi_recreation"){
    #check just in case
    answer <- readline("Running the hi_recreation function many times in quick succession can sometimes cause querying problems. \nEnter y if it's not too risky (y/n): ")
    
    if (tolower(answer) != "y") {
      message("Exiting...")
      return(invisible())
    }
  }
  
  #check if move2
  if(!inherits(move, "move2")){
    print('Input data not of class move2. Returning NULL.')
    return(NULL)
  }
  
  #get unique ids and col names
  ids <- unique(move2::mt_track_id(move))
  col <- move2::mt_track_id_column(move)
  time_col <- move2::mt_time_column(move)
  hi_f <- match.fun(fun)
  output <- NULL
  
  for (id in ids){
    m1 <- move[move[[col]] == id,]
    t1 <- Sys.time()
    
    #apply function here
    temp <- hi_f(m1,...)
    
    #If Null go to next one
    if (!is.null(temp)){
      #check if sf for hi_crossing_loc
      if (class(temp)[1]=='sf'){
        output <- rbind(output,temp)
      } else if (fun == "hi_recreation") {
        temp2 <- move2::mt_as_move2(temp, time_col, col)
        output <- rbind(output,temp2)
      } else {  #Assume Move class
        output <- c(output,list(temp))
      }
    } 

    t2 <- Sys.time()
    tdiff <- round(t2-t1,digits=1)
    print(paste0('Processed: ',id,'. Processing Time: ',tdiff,' s'))
  }
  output <- move2::mt_stack(output)  
  return(output)
}

