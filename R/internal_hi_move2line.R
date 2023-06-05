# ---- roxygen documentation ----
#' @title convert move to sf line object
#'
#' @description
#'   Internal function for converting move to a sf line
#'   
#' @param move a move or moveStack object.
#' @param crs_code a CRS code.
#' 
#' @return
#'   This function returns a sf line
#' @importFrom rlang .data
#'   
#' @noRd
# ---- End of roxygen documentation ----



internal_hi_move2line <- function(move){

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
  
  sf_pt <- st_as_sf(move)
  n <- nrow(sf_pt)
  sf_pt <- rbind(sf_pt[1:(n-1),], sf_pt[2:n,])
  sf_pt$SEGID <- 1:(n-1)
  
  sf_ln <- sf_pt |>
    dplyr::group_by(.data$SEGID) |>
    dplyr::summarize(do_union=FALSE) |>
    st_cast("LINESTRING")

  
  id_df <- data.frame(trackId=trackId(move)[1:(n-1)],
                      trackId2=trackId(move)[2:n],
                      timestamp1=timestamps(move)[1:(n-1)],
                      timestamp2=timestamps(move)[2:n])
  
  sf_ln <- cbind(sf_ln,id_df)
  
  #Remove line segments between individuals
  ind <- which(sf_ln$trackId != sf_ln$trackId2)
  if (length(ind) > 0){ sf_ln <- sf_ln[-ind,] }
  
  #remove unwanted columns
  sf_ln <- sf_ln |> 
    dplyr::select(-"SEGID") |>
    dplyr::select(-"trackId2")
  
  return(sf_ln)
}