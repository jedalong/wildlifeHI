# ---- roxygen documentation ----
#' @title convert move to sf line object
#'
#' @description
#'   Internal function for converting move2 to a sf line
#'   
#' @param move a move2 object.
#' @param crs_code a CRS code.
#' 
#' @return
#'   This function returns a sf line
#' @importFrom rlang .data
#'   
#' @noRd
# ---- End of roxygen documentation ----



internal_hi_move2line <- function(move){
  
  if(!inherits(move, "move2")){
    print('Input data not of class move2. Returning NULL.')
    return(NULL)
  }
  
  sf_pt <- move
  col <- move2::mt_track_id_column(move)
  tim <- move2::mt_time_column(move)
  move2::mt_track_id(sf_pt) <- NULL
  n <- nrow(sf_pt)
  sf_pt <- rbind(sf_pt[1:(n-1),], sf_pt[2:n,])
  sf_pt$SEGID <- 1:(n-1)
  
  sf_ln <- sf_pt |>
    dplyr::group_by(.data$SEGID) |>
    dplyr::summarize(do_union=FALSE) |>
    st_cast("LINESTRING")

  
  id_df <- data.frame(trackId=move2::mt_track_id(move)[1:(n-1)],
                      trackId2=move2::mt_track_id(move)[2:n],
                      timestamp1=move2::mt_time(move)[1:(n-1)],
                      timestamp2=move2::mt_time(move)[2:n])
  
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

