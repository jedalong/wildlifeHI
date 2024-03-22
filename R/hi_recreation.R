# ---- roxygen documentation ----
#
#' @title HI Recreation
#'
#' @description
#'  Attach outdoor recreation activity levels to GPS tracking data.
#'  
#' @details
#'  To use the `hi_recreation()` function, you will need access to an active Strava account.
#'  In `hi_recreation()`, pixel values are obtained from the Strava Global Heatmap and an intensity of use is calculated based on those pixel values along the GPS track provided as input. This can be useful for determining if individuals avoid or prefer areas of high human activity.
#'  
#' @param move an object of the class \code{move2}. For more information on objects of this type see \code{help(move2)}.
#' @param cookies string; formatted string from `hi_recreation_cookie_constructor()`. This is a required parameter.
#' @param env_name string (optional); the name of the environment specified in `hi_recreation_init_conda_helper()` or the name of a Conda environment that meets the requirements.
#' 
#' @return
#'  This function returns a \code{move2} object with the R, G, B, and A pixel values and the calculated intensity values appended from the Strava Global Heatmap.
#'
#' @examples
#' \dontrun{
#' library(move)
#' library(move2)
#' data(fishers)
#' fishers2 <- mt_as_move2(fishers)
#' hi_recreation_init_conda_helper(env_name = 'hi_rec')
#' cookies <- hi_recreation_cookie_constructor(<Key-Pair-Id>, <Policy>, <Signature>)
#' fishers2 <- hi_recreation(move = fishers2, cookies = cookies, env_name = 'hi_rec')
#' }
#' 
#' @export
#
# ---- End of roxygen documentation ----

hi_recreation <- function(move, cookies = NULL, env_name = "wild_hi_recreation"){
  
  #do the first time stuff
  if(the$first_time){
    the$first_time <- FALSE
    # use conda environment specified by user
    tryCatch({
      # should I have this in onLoad? And all the stuff with adding the qgis python path? I feel like it should be. 
      reticulate::use_condaenv(env_name, required = TRUE)
    },
    error = function(conda_error){
      message("Conda environment does not exist, please create the environment and specify the correct environment (reticulate::use_miniconda() and reticulate::use_condaenv()) in the function arguments, or run the 'initCondaHelper(<environment_name>)' function to set it up. ")
      return(move)
    })
    # Get the Python configuration https://stackoverflow.com/questions/64678135/segmentation-fault-11-when-using-initqgis-macos
    python_config <- reticulate::py_config()
    # Append "/Library/python" to the pythonhome path so that qgis can be discovered (since it is stored somewhere else)
    qgis_python_path <- file.path(python_config$pythonhome, "Library", "python")
    #need to set plugins path location within python script
    # thank you to: https://gis.stackexchange.com/questions/155745/layer-is-not-valid-error-in-my-standalone-pyqgis-script-app/155852#155852
    the$plugins_path <- file.path(python_config$pythonhome, "Library", "plugins")
    # append the path location (or move qgis out of the /Library/python into /Lib/site-packages)
    reticulate::py_run_string(sprintf('import sys; sys.path.append("%s")', qgis_python_path))
  }
  
  # check input data type
  if(inherits(move, "move2")) {
    if(!is.na(sf::st_crs(move)$epsg) && sf::st_crs(move)$epsg != 4326){
      move <- sf::st_transform(move, 4326)
      print("Transformed CRS of move2 object to EPSG:4326.")
    }else {
      move <- sf::st_set_crs(move, 4326)
      print("CRS was NA, so it was set to EPSG:4326. If this was not intended, please set your CRS and try again.")
    }
  }else {
    print('Input data not of class move2. Returning NULL. You can use mt_as_move2() to convert a MoveStack or Move object to move2.')
    return(NULL)
  }
  
  if(is.null(cookies)){
    print("Need to input cookies.")
    return(NULL)
  }
  
  # need the geometry column for after
  moveGeometry <- move$geometry
  # imports the functions contained within the specified file
  reticulate::source_python(file.path(fs::path_package("wildlifeHI"), "Python", 'hi_recreation.py'))
  # run the function from the python file and catch the pandas output
  output <- hi_rec_py(move, cookies, the$plugins_path)
  # append the geometry back to it
  output$geometry <- moveGeometry
  return(output)
}




