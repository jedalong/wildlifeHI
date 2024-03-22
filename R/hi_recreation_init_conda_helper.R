# ---- roxygen documentation ----
#
#' @title Conda environment helper
#'
#' @description
#'  The function, `hi_recreation_init_conda_helper()`, creates the appropriate Python environment for the user in miniconda. If the user does not have miniconda installed, it handles this too. 
#'
#' @details
#'  Using `hi_recreation_init_conda_helper()` may require administrator privileges. It creates a conda environment with a specified environment name using an environment.yml file that is included with the package. If miniconda is not downloaded, it will install miniconda. This can take some time.
#'  
#' @param env_name string; the name you want to use for the environment.
#' 
#' @return
#'  This function returns the environment name.
#'
#' @examples
#' \dontrun{
#' envName <- hi_recreation_init_conda_helper(env_name = 'hi_rec')
#' }
#' 
#' @export
#
# ---- End of roxygen documentation ----


#initialize conda for the user because you need it to run the main function
hi_recreation_init_conda_helper <- function(env_name = "wild_hi_recreation") {
  #install miniconda if it isn't already
  tryCatch({
    reticulate::install_miniconda()
  },
  error=function(cond) {
    message("Miniconda already installed. Proceeding to next step. If you need to replace this installation, please remove the existing installation of miniconda. Running reticulate::py_discover_config() will give you more information.")
  })
  
  if (tools::file_path_sans_ext(base::basename(reticulate::miniconda_path())) != "r-miniconda"){
    message("A new version of miniconda (r-miniconda) will be installed to allow for the execution of Python scripts. This may take some time.")
    #if this is not needed, the user needs to specify use_condaenv and the location of the conda binary installation 
    #  or use_miniconda because it has already been specified.  
    reticulate::install_miniconda()
    
  } else {
    message("Checking environments...")
  }
  
  #check environment names for the default (or whatever was specified)
  envs <- reticulate::conda_list()
  if (env_name %in% envs$name) {
    message("The target Conda environment already exists, executing script.")
    reticulate::use_condaenv(env_name, required = TRUE)
  } else {
    message("Conda environment does not exist, creating environment. This may take some time.")
    yml_file <- file.path(fs::path_package("wildlifeHI"), "Python", 'environment.yml')
    reticulate::conda_create(envname = env_name, conda = reticulate::conda_binary(), environment = yml_file)
  }
  reticulate::use_condaenv(env_name, required = TRUE)
  return(env_name)
}

#### MATT CHECK THAT THIS SHOULD BE HERE!?!?!? THIS WONT WORK IN A PACKAGE FORMAT?
#set a state
the <- new.env(parent = emptyenv())
the$first_time <- TRUE
