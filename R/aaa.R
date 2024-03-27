# ---- roxygen documentation ----
#' @title for managing internal state
#'
#' @description
#'   For managing internal state
#'   
#' @noRd
# ---- End of roxygen documentation ----


#managing environments in hi_recreation to minimize load times in subsequent runs
the <- new.env(parent = emptyenv())
the$first_time <- TRUE
