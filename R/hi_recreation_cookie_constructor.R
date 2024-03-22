# ---- roxygen documentation ----
#
#' @title Cookie constructor
#'
#' @description
#'  The function, `hi_recreation_cookie_constructor()`, creates a string from the Key-Pair-Id, Policy, and Signature from the Strava Global Heatmap.
#'  The function, `hi_recreation_init_conda_helper()`, creates the appropriate Python environment for the user in miniconda. If the user does not have miniconda installed, it handles this too. 
#'
#' @details
#'  To use `hi_recreation_cookie_constructor()`, navigate to https://www.strava.com/heatmap in Chrome or Firefox after signing in to the website, right click the header and select 'Inspect'. Navigate to 'Application' on the inspect window header and then to 'https://www.strava.com' under 'Cookies' on the sidebar. In the main window under the 'Value' column, copy the values corresponding with 'CloudFront-Key-Pair-Id', 'CloudFront-Policy', 'CloudFront-Signature' as comma separated string arguments into this function.  
#'  Using `hi_recreation_init_conda_helper()` may require administrator privileges.
#'  
#' @param KeyPairID string; obtained from https://www.strava.com/heatmap
#' @param Policy string; obtained from https://www.strava.com/heatmap
#' @param Signature string; obtained from https://www.strava.com/heatmap
#' 
#' @return
#'  This function returns a formatted string.
#'
#' @examples
#' \dontrun{
#' cookies <- hi_recreation_cookie_constructor(<Key-Pair-Id>, <Policy>, <Signature>)
#' }
#' 
#' @export
#
# ---- End of roxygen documentation ----

# gerenate cookies from the Strava website
hi_recreation_cookie_constructor <- function(KeyPairID, Policy, Signature) {
  return(paste("Key-Pair-Id=", KeyPairID, "&Policy=", Policy, "&Signature=", Signature, sep=""))
}