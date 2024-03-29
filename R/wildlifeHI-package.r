# This is package documentation for wildlifeHI.
# roxygen will use this file to create a NAMESPACE file.
# Of importance is the @import command, as it lists package dependencies.

#' @title wildlifeHI: Human Infrastructure and Wildlife Tracking Data
#'
#' @description A suite of basic tools for studying how wildlife are impacted by human infrastructure using tracking data. It focuses largely on working with Open Street Map data. It allows the user to easily perform distance, crossing, and buffer analysis. Also included are functions for attaching outdoor recreation activity data (Strava Global Heatmap) to wildlife tracking data.
#'
#' @details The package \code{wildlifeHI} incorporates a basic wrapper function (\code{hi_get_osm}) for extracting OSM data using the \code{osmdata} package. It also has functions for performing different types of analysis to study how wildlife respond to human infrastructure; these include distance analysis, identifying and mapping crossings, and buffer-type analysis.
#' 
#'
#' @author Jed Long
#' @author Matthew Ketchin
#' 
#' @import move move2 sf osmdata units dplyr reticulate fs tools rlang
#' @docType package
#' @name wildlifeHI-package
NULL
