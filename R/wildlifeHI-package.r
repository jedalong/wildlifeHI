# This is package documentation for wildlifeHI.
# roxygen will use this file to create a NAMESPACE file.
# Of importance is the @import command, as it lists package dependencies.

#' @title wildlifeHI: Tools for Linking Wildlife Tracking Data to Human Infrastructure
#'
#' @description This package contains a suite of basic tools for studying how wildlife are impacted by human infrastructure using tracking data. It focuses largely on working with Open Street Map (OSM) data using the package \code{osmdata}. 
#'
#' @details The package \code{wildlifeHI} incorporates a basic wrapper function (\code{hi_get_osm}) for extracting OSM data using the \code{osmdata} package. It also has functions for performing different types of analysis to study how wildife respond to human infrastructure; these include distance analysis, identifying and mapping crossings, and buffer-type analysis. The goal is to include several of these functions as MoveApps.
#' 
#'
#' @author Jed Long
#' 
#' @import move sf osmdata units
#
#' @docType package
#' @name wildlifeHI-package
NULL
