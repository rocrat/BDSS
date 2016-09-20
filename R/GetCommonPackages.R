#' @title A list of commonly used packages
#' 
#' @description Returns a list of commonly used packages.
#' Intended to be used in conjunction with BDSS::require.anyway()
#' to force loading of commonly used packages into a users environment
#' 
#' @param pkgs a vector of additional packages to load
#' @param verbose logical. TRUE prints the list of packages
#' returned by the function. Useful for debugging.
#' 
#' @usage GetCommonPackages(pkgs=NULL,verbose=FALSE)
#' 
#' @examples
#' \donttest{
#' extra.pkgs <- c('MASS','brew')
#' require.anyway(GetCommonPackages(pkgs=extra.pkgs,verbose=TRUE))
#' }
#' 
#' @export
#' @name GetCommonPackages
#' @author Brian Mannakee

GetCommonPackages <- function(pkgs=NULL,verbose=FALSE){
  common <- c('rtf','rms','epiR','xlsx','ggplot2','xtable','OptimalCutpoints',
              'survival')
  if (is.vector(pkgs)){
    common <- c(common,pkgs)
  }
  if (verbose){
    print('Returning Packages')
    print(common)
  }
  return(common)
}