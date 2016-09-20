#' @title require.anyway
#' 
#' @description
#' checks the local system to see if a package
#' is installed, and if not, installs it
#' 
#' @details
#' the function wraps a call to install.packages
#' that uses system defaults. \code{install.packages(pkg)}
#' 
#' @usage require.anyway(pkg,verbose=TRUE)
#' 
#' @param pkg the package to be loaded
#' @param verbose logical, whether to print confirmation that package loaded correctly
#' 
#' @export
#' 
#' @name require.anyway
#' @author Chang Xu



require.anyway <- function(pkg,verbose=TRUE) {
  if(require(pkg, character.only=T)){
    if(verbose){
      print(paste0(pkg, " is loaded correctly"))
    }
  } 
  else {
    print(paste0("trying to install ", pkg))
    
    install.packages(pkg)
    if(require(pkg, character.only=T)){
      print(paste0(pkg, " installed and loaded"))
    } else {
      stop(paste0("could not install ", pkg))
    }
  } 
}