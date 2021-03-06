% Generated by roxygen2 (4.0.1): do not edit by hand
\name{GetCommonPackages}
\alias{GetCommonPackages}
\title{A list of commonly used packages}
\usage{
GetCommonPackages(pkgs=NULL,verbose=FALSE)
}
\arguments{
\item{pkgs}{a vector of additional packages to load}

\item{verbose}{logical. TRUE prints the list of packages
returned by the function. Useful for debugging.}
}
\description{
Returns a list of commonly used packages.
Intended to be used in conjunction with BDSS::require.anyway()
to force loading of commonly used packages into a users environment
}
\examples{
\donttest{
extra.pkgs <- c('MASS','brew')
require.anyway(GetCommonPackages(pkgs=extra.pkgs,verbose=TRUE))
}
}
\author{
Brian Mannakee
}

