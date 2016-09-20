#' Package containing internal BaDaSS convenience functions
#' 
#' This package contains the following functions
#' \enumerate{
#'   \item SummaryTable
#'   \item require.anyway
#'   \item GetCommonPackages
#'   }
#' @author Dominic Laroche,
#' Chang Xu,
#' Brian Mannakee
#' 
#' Maintainer: Dominic Laroche \email{dominic.laroche@@contractors.roche.com}
#' Depends: xtable, rtf, ggplot2
#' @import xtable rtf ggplot2 rms
#' @docType package
#' @name BDSS
#' 
NULL

#' A Sample Recurrence Dataset
#' 
#' This is a randomly generated dataset containing
#' recurrence data on 200 patients on one of two treatments
#' based on their age. Mostly used to test the function SummaryTable.
#' The variables are as follows:
#' 
#' \itemize{
#'   \item Age. age of patients, mean 60
#'   \item Recur. did the patient recur (recurred,not recurred)
#'   \item treated. did the patient recieve treatment 1 (Treated, Not Treated)
#'   \item trt2. did the patient recieve treatment 2 (Treatment2, No Trt2)
#'   \item meas. Age - 30.
#'   \item single. all patients get single treatment. (Yes)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name BDSSRecurrenceExample
#' @format A data frame with 200 rows and 6 variables
NULL

