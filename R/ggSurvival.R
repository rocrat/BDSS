#' @title Kaplan Meier plots with the ggplot package 
#' 
#' @description ggSurvplot creates a ggplot object from a '\code{survfit}' object.  The resulting plot can be extended using any of ggplot's many tools
#' @details The plot is constructed from the survfit object by accessing the internal structure of the fit and plotting a curve for each strata and confidence interval if desired.
#' 
#' @usage ggSurvival(s, CI = FALSE, spot.cens = FALSE, surv.col = 'gg.def',
#' cens.col = 'red', lty.est = 1, lty.ci = 2, lwd.est=1, lwd.ci=1,
#' cens.shape = 3, black.white = TRUE, xlab = 'Time', ylab = 'Survival',
#'  main = '',stratName=NULL, groupNames=NULL, groupNameLevels=NULL)
#' 
#' @param s A object of class 'survfit'
#' @param CI Logical indicating whether to plot confidence intervals as defined in the survfit object
#' @param spot.cens Logical indicating whether to add tick marks to censor points
#' @param surv.col A vector of colors for the survival curves
#' @param cens.col A color for the censor ticks
#' @param lty.est A vector of line types for the survival curves
#' @param lty.ci A vector of line types for the confidence intervals
#' @param lwd.est A vector of line widths for the survival curves
#' @param lwd.ci A vector of line widths for the confidence intervals
#' @param cens.chape A character string or number indicating what shape the censor indicator should be.
#' @param black.white Logical indicating whether to use \code{theme_bw}
#' @param xlab,ylab,main Same as ggplot2 and base
#' @param stratName A character string which overrides the name of the stratification variable (optional for prettyification).
#' @param groupNames A character vector renaming the levels of the stratification variable.  These must be in the same order as the original factor levels (optional).
#' @param groupNameLevels A character vector of the stratification variable levels in the order which you want them to appear.
#' @param ylimit a vector giving the limits for plotting the y axis (default to c(0,1)). 
#' 
#' 
#' @examples
#' \donttest{
#' require(survival)
#' require(ggplot2)
#' data(BDSSRecurrenceExample)
#' dat <- BDSSRecurrenceExample
#' dat$Rec<-ifelse(dat$Recur=="recurred",1,0)
#' sv<-Surv(dat$time,dat$Rec)
#' sv.fit<-survfit(sv~treated,data=dat)
#' ggSurvival(sv.fit,stratName="Treatment")+theme(legend.position="bottom")
#' }
#' @export
#' @name ggSurvival
#' @author Dominic LaRoche (adapted from Edwin Thoen)
#' 

ggSurvival <- function(s, CI = FALSE, spot.cens = FALSE, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2, lwd.est=1, lwd.ci=1,
                       cens.shape = 3, black.white = TRUE, xlab = 'Time', stratName=NULL,
                       ylab = 'Survival', main = '',groupNames=NULL, groupNameLevels=NULL,
                       ylimit= c(0,1)){
  
  
  strata <- ifelse(is.null(s$strata), 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  
  ggsurv.s <- function(){
    
    dat <- data.frame(time = c(0, s$time),
                      surv = c(1, s$surv),
                      up = c(1, s$upper),
                      low = c(1, s$lower),
                      cens = c(0, s$n.censor))
    dat.cens <- subset(dat, cens != 0)#uncensored data
    
    col <- surv.col
    
    sp <- ggplot(dat, aes(x = time, y = surv)) + 
      xlab(xlab) + ylab(ylab) + ggtitle(main) + 
      geom_step(col = col, lty = lty.est, lwd=lwd.est) +
      ylim(ylimit)
    
    if(!is.null(CI)) {
      sp <- sp + geom_step(aes(y = up), color = col, lty = lty.ci, lwd= lwd.ci) +
        geom_step(aes(y = low), color = col, lty = lty.ci, lwd= lwd.ci)
    } 
    
    if(spot.cens & length(dat.cens) > 0){
      sp <- sp + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                            col = cens.col)
    } else if (spot.cens & length(dat.cens) == 0){
      stop ('There are no censored observations') 
    } 
    
    if(black.white) {
      sp <- sp + theme_bw()
    } else 
      print(sp)
  }
  
  ggsurv.m <- function() {
    n <- s$strata
    if(!is.null(groupNames)){
      groups<-factor(groupNames,levels=groupNameLevels)
    }else{
      groups <- factor(unlist(strsplit(names(s$strata), '='))[seq(2, 2*strata, by = 2)])
    }
    if(!is.null(stratName)){
      str.name <- stratName
    }else{
      str.name <-  unlist(strsplit(names(s$strata), '='))[1]
    }
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
    #make separate df for each strata
    for(i in 1:strata){
      gr.df[[i]] <- data.frame(
        time = c(0, s$time[ ind[[i]] ]),
        surv = c(1, s$surv[ ind[[i]] ]),
        up = c(1, s$upper[ ind[[i]] ]), 
        low = c(1, s$lower[ ind[[i]] ]),
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], n[i] + 1)) 
    }
    
    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)
    
    sp <- ggplot(dat, aes(x = time, y = surv, group = group)) + 
      xlab(xlab) + ylab(ylab) + ggtitle(main) + ylim(ylimit) +
      geom_step(aes(col = group, lty = group), lwd = lwd.est)
    
    if(length(surv.col == 1)){
      col <- scale_colour_manual(name = str.name, values = rep(surv.col, strata))
    } else{
      col <- scale_colour_manual(name = str.name, values = surv.col)
    }
    
    if(surv.col[1] != 'gg.def'){
      sp <- sp + col
    } else {
      sp <- sp + scale_colour_discrete(name = str.name)}
    
    if(length(lty.est) == 1){
      line <- scale_linetype_manual(name = str.name, values = rep(lty.est, strata))
    } else {
      line <- scale_linetype_manual(name = str.name, values = lty.est)
    }
    
    sp <- sp + line
    
    if(CI) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
             to spot 95% CI with multiple strata')
      }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
        sp <- sp + geom_step(aes(y = up, color = group), lty = lty.ci) +
          geom_step(aes(y = low, color = group), lty = lty.ci)
      } else{
        sp <- sp +  geom_step(aes(y = up, lty = group), col = surv.col) +
          geom_step(aes(y = low,lty = group), col = surv.col)
      }   
    }
    
    
    if(spot.cens == T & length(dat.cens) > 0){
      sp <- sp + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                            col = cens.col)
    } else if (spot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations') 
    }
    
    if(black.white == T) {
      sp <- sp + theme_bw()
    } 
    sp
  } 
  if(strata == 1) {
    sp <- ggsurv.s() 
  } else {
    sp <- ggsurv.m()
  }
  sp
}