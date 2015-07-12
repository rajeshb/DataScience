#########################################
##                                     ##
## R basic functions                   ##
## Jelmer Borst                        ##
## jelmerborst@gmail.com               ##
## 090112                              ##
##                                     ##
#########################################

print.functions <- function(){
        cat("Rounding etc.:\n",sep="")
        cat("---------\n",sep="")
        cat("roundup(x, numdigits=0) - correct rounding of .5 etc.\n",sep="")
        cat("round.largest(x) - round to largest digit, i.e., 54 -> 50 \n",sep="")
        cat("ceiling.largest(x) - ceiling to largest digit, i.e., 54 -> 60\n",sep="")
        cat("is.odd(x) - returns TRUE if roundup(x) is an odd number\n",sep="")
        cat("is.even(x) - returns TRUE if roundup(x) is an even number\n",sep="")
        cat("get.num(x) - returns numeric values of x\n\n",sep="")
        
        
        cat("Standard errors, error bars, rmsd etc:\n",sep="")
        cat("--------------------------------------\n",sep="")
        cat("se(x) - standard error\n",sep="")
        cat("rmsd(x) - root mean squared deviation\n",sep="")
        cat("errbar(x,y,error,color=black) - plot error bars on (x,y)\n",sep="")
        cat("runmean(x,window) - running average of x with window, returns same length as x, with smoothed end points\n",sep="")
        cat("runmax(x,window) - running max of x with window, returns same length as x, with smoothed end points\n",sep="")
        cat("rollmin(x,window) - running min of x with window, edited version of rollmax from zoo package\n",sep="")
        cat("runmin(x,window) - running min of x with window, returns same length as x, with smoothed end points\n\n",sep="")
        
        
        cat("Statistics:\n",sep="")
        cat("-----------\n",sep="")
        cat("simple.partial.eta.squared(model,frame) - calculate partial eta squared for frame of anova model -- NOT ALWAYS CORRECT\n",sep="")
        cat("get.partial.etas(model) - calculate all partial eta squared's for anova model\n",sep="")
        cat("rm.levels(factor) - remove non-used levels from factor\n",sep="")
        cat("reject.z(x,index=NULL,threshold=2) - reject x's, split over index, with z > threshold\n",sep="")
        cat("make.z(x,index=NULL) - get z scores, split over index\n",sep="")
        cat("replace.z(x,index=NULL,threshold=2) - replace x's with threshold, split over index, with z > threshold\n\n",sep="")
        
        
        cat("Misc.:\n",sep="")
        cat("------\n",sep="")
        cat("h(x,...) - shortcut for head(x,...), see ?head\n",sep="")
        cat("last(x) - get last element of vector, list, data.frame, etc.\n",sep="")
        cat("x %out% y - return T if x not in y (opposite of %in%)\n",sep="")
        cat("agg(x,index,function,name=x) - aggregate(x,index,function), name column x 'name'\n",sep="")
        cat("format.hrs.min.sec(seconds) - return hrs:min:sec or min:sec if sec < 3600\n",sep="")
        cat("performance(expr, samples=1, gcFirst=TRUE) - do 'samples' samples of function 'expr' to test its performance\n\n",sep="")
}

## Rounding etc.
##########################################################################################

#correct rounding of .5 etc.
roundup <- function(x,numdigits=0){
        x <- x * 10^numdigits
        x <- ifelse(x<0,-trunc(abs(x)+0.5),trunc(x+0.5))
        x / 10^numdigits
}

#round to largest 10's
round.largest <- function(x){
        x <- roundup(x)
        y <- 10^(nchar(as.character(x))-1)
        roundup(x / y) * y
}

#ceiling to largest 10's
ceiling.largest <- function(x){
        x <- roundup(x)
        y <- 10^(nchar(as.character(x))-1)
        ceiling(x / y) * y
}

#is x odd?
is.odd <- function(x){
        (roundup(x) %% 2 != 0)
}

#is x even?
is.even <- function(x){
        (roundup(x) %% 2 == 0)
}

#return numeric value of x
get.num <- function(x){
        if(is.factor(x)){
                as.numeric(as.character(x))
        }else{
                as.numeric(x)
        }
}


## Standard errors, error bars, rmsd etc:
##########################################################################################

#rmsd
rmsd <- function(data,model){
        sqrt(mean((data - model)^2))	
}

#standard error
se <- function(x){
        sd(x)/sqrt(length(x))
}

#test set
tmp <- data.frame(subj=rep(1:10,3), cond=rep(1:3, each = 10), x = c(10, 6, 11, 22, 16, 15, 1, 12, 9, 8, 13, 8, 14, 23, 18, 17, 1, 15, 12, 9, 13, 8, 14, 25, 20, 17, 4, 17, 12, 12))

#standard error within subjects (see Morey, 2008)
se.within <- function(dat,subvar='subj',conditions=list(cond=dat$cond),measvar ='x'){
        # se.within expects a dataframe with one datapoint per subj+cond. 
        # subvar indicates column with subject, conditions is a list with the condition definitions
        
        #normalize the data by subtracting the appropriate participants mean performance from each observation, and then add the grand mean score to every observation.
        # difference between subject mean and subjection condition + grand mean
        grandmean <- mean(dat[[measvar]])
        dat$z <- NA
        for(subj in unique(dat[[subvar]])){
                dat[dat[[subvar]] == subj,'z'] <- dat[dat[[subvar]] == subj,measvar] - mean(dat[dat[[subvar]] == subj,measvar]) + grandmean
        }
        dat <- with(dat, aggregate(list(se=z), conditions, se))
        
        #morey's fix:
        dat$se <- dat$se * sqrt(nrow(dat)/(nrow(dat)-1))
        dat
}



#draw error bars
errbar <- function(x,y,error,color="black"){
        arrows(x,y-error,x,y+error,angle=90,length=.05,code=3,col=color) 
}

#from plotCI
errbarPlus <- function (x, y = NULL, uiw, liw = uiw, ui, li, err = "y", ylim = NULL, 
                        xlim = NULL, type = "p", col = par("col"), barcol = col, 
                        pt.bg = par("bg"), sfrac = 0.01, gap = 1, lwd = par("lwd"), 
                        lty = par("lty"), labels = FALSE, add = FALSE, xlab, ylab, 
                        minbar, maxbar, ...) 
{
        if (is.list(x)) {
                y <- x$y
                x <- x$x
        }
        if (invalid(xlab)) 
                xlab <- deparse(substitute(x))
        if (invalid(ylab)) {
                if (is.null(y)) {
                        xlab <- ""
                        ylab <- deparse(substitute(x))
                }
                else ylab <- deparse(substitute(y))
        }
        if (is.null(y)) {
                if (is.null(x)) 
                        stop("both x and y NULL")
                y <- as.numeric(x)
                x <- seq(along = x)
        }
        if (err == "y") 
                z <- y
        else z <- x
        if (invalid(uiw)) 
                uiw <- NA
        if (invalid(liw)) 
                liw <- NA
        if (invalid(ui)) 
                ui <- z + uiw
        if (invalid(li)) 
                li <- z - liw
        if (!invalid(minbar)) 
                li <- ifelse(li < minbar, minbar, li)
        if (!invalid(maxbar)) 
                ui <- ifelse(ui > maxbar, maxbar, ui)
        if (err == "y") {
                if (is.null(ylim)) 
                        ylim <- range(c(y, ui, li), na.rm = TRUE)
                if (is.null(xlim) && !is.R()) 
                        xlim <- range(x, na.rm = TRUE)
        }
        else if (err == "x") {
                if (is.null(xlim)) 
                        xlim <- range(c(x, ui, li), na.rm = TRUE)
                if (is.null(ylim) && !is.R()) 
                        ylim <- range(x, na.rm = TRUE)
        }
        if (!add) {
                if (invalid(labels) || labels == FALSE) 
                        plot(x, y, ylim = ylim, xlim = xlim, col = col, xlab = xlab, 
                             ylab = ylab, ...)
                else {
                        plot(x, y, ylim = ylim, xlim = xlim, col = col, type = "n", 
                             xlab = xlab, ylab = ylab, ...)
                        text(x, y, label = labels, col = col, ...)
                }
        }
        if (is.R()) 
                myarrows <- function(...) arrows(...)
        else myarrows <- function(x1, y1, x2, y2, angle, code, length, 
                                  ...) {
                segments(x1, y1, x2, y2, open = TRUE, ...)
                if (code == 1) 
                        segments(x1 - length/2, y1, x1 + length/2, y1, ...)
                else segments(x2 - length/2, y2, x2 + length/2, y2, ...)
        }
        if (err == "y") {
                if (gap != FALSE){
                        gap <- strheight("O") * gap
                        gap <- sapply(uiw,function(x){ifelse(x <= gap,0,gap)})
                }
                
                smidge <- par("fin")[1] * sfrac
                if (!is.null(li)) 
                        myarrows(x, li, x, pmax(y - gap, li), col = barcol, 
                                 lwd = lwd, lty = lty, angle = 90, length = smidge, 
                                 code = 1)
                if (!is.null(ui)) 
                        myarrows(x, ui, x, pmin(y + gap, ui), col = barcol, 
                                 lwd = lwd, lty = lty, angle = 90, length = smidge, 
                                 code = 1)
        }
        else {
                if (gap != FALSE) 
                        gap <- strwidth("O") * gap
                smidge <- par("fin")[2] * sfrac
                if (!is.null(li)) 
                        myarrows(li, y, pmax(x - gap, li), y, col = barcol, 
                                 lwd = lwd, lty = lty, angle = 90, length = smidge, 
                                 code = 1)
                if (!is.null(ui)) 
                        myarrows(ui, y, pmin(x + gap, ui), y, col = barcol, 
                                 lwd = lwd, lty = lty, angle = 90, length = smidge, 
                                 code = 1)
        }
        points(x, y, col = col, lwd = lwd, lty = lty,bg = pt.bg, type = type, 
               ...)
        invisible(list(x = x, y = y))
}



##rolmean with smooth function
runmean <- function(x,window){
        require(zoo)
        ori <- x
        new <- rollmean(x,window,na.pad=T)
        new[is.na(new)] <- ori[is.na(new)]
        new <- smoothEnds(new,window)
        new
}

#rollmax with smooth function
runmax <- function(x,window){
        require(zoo)
        ori <- x
        new <- rollmax(x,window,na.pad=T)
        new[is.na(new)] <- ori[is.na(new)]
        new <- smoothEnds(new,window)
        new
}

#copy-pasted and edited from zoo
rollmin <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...)
{
        n <- length(x) 
        rval <- rep(0, n) 
        a <- 0
        for (i in k:n) {
                rval[i] <- if (is.na(a) || is.na(rval[i=1]) || a==rval[i-1]) 
                        min(x[(i-k+1):i]) # calculate max of window
                else 
                        min(rval[i-1], x[i]); # max of window = rval[i-1] 
                a <- x[i-k+1] # point that will be removed from window
        }
        rval <- rval[-seq(k-1)]
        if (na.pad) {
                rval <- switch(match.arg(align),
                               "left" = { c(rval, rep(NA, k-1)) },
                               "center" = { c(rep(NA, floor((k-1)/2)), rval, rep(NA, ceiling((k-1)/2))) },
                               "right" = { c(rep(NA, k-1), rval) })
        }
        return(rval)
} 

#rollmin with smooth function
runmin <- function(x,window){
        require(zoo)
        ori <- x
        new <- rollmin(x,window,na.pad=T)
        new[is.na(new)] <- ori[is.na(new)]
        new <- smoothEnds(new,window)
        new
}


## Statistics
##########################################################################################


## Calulates the partial eta squared for the nth frame of the model. 
## NOT ALWAYS CORRECT!!
simple.partial.eta.squared <- function(model,frame) {
        ss <- summary(model)[[frame]][[1]]$"Sum Sq"
        ss[1] / (ss[1] + ss[length(ss)])
}

#calculates all eta squareds for anova model
get.partial.etas <- function(model){
        require(gdata)
        n <- names(summary(model))
        cat("\nPartial eta squared values\n",sep="")
        cat("----------------------------\n\n",sep="")
        for(idx in 1:length(n)){
                m <- row.names(summary(model)[[idx]][[1]])
                ss <- summary(model)[[idx]][[1]]$"Sum Sq"
                if(length(ss)>1){
                        cat(n[idx],"\n",sep="")	
                        for(j in 1:(length(m)-1)){
                                eta <- ifelse(length(ss) < 2, NA, ss[j] / (ss[j] + last(ss)))
                                cat(m[j],": ",eta,"\n",sep="")
                        }
                        cat("---\n\n",sep="")
                }
        }
}

## rm.levels(factor) - remove non-used levels from factor\n",sep="")
rm.levels <- function(factor){
        as.factor(as.character(factor))
}



#for example: with(dat,reject.z(RT,list(cond,cond2,pp),z_score)))
reject.z <- function(x,index=NULL,threshold=2) {
        if (is.null(index)) {
                index <- rep(1,length(x))
        }
        z <- rep(NA,length(x))
        
        .reject.z <- function(v,data,threshold) {
                
                d <- data[v]
                mean.d <- mean(d)
                sd.d <- sd(d)
                
                if (sd.d>0) {
                        tmp.z <- (d - mean.d) / sd.d
                        
                        z[v] <<- d
                        z[v][abs(tmp.z)>threshold] <<- NA
                }
                
                return(NULL)
        }
        tapply(1:length(x),index,.reject.z,x,threshold)
        
        z
}

make.z <- function(x,index=NULL) {
        if (is.null(index)) {
                index <- rep(1,length(x))
        }
        z <- rep(NA,length(x))
        
        .make.z <- function(v,data) {
                d <- data[v]
                z[v][sd(d)>0] <<- (d - mean(d)) / sd(d)[sd(d)>0]
                return(NULL)
        }
        tapply(1:length(x),index,.make.z,x)
        z
}

replace.z <- function(x,index=NULL,threshold=2) {
        if (is.null(index)) {
                index <- rep(1,length(x))
        }
        z <- rep(NA,length(x))
        
        .replace.z <- function(v,data,threshold) {
                
                d <- data[v]
                mean.d <- mean(d)
                sd.d <- sd(d)
                
                if (sd.d>0) {
                        ma <- mean.d + (threshold * sd.d)
                        mi <- mean.d - (threshold * sd.d)
                        tmp.z <- (d - mean.d) / sd.d
                        
                        z[v] <<- d
                        z[v][tmp.z>threshold] <<- ma
                        z[v][tmp.z<(-1*threshold)] <<- mi
                }
                
                return(NULL)
        }
        tapply(1:length(x),index,.replace.z,x,threshold)
        z
}

## Misc
##########################################################################################

#shortcur for head: see ?head
h <- function(data, ...){
        head(data, ...)
}


#get last element of list, vector, etc
last <- function(x){
        x[length(x)]
}

#return TRUE for items not in y, opposite of %in%
"%out%" <- function(x,y){
        !(x %in% y)
}


#aggregate with 'naming the x'
agg <- function(x,index,fun,name="x"){
        tmp <- aggregate(x,index,fun)
        names(tmp)[ncol(tmp)] <- name
        tmp
}

#get hrs:min:sec from seconds
format.hrs.min.sec <- function(seconds){
        minutes <- seconds / 60
        if(minutes >= 60){
                hrs <- trunc(seconds / 3600)
                paste(hrs,":",sprintf("%02.0f",trunc(minutes) - (60*hrs),2),":",sprintf("%02.0f",roundup((minutes - trunc(minutes)) * 60,2)),sep="")
        }else{
                paste(trunc(minutes),":",sprintf("%02.0f",roundup((minutes - trunc(minutes)) * 60,2)),sep="")
        }
}

#do 'samples' samples of function 'expr' to test its performance
performance <- function(expr, samples=1, gcFirst=TRUE){
        
        loc.frame <- parent.frame()
        results <- data.frame()
        
        for(i in 1:samples){
                if (gcFirst){ gc(FALSE) }
                expr <- substitute(expr)
                time <- proc.time()
                eval(expr, envir = loc.frame)
                new.time <- proc.time()
                results <- rbind(results,as.vector(structure(new.time - time, class = "proc_time"))[1:3])
        }   
        
        test <<- results
        cat("\n\tAverage time per run:\n\t---------------------\n")
        cat("\tUser\t\tSystem\t\tElapsed\n")
        cat("\t",format(roundup(mean(results[1]),3),nsmall=3),"\t\t",format(roundup(mean(results[2]),3),nsmall=3),"\t\t",format(roundup(mean(results[3]),3),nsmall=3),sep="")
        
        cat("\n\n\tTotal time for all runs:\n\t------------------------\n")
        cat("\tUser\t\tSystem\t\tElapsed\n")
        cat("\t",format(roundup(sum(results[1]),3), nsmall=3),"\t\t",format(roundup(sum(results[2]),3),nsmall=3),"\t\t",format(roundup(sum(results[3]),3),nsmall=3),sep="")
        cat(" \n ")
        
}
