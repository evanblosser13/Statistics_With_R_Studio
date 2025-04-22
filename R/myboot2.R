#' Bootstrap 2.0
#'
#' @param iter  Iterations desired
#' @param x     Vector of sample data
#' @param fun   Function to be applied
#' @param alpha Alpha value: (1-alpha)*100 is the confidence interval
#'              percentage
#' @param cx    Confidence Interval text on graph
#' @param ...   Other parameters
#' @description
#' This is a bootstrap function designed to find the confidence interval of a
#' given set of sample data. This graphs the output and places the confidence
#' interval on the graph. It also prints a list of significant values to the user
#' pertaining to the given data; such as the confidence interval "ci", the
#' function applied "fun", etc...
#'
#' @returns A graph and a list of significant values.
#' @export
#'
#' @examples
#' myboot2<-function(iter=10000,x=c(1,2,2,3,3,3,4),fun="mean",alpha=0.05,cx=1.5)
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=stats::quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=graphics::hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  # Plot Settings
  graphics::abline(v=pte,lwd=3,col="Black")# Vertical line
  graphics::segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  graphics::text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  graphics::text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  graphics::text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(xstat=xstat,ci=ci,fun=fun,x=x))# Some output to use if necessary
}
