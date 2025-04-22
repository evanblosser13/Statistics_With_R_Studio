#' chi-square simualtion
#'
#' @param iter Number of iterations
#' @param n    Number of sample size
#'
#' @return
#' @export
#'
#' @examples
mychisim <- function(iter = 1000, n = 10){
  x<-NULL
  mat <- matrix(data=NA, nrow = n, ncol = iter, byrow=TRUE)

  for(i in 1:iter){
    mat[,i] <- stats::rnorm(n = n,mean = 0,sd = 1)^2

  }

  stat <- apply(mat,2,sum)

  h <- graphics::hist(stat, plot=FALSE)

  dd <- h$density
  cll <- dd/max(dd)

  graphics::hist(stat, freq = FALSE, col = grDevices::rgb(0,0,cll))

  graphics::curve(stats::dnorm(x, mean = n, sd = sqrt(2*n)),add=TRUE,lwd = 2, col= "Red")

}


