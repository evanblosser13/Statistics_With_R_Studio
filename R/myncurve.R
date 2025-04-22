#' Normal Curve Probability Distribution
#'
#' @param mu     mean
#' @param sigma S.D.
#' @param a     user input
#'
#' @return A graph representing probabiltiy through area
#' @export
#'
#' @examples
#' myncurve(mu=4, sigma=2, a=5)
myncurve = function(mu, sigma, a){
  x<-NULL
  graphics::curve(stats::dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma),col="purple")
  # Create Curves
  xcurve=seq(-100,a,length=1000)
  ycurve=stats::dnorm(xcurve,mean=mu,sd=sigma)
  # Create Polygon Volume
  graphics::polygon(c(-100,xcurve,a),c(0,ycurve,0),col="cyan")
  # Probability
  prob=stats::pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  graphics::text(x=a,y=.05, paste("Area = ", prob, sep=""))
}
