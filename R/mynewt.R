#' Newton-Raphson Algorithm
#'
#' @param x0      X-not first approximation value
#' @param delta   increment, try 0.001
#' @param f       Function
#' @param fdash   Function derivative
#' @description
#' The Newton-Raphson Algorithm, which can be used to
#'    find the roots of a function.
#'
#'
#' @return
#' @export
#'
#' @examples
#' mynewt(x0=10,delta=0.000001,f=function(x) x^2-4,fdash=function(x) 2*x )
mynewt=function(x0,delta=0.001,f,fdash){
  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > delta & i<10000){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i])
  }
  #windows()
  graphics::curve(f(x),xlim=range(c(range(x),-range(x))),xaxt="n", main="Newton-Raphson Algorithm")
  graphics::points(x,y,col="Red",pch=19,cex=1.5)
  graphics::axis(1,x,round(x,2),las=2)
  graphics::abline(h=0,col="Red")

  graphics::segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  graphics::segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Pink")

  list(x=x,y=y)
}
