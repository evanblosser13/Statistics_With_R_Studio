#' myf - " quadratic linear model
#'
#' @param x     Set an X intercept
#' @param coef  Input a list of coefficients of a linear! model
#'
#' @return
#' @export
#'
#' @examples
myf = function(x,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-18)*(x-18>0)
}
