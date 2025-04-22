#' Negative Binomial Probability
#'
#' @param y Number of trials until the rth success is observed
#' @param r rth success
#' @param p probability of success on a single Bernoulli trial
#'
#' @return The probability distribution for a Negative Binomial of random variable Y
#' @export
#'
#' @examples
#' #P(Y=10), Y~NegBin(p=0.4,r=3)
#' neg.bi.nom(10,3,0.4)
neg.bi.nom=function(y,r,p){


  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
