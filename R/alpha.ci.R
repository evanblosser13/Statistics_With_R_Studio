#' Confidence Interval Estimator
#'
#' @param per    Percentage of confidence
#' @param sample A vector of sample data
#' @description
#' This is a function that takes a desired percentage of
#' confidence, ie a 94\% confidence interval, and a vector of sample data as inputs.
#' Then the confidence interval is calculated using theory from Statistics for
#' Engineering and the Sciences 6th edition.
#'
#'
#' @return       A confidence interval for the given data
#' @export
#'
#' @examples
alpha.ci = function(per,sample){
  if(per > 1){
    print(paste0("You entered the percentage ",per,"%"))
    x.deci = per/100
  }else{
    print(paste0("You entered the decimal form of the percent ",per*100,"%"))
    x.deci = per
  }
  # Initial Calculations ###
  alpha  = 1-x.deci
  mu      = mean(sample)
  sigma   = stats::sd(sample)
  sam.siz = length(sample)
  z.alpha = stats::qnorm(1-alpha/2,mean=0,sd=1)
  # Confidence interval
  confidence.interval= list(c(Low.end= mu-z.alpha*(sigma/sqrt(sam.siz)),
                              upper.end= mu+z.alpha*(sigma/sqrt(sam.siz))))
  return(confidence.interval)
}
