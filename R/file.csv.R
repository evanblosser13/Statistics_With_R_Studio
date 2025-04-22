#' Load a .csv File
#'
#' @param obj.name object name desired
#'
#' @return  an object assigned to a.csv
#' @export
#'
#' @examples
file.csv=function(obj.name){
  obj.name<-utils::read.csv(file.choose())
  return(obj.name)
}
