#' Random Data Selection
#'
#' @param data.object   A data frame/csv
#' @param n             Number of desired rows
#' @description
#' Selects a specific number of random rows from a list of data.
#'
#'
#' @return          An object data.select, with the randomly selected data.
#' @export
#'
#' @examples
rand.data.select = function(data.object,n){
  data.legnth <- nrow(data.object)
  Random.Data=sample(1:data.legnth,n,replace=FALSE)
  data.select <- data.object[Random.Data,]
  return(data.select)
}
