#' Read csv
#'
#' @param csv  Nme of desired file
#' @param dird Directory of desired file
#'
#' @return
#' @export
#'
#' @examples
myread=function(csv, dird){
  fl=paste(dird,csv,sep="")
  utils::read.table(fl,header=TRUE,sep=",")
}
