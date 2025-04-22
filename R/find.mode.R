#' Finding the Mode of a Data set
#'
#' @param x input
#'
#' @return the mode of a data set
#' @export
#'
#' @examples
#' find.mode(c(1,2,3,3,4,5))
find.mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
