#' Title
#'
#' @param N     Number of tickets
#' @param gamma Gamma
#' @param p     Probability
#'
#' @return
#' @export
#'
#' @examples
ntickets <- function(N, gamma, p) {
  # Define the objective function for the discrete case
  objective_function_discrete <- function(n) {
    return(1 - gamma - stats::pbinom(N - n, N, p))
  }

  # Define the objective function for the continuous case
  objective_function_continuous <- function(n) {
    mu <- N * p
    sigma <- sqrt(N * p * (1 - p))
    return(1 - gamma - stats::pnorm(N - n, mu, sigma))
  }

  # Find the number of tickets using the discrete distribution
  nd_opt <- stats::optimize(function(n) abs(objective_function_discrete(n)), interval = c(0, N))$minimum
  # Round The output
  nd <- round(nd_opt,2)

  # Find the number of tickets using the normal approximation
  nc_opt <- stats::optimize(function(n) abs(objective_function_continuous(n)), interval = c(0, N))$minimum
  # Round The output
  nc <- round(nc_opt,2)

  # Create the objective function for the discrete case
  objective_values_discrete <- sapply(0:N, objective_function_discrete)

  # Create the objective function for the continuous case
  objective_values_continuous <- sapply(0:N, objective_function_continuous)
  graphics::layout(matrix(1:2))
  # Plot objective function for the discrete case
  plot(0:N, objective_values_discrete, type = "b", col = "blue",
       xlab = "n (Number of Tickets to be Sold)", ylab = "Objective",
       main = "Objective Function vs n")




  # Plot objective function for the continuous case
  plot(0:N, objective_values_continuous,xlab = "n (Number of Tickets to be Sold)",ylab="Objective", type = "l", col = "purple")



  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
