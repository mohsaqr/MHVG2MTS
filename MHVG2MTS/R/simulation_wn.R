#' Simulate a bivariate white noise process
#'
#' @param t The length of the time series.
#' @param cov_mat A covariance matrix.
#' @return A matrix with the simulated white noise process.
#' @importFrom mvtnorm rmvnorm
#' @export
simulate_wn <- function(t, cov_mat = cbind(c(1, 0), c(0, 1))) {
  rmvnorm(t, sigma = cov_mat)
}
