#' Simulate a bivariate VAR process
#'
#' @param t The length of the time series.
#' @param const A vector of intercept terms.
#' @param phi A matrix of AR coefficients.
#' @param cov_mat A covariance matrix.
#' @param burnin The length of the burn-in period.
#' @return A matrix with the simulated VAR process.
#' @importFrom mAr mAr.sim
#' @export
simulate_var <- function(t, const, phi, cov_mat, burnin = 500) {
  res <- mAr.sim(w = const, A = phi, C = cov_mat, N = t + burnin)
  as.matrix(res[(burnin + 1):(t + burnin), ])
}
