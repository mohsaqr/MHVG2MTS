#' Simulate a bivariate GARCH process
#'
#' @param t The length of the time series.
#' @param omega A vector of constants in the GARCH equation.
#' @param alpha An ARCH parameter matrix in the GARCH equation.
#' @param beta A GARCH parameter matrix in the GARCH equation.
#' @param varcov A constant conditional correlation matrix.
#' @return A matrix with the simulated GARCH process.
#' @importFrom ccgarch eccc.sim
#' @export
simulate_garch <- function(t, omega, alpha, beta, varcov) {
  res <- eccc.sim(
    nobs = t + 200,
    a = omega,
    A = alpha,
    B = beta,
    R = varcov,
    d.f = 5,
    model = "diagonal"
  )$eps

  res[201:(t + 200), ]
}
