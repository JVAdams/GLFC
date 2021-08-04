#' Unbiased Prediction of Natural Log Transformed Response on Original Scale
#'
#' Provide unbiased estimates on the original scale from a linear
#' model with a natural log-transformed response.
#' @param modfit
#'   A fitted model object from a call to \code{\link{lm}}, \code{\link{aov}},
#'   or \code{\link{glm}}.
#' @param xdata
#'   A data frame with predictor variables corresponding to those in
#'   \code{modfit} for which predictions should be made.
#' @param k
#'   A numeric scalar, the constant added to the response prior to
#'   transformation, default 0.
#' @return
#'   A list with two numeric vectors, one with predicted values \code{pred}
#'   and one with standard deviation \code{sdpred} of those predictions,
#'   both on the original scale of the response.
#' @export
#' @references
#' Mood, AM, FA Graybill, DC Boes. 1974.
#'  Introduction to the Theory of Statistics.
#'  McGraw-Hill, New York.
#' @examples
#' fit <- aov(log(yield) ~ block + N * P + K, data=npk)
#' predAntilognorm(fit, npk)

predAntilognorm <- function(modfit, xdata, k=0) {
  sigma2 <- sigma(modfit)^2
  mu <- predict(modfit, newdata=xdata)
  pred <- exp(mu + sigma2/2) - k
  varpred <- exp(2*mu + 2*sigma2) - exp(2*mu + sigma2)
  list(pred=pred, sdpred=sqrt(varpred))
}
