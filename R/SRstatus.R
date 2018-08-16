#' Determine Status and Trend
#'
#' Determine the status, relative to a target, and the trend, adjusted for
#' autocorrelation, of a time series.
#'
#' @param bydat
#'   A vector of grouping variables.
#' @param timedat
#'   A time variable, same length as \code{bydat}.
#' @param measdat
#'   A numeric vector, the estimate of interest, same length as \code{bydat}.
#' @param targdat
#'   A numeric vector, the target of interest, same length as \code{bytar}.
#' @param bytar
#'   A vector of unique grouping variables, default \code{sort(unique(bydat))}.
#' @param status.length
#'   A numeric scalar, the number of time units (the length of time) over
#'   which the mean status should be assessed, default 3.  If set to NULL,
#'   status will not be evaluated.
#' @param trend.length
#'   A numeric scalar, the number of time units (the length of time) over
#'   which the trend should be assessed, default 5.  If set to NULL,
#'   trend will not be evaluated.
#' @param response.stat
#'   A character vector giving the responses for estimates below and at or
#'   above target, default c("Met", "Above")
#' @param response.trend
#'   A character vector giving the responses for estimates with decreasing,
#'   steady, or increasing trends, default
#'   c("Decr.", "Steady", "Incr.")
#' @return
#'   A data frame with one row per bydat group, giving the
#'   time span, mean, target, and response for the status and the
#'   time span, slope, P value, and response for the trend.
#'   Column names: "bydat", "stspan", "stmean", "targdat", "status",
#'   "trspan", "slope", "pv", "trnd".
#' @details
#'   Significant trends in the time series are tested after automatically
#'   adjusting for first order autocorrelation using the iterative
#'   Cochrane–Orcutt procedure.  Verbeek (2004) reports that the iterated
#'   estimated generalized least squares (EGLS) typically performs somewhat
#'   better than its two-step variant in small samples (pages 100-101).
#' @seealso \code{\link{cochrane.orcutt.jva}}
#' @references
#'   Verbeek M.  2004.  A Guide to Modern Econometrics.
#'     John Wiley & Sons Ltd, West Sussex.
#' @importFrom orcutt summary.orcutt
#' @export
#' @examples
#' rawdat <- data.frame(year=1990+c(1:6, 1:6), group=rep(1:2, c(6, 6)),
#'   y=c(arima.sim(n=6, list(ar=0.2)), arima.sim(n=6, list(ar=0.8))))
#' targetdat <- data.frame(group=1:2, targ=c(0, -1))
#' SRstatus(bydat=rawdat$group, timedat=rawdat$year,
#'   measdat=rawdat$y, targdat=targetdat$targ)

SRstatus <- function(bydat, timedat, measdat, targdat,
	bytar=sort(unique(bydat)), status.length=3, trend.length=5,
  response.stat=c("Met", "Above"),
  response.trend=c("Decr.", "Steady", "Incr.")) {

  # error checking
	if (!is.null(trend.length)) {
	  if(trend.length < 2.5 |
      abs(as.integer(trend.length)-trend.length)>0.0000001)
  		stop("trend.length must be an integer > 2")
	}
	if (length(response.stat) != 2 | length(response.trend) != 3)
		stop("response.stat and response.trend must be of length 2 and 3")

	# set up data
	meas <- data.frame(cbind(bydat, timedat, measdat))

	# information from latest year of data available
	out <- aggregate(timedat ~ bydat, dat=meas[!is.na(measdat), ], max)
	keepvars <- "bydat"

	if (!is.null(status.length)) {
  	targ <- data.frame(cbind(bydat=bytar, targdat))
  	out <- merge(out, targ)
		sub <- sort(unique(bydat))
		stat <- data.frame(bydat=rep(sub, rep(status.length, length(sub))),
			timedat=unlist(lapply(out$timedat,
			function(mx) seq(mx-status.length+1, mx, 1))))
		if(status.length > 1) {
  		out$stspan <- tapply(stat$timedat, stat$bydat, function(x)
        paste(range(x), collapse="-"))
		} else {
  		out$stspan <- tapply(stat$timedat, stat$bydat, function(x)
        as.character(max(x)))
		}
		stat <- merge(stat, meas)
		statagg <- aggregate(measdat ~ bydat, data=stat, mean)
		out$stmean <- statagg$measdat
		out$status <- ifelse(out$stmean <= out$targdat,
		  response.stat[1], response.stat[2])
		keepvars <- unique(c(keepvars,
		  "bydat", "stspan", "stmean", "targdat", "status"))
	}

	if (!is.null(trend.length)) {
		sub <- sort(unique(bydat))
		trnd <- data.frame(bydat=rep(sub, rep(trend.length, length(sub))),
			timedat=unlist(lapply(out$timedat,
			function(mx) seq(mx-trend.length+1, mx, 1))))
		out$trspan <- tapply(trnd$timedat, trnd$bydat, function(x)
      paste(range(x), collapse="-"))
		trnd <- merge(trnd, meas)
		coefs <- vector("list", length(sub))
		for(i in seq(sub)) {
		  lmfit <- lm(measdat ~ timedat, trnd[trnd$bydat==i, ])
		  cofit <- cochrane.orcutt.jva(lmfit)
		  if(sum(is.na(cofit$coefficients))>0) {
		    coefs[[i]] <- c(NA, NA)
	    } else {
  		  coefs[[i]] <- summary(cofit)$coef[2, c(1, 4)]
	    }
		}
		out$slope <- sapply(coefs, "[", 1)
		out$pv <- sapply(coefs, "[", 2)
		out$trend <- rep(response.trend[2], length(sub))
		out$trend[!is.na(out$pv) & out$pv < 0.05 & out$slope < 0] <-
		  response.trend[1]
		out$trend[!is.na(out$pv) & out$pv < 0.05 & out$slope > 0] <-
		  response.trend[3]
		out$trend[is.na(out$pv)] <- "?"
		keepvars <- unique(c(keepvars, "bydat", "trspan", "slope", "pv", "trend"))
	}
	out[, keepvars]
}
