#' Determining Status and Trends
#'
#' Determine the status and trends of a time series relative to a target.
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
#' @return
#'   A data frame with one row per bydat group, giving the
#'   time span, mean, target, and response for the status and the
#'   time span, slope, P value, and response for the trend.
#'   Column names: "bydat", "stspan", "stmean", "targdat", "status",
#'   "trspan", "slope", "pv", "trnd".
#' @importFrom magrittr %>%
#' @importFrom broom tidy
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @import dplyr
#' @export
#' @examples
#'  rawdat <- data.frame(year=1990+c(1:6, 1:6), group=rep(1:2, c(6, 6)),
#'   x=c(1:10, 9, 7)*10)
#'  targetdat <- data.frame(group=1:2, targ=c(30, 140))
#'  SRstatus(bydat=rawdat$group, timedat=rawdat$year,
#'   measdat=rawdat$x, targdat=targetdat$targ)

SRstatus <- function(bydat, timedat, measdat, targdat,
	bytar=sort(unique(bydat)), status.length=3, trend.length=5) {

  # error checking
	if (!is.null(trend.length)) {
	  if(trend.length < 2.5 |
      abs(as.integer(trend.length)-trend.length)>0.0000001)
  		stop("trend.length must be an integer > 2")
	}

  looky <<- list(bydat, timedat, measdat)

	# set up data
	meas <- data.frame(bydat, timedat, measdat, stringsAsFactors=FALSE)

	# latest year of data available
	out <- meas %>%
	  filter(!is.na(measdat)) %>%
	  dplyr::group_by(bydat) %>%
 	  top_n(1, timedat)

	keepvars <- "bydat"

	rangechar <- function(x) {
	  rng <- range(x, na.rm=TRUE)
	  if(diff(rng)==0) as.character(rng[1]) else paste(rng, collapse="-")
	}

	if (!is.null(status.length)) {
  	targ <- data.frame(bydat=bytar, targdat, stringsAsFactors=FALSE)
  	out <- 	meas %>%
  	  filter(!is.na(measdat)) %>%
  	  dplyr::group_by(bydat) %>%
  	  top_n(status.length, timedat) %>%
  	  mutate(
  	    stspan=rangechar(timedat)
  	  ) %>%
  	  dplyr::group_by(bydat, stspan) %>%
  	  summarise(stmean=mean(measdat)) %>%
  	  full_join(targ, by="bydat") %>%
  	  mutate(
  	    status=ifelse(stmean < targdat, "Met", "Above")
  	  )
		keepvars <- unique(c(keepvars,
		  "bydat", "stspan", "stmean", "targdat", "status"))
	}

	if (!is.null(trend.length)) {
  	trnd <- meas %>%
  	  filter(!is.na(measdat)) %>%
  	  dplyr::group_by(bydat) %>%
  	  top_n(trend.length, timedat) %>%
  	  mutate(
  	    trspan=rangechar(timedat)
  	  ) %>%
  	  ungroup() %>%
  	  tidyr::nest(data=c(timedat, measdat)) %>%
  	  mutate(
  	    fit = purrr::map(data, ~ broom::tidy(lm(measdat ~ timedat, data = .)))
  	  ) %>%
  	  tidyr::unnest(fit) %>%
  	  filter(term=="timedat") %>%
  	  rename(slope=estimate, pv=p.value) %>%
  	  mutate(
  	    trend = case_when(
  	      pv < 0.05 & slope < 0 ~ "Decr.",
  	      pv < 0.05 & slope > 0 ~ "Incr.",
  	      TRUE ~ "Steady"
  	    )
  	  )
  	out <- trnd %>%
  	  full_join(out, by="bydat")
 		keepvars <- unique(c(keepvars, "bydat", "trspan", "slope", "pv", "trend"))
	}
	out[, keepvars]
}
