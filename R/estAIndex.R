#' Estimate Index of Sea Lamprey Adults
#'
#' Estimate the Adult Index of sea lampreys in a single Great Lake.
#' @param indexStreams
#'   A numeric vector of IDs identifying streams to be included
#'   in the index, e.g., 1.064 = lake code + (stream code)/1000.
#' @param streamPECurr
#'   A data frame of stream mark-recapture estimates from the current year,
#'   with variables: year, lake, lscode, Emr, CVmr.
#' @param streamPEPrev
#'   A data frame of stream mark-recapture and Adult Index estimates
#'   from previous years, with variables: year, lake, lscode, Emr, CVmr,
#'   indexContrib, default NULL.
#' @param minNMR
#'   A numeric scalar, the minimum number of mark-recapture estimates
#'   needed in a year to generate an index, default 2.
#' @param show
#'   A logical Scalar indicating if a brief summary of the results should
#'   by printed, default FALSE.
#'
#' @return
#'   A list with two components:
#'   streamPE, a data frame of stream mark-recapture and Adult Index estimates
#'   from previous and current years combined, with the same variables as
#'   \code{streamPEPrev}; and
#'   lakeIndex, a numeric matrix with three columns, the Adult Index,
#'   and the lower and upper jackknifed range.
#' @import
#'   plyr
#' @export
#' @details
#'   The annual Adult Index is simply the sum of the columns in \code{m} for
#'   each row.  The jackknifed range is produced by recalculating the index,
#'   leaving out one stream at a time, then scaling up the result to the same
#'   scale as the Adult Index based on all streams.
#' @examples
#' streampe <- matrix(1:12, nrow=3, dimnames=list(1996:1998, letters[1:4]))
#' jackIndex(streampe)

estAIndex <- function(indexStreams, streamPECurr, streamPEPrev=NULL, minNMR=2,
  show=FALSE) {

  varShort <- c("year", "lake", "lscode", "trapcatch", "Emr", "CVmr")
  varLong <- c("year", "lake", "lscode", "trapcatch", "Emr", "CVmr",
      "indexContrib")
  if (any(is.na(match(varShort, names(streamPECurr))))) {
    stop("streamPECurr must include these variables:",
      paste(varShort, collapse=", "), ".")
  }
  streamPECurr$complete <- FALSE

  if(!is.null(streamPEPrev)) {
    # combine the previous data with the current data
    if (any(is.na(match(varLong, names(streamPEPrev))))) {
      stop("streamPEPrev (if not NULL) must include these variables:",
        paste(varLong, collapse=", "), ".")
    }
    streamPEPrev$complete <- TRUE
    streamPE <- rbind.fill(streamPEPrev[, c(varLong, "complete")],
      streamPECurr[, c(varShort, "complete")])
  } else {
    streamPE <- streamPECurr[, c(varShort, "complete")]
  }

	selstreams <- streamPE$lscode %in% indexStreams

  check1 <- var(streamPE$lake[selstreams])
	if (is.na(check1) | is.null(check1)) {
    stop("Either no streams selected or critical data missing.")
  } else {
    if (check1 > 0) {
      stop("Selected streams should be only from ONE lake.")
    }
  }

  # set index contributions for new data to Emr
  streamPE$indexContrib[!streamPE$complete & selstreams] <-
    streamPE$Emr[!streamPE$complete & selstreams]
  # fill in missing contributions in new data
  incompMiss <- !streamPE$complete & is.na(streamPE$Emr) & selstreams
  if(any(incompMiss)) {
    sub <- streamPE[selstreams, ]
  	indfit <- with(sub, aov(log(Emr) ~ as.factor(lscode) + as.factor(year),
      weights=1/CVmr^2))
  	# figure out estimable years (those with at least minNMR m-r estimate)
  	n.mr <- tapply(!is.na(sub$Emr), sub$year, sum)
  	eyrs <- as.numeric(names(n.mr)[n.mr > (minNMR - 0.5)])

  	estimable <- streamPE$year %in% eyrs
  	Pmr <- rep(NA, length(estimable))
  	Pmr[estimable & selstreams] <-
        predAntilog(aovfit=indfit, xdata=streamPE[estimable & selstreams, ])
    streamPE$indexContrib[incompMiss] <- Pmr[incompMiss]
  }

  streamests <- with(streamPE[selstreams, ],
    tapply(indexContrib, list(year, lscode), mean))
  # get rid of years with missing estimates
  se2 <- streamests[!apply(is.na(streamests), 1, any), ]
  # THEN, get rid of streams with missing estimates
  se3 <- se2[, !apply(is.na(se2), 2, any)]
  jack <- jackIndex(se3)
  jack <- cbind(lake=streamPE$lake[1], year=as.numeric(row.names(jack)), jack)
  row.names(jack) <- 1:dim(jack)[1]

  if (show) {
  	cat("\nContribution of stream PEs to Adult Index from most recent year\n")
  	print(streamPE[streamPE$year == max(streamPE$year), varLong])
  	cat("\nLast five years of Adult Index estimates\n")
  	print(tail(jack, 5))
	}
  list(streamPE=streamPE[, varLong], lakeIndex=jack)
}



if(FALSE) {

	# annual index (sum across streams)
	indxdf <- aggregate(COMBmr ~ year + lake, streamPE, sum, na.rm=TRUE,
    na.action=na.pass)
	names(indxdf)[names(indxdf)=="COMBmr"] <- "indxraw"
	indxdf$indxraw[indxdf$indxraw==0] <- NA
	# only keep lake-wide index for years with at least minNMR mark-recap ests
	indxdf$n.mr <- n.mr
	indxdf$indxkeep <- ifelse(indxdf$n.mr > (minNMR - 0.5), indxdf$indxraw, NA)
	indxdf$indxkeep.lo <- NA
	indxdf$indxkeep.hi <- NA

	# matrix of stream estimates (rows=years, columns=index streams)
	streamests <- with(streamPE, tapply(COMBmr, list(year, lscode), mean))
	# selection of only those streams with a keepable index
	selkeep <- !is.na(indxdf$indxkeep)





	jack <- jackIndex(streamests[selkeep, ])
	indxdf$indxkeep.lo[selkeep] <- jack[, "lo"]
	indxdf$indxkeep.hi[selkeep] <- jack[, "hi"]

	# scale up the index to the spawner model PE
	lk1 <- lk[lk$lake==streamPE$lake[1], ]
	indxdf2 <- merge(lk1, indxdf, all=TRUE)
	scaleup <- median(indxdf2$PE / indxdf2$indxkeep, na.rm=TRUE)

	if (show) {
		cat("\nindfit\n")
		print(summary(indfit))
		cat("\nstreamPE\n")
		print(tail(streamPE[,
      c("lake", "year", "lscode", "Emr", "CVmr", "Pmr", "COMBmr")]))
		cat("\nscaleup\n")
		print(scaleup)
		cat("\nindxdf\n")
		print(tail(indxdf[, c("lake", "year", "n.mr", "indxraw", "indxkeep",
      "indxkeep.lo", "indxkeep.hi")]))
		}
	list(indfit=indfit, streamPE=streamPE, scaleup=scaleup, indxdf=indxdf)
	}
