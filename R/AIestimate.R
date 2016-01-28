#' Estimate Index of Sea Lamprey Adults
#'
#' Estimate the Adult Index of sea lampreys in a single Great Lake.
#' @param streamDat
#'   A data frame of old and new stream mark-recapture estimates
#'   used to estimate the lake-wide Adult Indices,
#'   typically the output from \code{\link{AIprep}}.  The data frame must
#'   include: \code{year},
#'   \code{lake}, lake-stream ID \code{lscode}
#'   (see details), population estimate
#'   \code{PEmr}, coefficient of variation \code{CVmr}
#'   (100\% * sqrt(variance(PEmr)) / PEmr), \code{index}, a logical
#'   identifying the index streams; \code{maintain} a logical identifying the
#'   streams that will continue to have ongoing trapping even if not part of
#'   the Adult Index; \code{indexContrib} a numeric, the stream population
#'   estimate that will be used in the Adult Index (NA for new); and
#'   \code{complete} a logical identifying streams and years for which the
#'   Adult Index has already been estimated (FALSE for new).
#' @param minNMR
#'   An integer scalar greater than or equal to 2,
#'   the minimum number of mark-recapture estimates
#'   needed in a year to generate an index, default 2.
#'
#' @return
#'   A list with 3 components:
#'   \describe{
#'     \item{streamPE}{a data frame of stream mark-recapture and Adult Index
#'   contributions for the incomplete rows in \code{streamDat}, with
#'   the same variables as \code{streamDat};}
#'     \item{lakeIndex}{a data frame of annual lake-wide Adult Indices
#'   with 5 columns: \code{lake}, \code{year}, the Adult Index \code{index},
#'   and the lower and upper jackknifed range \code{jlo} and \code{jhi}; and}
#'     \item{lakeJackRaw}{a data frame of the raw contributions to the jackknifed
#'   range with columns for \code{lake}, \code{year}, and each of the index
#'   streams (see \code{\link{jackIndex}}).}
#'   }
#' @export
#' @details
#'   The annual Adult Index is simply the sum of stream population estimates for
#'   each year.  Missing stream estimates are estimated by a lake-specific
#'   ANOVA model relating the log of the stream estimates to the main effects
#'   of each stream and each year, weighted by the inverse of the CV squared.
#'   The jackknifed range is produced by recalculating the index,
#'   leaving out one stream at a time, then scaling up the result to the same
#'   scale as the Adult Index based on all streams.

AIestimate <- function(streamDat, minNMR=2) {

#   library(GLFC)
#   library(plyr)
#   streamDat=streamIncomp[streamIncomp$lake==1, ]
#   minNMR=2

  # keep track of who started off as incomplete
  incomp <- with(streamDat, !complete)

  # set index contributions for new data to PEmr
  incompindex <- with(streamDat, !complete & index)
  streamDat$indexContrib[incompindex] <- streamDat$PEmr[incompindex]

  # fill in missing contributions in new data
  incompiMiss <- with(streamDat, !complete & index & is.na(PEmr))
  if (any(incompiMiss)) {
    sub <- streamDat[streamDat$index, ]
    indfit <- with(sub,
      aov(log(PEmr) ~ as.factor(lscode) + as.factor(year), weights=1/CVmr^2)
      )
    # figure out estimable years (those with at least minNMR m-r estimate)
    n.mr <- tapply(!is.na(sub$PEmr), sub$year, sum)
    eyrs <- as.numeric(names(n.mr)[n.mr > (minNMR - 0.5)])
    estimable <- streamDat$year %in% eyrs
    Pmr <- rep(NA, length(estimable))
    Pmr[estimable & streamDat$index] <- predAntilog(aovfit=indfit,
      xdata=streamDat[estimable & streamDat$index, ])
    streamDat$indexContrib[incompiMiss] <- Pmr[incompiMiss]
  }

  # mark as complete
  streamDat$complete[incomp] <- TRUE

  # arrange estimates in a matrix
  streamests <- with(streamDat[streamDat$index, ],
    tapply(indexContrib, list(year, lscode), mean))
  # get rid of years with missing estimates
  se2 <- streamests[!apply(is.na(streamests), 1, any), ]
  # THEN, get rid of streams with missing estimates
  se3 <- se2[, !apply(is.na(se2), 2, any)]
  jacklist <- jackIndex(se3, simple=FALSE)
  jack <- cbind(lake=streamDat$lake[1],
    year=as.numeric(row.names(jacklist$jack.range)), jacklist$jack.range,
    jacklist$jack.raw)
  row.names(jack) <- 1:dim(jack)[1]
  jack <- as.data.frame(jack)

  # subset the output to only include the year-lakes in streamDatCurr
  uyl <- with(streamDat[incompindex, ], unique(paste0(year, lake)))
  streamDatOut <- streamDat[with(streamDat, paste0(year, lake)) %in% uyl, ]
  jsel <- with(jack, paste0(year, lake)) %in% uyl
  lakeIndexOut <- jack[jsel, c("lake", "year", "index", "jlo", "jhi")]
  lakeIndexRaw <- jack[jsel,
    c("lake", "year", dimnames(jacklist$jack.raw)[[2]])]
  list(streamPE=streamDatOut, lakeIndex=lakeIndexOut, lakeJackRaw=lakeIndexRaw)
}
