#' Estimate Index of Sea Lamprey Adults
#'
#' Estimate the Adult Index of sea lampreys in a single Great Lake.
#' @param streamDat
#'   A data frame of old and new stream mark-recapture estimates
#'   used to estimate the lake-wide Adult Indices,
#'   typically the output from \code{\link{AIprep}}.  The data frame must
#'   include these 10 columns:
#'   \itemize{
#'     \item \code{year};
#'     \item \code{lake};
#'     \item \code{lscode}, the lake-stream ID (see details);
#'     \item \code{PEmr}, the population estimate;
#'     \item \code{CVmr}, the coefficient of variation
#'   (100\% * sqrt(variance(PEmr)) / PEmr);
#'     \item \code{index}, a logical identifying the index streams (TRUE for
#'   index);
#'     \item \code{maintain}, a logical identifying the streams that will
#'   continue to have ongoing trapping even if not part of the Adult Index;
#'     \item \code{indexContrib} the stream population
#'   estimate that will be used in the Adult Index (NA for new);
#'     \item \code{indexContribCV} the stream CV that will be used to
#'   generate 95\% confidence intervals for the Adult Index (NA for new); and
#'     \item \code{complete} a logical identifying streams and years for which
#'   the Adult Index has already been estimated (FALSE for new).
#'   }
#' @param minNMR
#'   An integer scalar greater than or equal to 2,
#'   the minimum number of mark-recapture estimates
#'   needed in a year to generate an index, default 2.
#' @return
#'   A list with 2 data frames:
#'   \itemize{
#'     \item \code{streamPE} - stream mark-recapture and Adult
#'   Index contributions for the incomplete rows in \code{streamDat}, with
#'   the same variables as \code{streamDat}; and
#'     \item \code{lakeIndex} - annual lake-wide Adult Indices
#'   with 5 columns: \code{lake}, \code{year}, the Adult Index \code{index},
#'   and the lower and upper 95\% confidence interval \code{ilo} and
#'   \code{ihi}.
#'   }
#' @export
#' @details
#'   The annual Adult Index is simply the sum of stream population estimates for
#'   each year.  Missing stream estimates are estimated by a lake-specific
#'   ANOVA model relating the log of the stream estimates to the main effects
#'   of each stream and each year, weighted by the inverse of the CV squared.
#' @references
#' Adams, JV, JM Barber, GA Bravener, SA Lewandoski. 2021.
#' Quantifying Great Lakes sea lamprey populations using an index of adults.
#' Journal of Great Lakes Research.
#'  \href{https://doi.org/10.1016/j.jglr.2021.04.009}{[link]}

AIestimate <- function(streamDat, minNMR=2) {

  # keep track of who started off as incomplete
  incomp <- with(streamDat, !complete)

  # set index contributions for new data to PEmr
  incompindex <- with(streamDat, !complete & index)
  streamDat$indexContrib[incompindex] <- streamDat$PEmr[incompindex]
  streamDat$indexContribCV[incompindex] <- streamDat$CVmr[incompindex]
  uy <- unique(streamDat$year[incompindex])

  # fill in missing contributions in new data
  incompiMiss <- with(streamDat, !complete & index & is.na(PEmr))
  if (any(incompiMiss)) {
    sub <- streamDat[streamDat$index, ]
    indfit <- with(sub,
      aov(log(PEmr) ~ as.factor(lscode) + as.factor(year), weights=1/CVmr^2)
      )
    n.mr <- tapply(!is.na(sub$PEmr), sub$year, sum)
    eyrs <- as.numeric(names(n.mr)[n.mr > (minNMR - 0.5)])
    estimable <- streamDat$year %in% eyrs
    Pmr <- rep(NA, length(estimable))
    CVmr <- Pmr
    p.sd <- predAntilognorm(modfit=indfit,
      xdata=streamDat[estimable & streamDat$index, ])
    Pmr[estimable & streamDat$index] <- p.sd$pred
    CVmr[estimable & streamDat$index] <- 100*p.sd$sdpred/p.sd$pred
    streamDat$indexContrib[incompiMiss] <- Pmr[incompiMiss]
    streamDat$indexContribCV[incompiMiss] <- CVmr[incompiMiss]
  }

  # mark as complete
  streamDat$complete[incomp] <- TRUE

  # arrange estimates in a matrix
  streamests <- with(streamDat[streamDat$index, ],
    tapply(indexContrib, list(year, lscode), mean))
  streamCVs <- with(streamDat[streamDat$index, ],
    tapply(indexContribCV, list(year, lscode), mean))
  # get rid of years with missing estimates
  missyears <- apply(is.na(streamests), 1, any)
  pe2 <- streamests[!missyears, , drop=FALSE]
  cv2 <- streamCVs[!missyears, , drop=FALSE]
  # THEN, get rid of streams with missing estimates
  missstrs <- apply(is.na(pe2), 2, any)
  pe3 <- pe2[, !missstrs, drop=FALSE]
  cv3 <- cv2[, !missstrs, drop=FALSE]
  vr <- (cv3 * pe3 / 100)^2

  # calculate index with 95% confidence interval
  sumpe <- apply(pe3, 1, sum)
  sumvar <- apply(vr, 1, sum)
  n <- dim(pe3)[2]
  int <- qt(1-0.05/2, n-1)*sqrt(sumvar)/sqrt(n)
  ilo <- sumpe - int
  ihi <- sumpe + int
  lakesum <- data.frame(lake=streamDat$lake[1], year=as.numeric(row.names(pe3)),
    index=sumpe, ilo=ilo, ihi=ihi)

  # subset the output to only include the years that needed new estimates
  streamDatOut <- streamDat[streamDat$year %in% uy, ]
  lakeIndexOut <- lakesum[lakesum$year %in% uy, ]
  list(streamPE=streamDatOut, lakeIndex=lakeIndexOut)
}
