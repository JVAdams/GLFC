#' Estimate Index of Sea Lamprey Adults
#'
#' Estimate the Adult Index of sea lampreys in a single Great Lake.
#' @param indexStreams
#'   A numeric vector of lake-stream IDs identifying streams to be included
#'   in the index, e.g., 1.064 = lake ID + (stream ID)/1000.
#' @param streamPECurr
#'   A data frame of stream mark-recapture estimates without any previously
#'   estimated Adult Indices (typically from the current year),
#'   with variables: \code{year}, \code{lake}, lake-stream ID \code{lscode}
#'   (see description under \code{indexStreams}), population estimate
#'   \code{PEmr}, coefficient of variation \code{CVmr}
#'   (100% * sqrt(variance(PEmr)) / PEmr).  There should be no missing values
#'   in \code{year}, \code{lake}, or \code{lscode}.  There should be only one
#'   value for \code{lake} in the data frame.  The data frame may include
#'   additional variables, but they will be ignored unless listed in
#'   \code{varKeep}.
#' @param streamPEPrev
#'   A data frame of stream mark-recapture estimates with
#'   estimated Adult Index contributions (typically from previous years),
#'   with the same variables as in \code{streamPECurr} plus the
#'   previously estimated contribution \code{indexContrib}, default NULL.
#'   There should be no missing values
#'   in \code{year}, \code{lake}, or \code{lscode}.  The data frame may include
#'   additional variables, but they will be ignored unless listed in
#'   \code{varKeep}.
#' @param varKeep
#'   A character vector naming additional variables (present in both
#'   \code{streamPECurr} and \code{streamPEPrev}) to keep in the output
#'  data frame (\code{streamPE}), default NULL.
#' @param minNMR
#'   An integer scalar greater than or equal to 2,
#'   the minimum number of mark-recapture estimates
#'   needed in a year to generate an index, default 2.
#'
#' @return
#'   A list with 2 components:
#'   \code{streamPE}, a data frame of stream mark-recapture and Adult Index
#'   contributions for the current data (\code{streamPECurr}), with the same
#'   variables as \code{streamPEPrev}; and
#'   \code{lakeIndex}, a data frame of annual lake-wide Adult Indices
#'   for the current data (\code{streamPECurr}), with 5 columns: \code{lake},
#'   \code{year}, the Adult Index \code{index}, and the lower and upper
#'   jackknifed range \code{jlo} and \code{jhi}.
#' @import
#'   plyr
#' @export
#' @details
#'   The annual Adult Index is simply the sum of stream population estimates for
#'   each year.  Missing stream estimates are estimated by a lake-specific
#'   ANOVA model relating the log of the stream estimates to the main effects
#'   of each stream and each year, weighted by the inverse of the CV squared.
#'   The jackknifed range is produced by recalculating the index,
#'   leaving out one stream at a time, then scaling up the result to the same
#'   scale as the Adult Index based on all streams.
#' @examples
#'
#' # estimate the index initially for 1998-1999 data
#' str9899 <- data.frame(
#'   year=rep(1998:1999, c(3, 3)), lake=1,
#'   lscode=rep(c(1.1, 1.2, 1.3), 2),
#'   PEmr=c(15, 20, NA, 12, 22, 30),
#'   CVmr=c(50, 50, NA, 50, 40, 30))
#' istr <- c(1.1, 1.2, 1.3)
#' est9899 <- estAIndex(indexStreams=istr, streamPECurr=str9899)
#' est9899
#'
#' # then estimate the index for 2000 data
#' str00 <- data.frame(
#'   year=2000, lake=1,
#'   lscode=c(1.1, 1.2, 1.3),
#'   PEmr=c(10, NA, 28),
#'   CVmr=c(50, NA, 32))
#' estAIndex(indexStreams=istr, streamPECurr=str00,
#'   streamPEPrev=est9899$streamPE)
#'
#' # notice how this is different than
#' # estimating the index for 1998-2000 altogether
#' estAIndex(indexStreams=istr, streamPECurr=rbind(str9899, str00))

estAIndex <- function(indexStreams, streamPECurr, streamPEPrev=NULL,
  varKeep=NULL, minNMR=2) {

  if (length(indexStreams)>length(unique(indexStreams))) {
    stop("indexStreams should be unique (without duplicates).")
  }

  if (length(minNMR)!=1 | !is.element(minNMR, 2:100)) {
    stop("minNMR should be an integer scalar >= 2.")
  }

  varShort <- c("year", "lake", "lscode", "PEmr", "CVmr")
  varLong <- c("year", "lake", "lscode", "PEmr", "CVmr",
      "indexContrib")
  if (!is.null(varKeep)) {
    if (!is.character(varKeep)) stop("varKeep must be a character vector")
    if (any(is.na(match(varKeep, names(streamPECurr))))) {
      stop("streamPECurr must include these variables:",
        paste(varKeep, collapse=", "), ".")
    }
    varShort <- c(varShort, varKeep)
    varLong <- c(varLong, varKeep)
  }
  if (any(is.na(match(varShort, names(streamPECurr))))) {
    stop("streamPECurr must include these variables:",
      paste(varShort, collapse=", "), ".")
  }

  streamPECurr$complete <- FALSE

  if (!is.null(streamPEPrev)) {
    # combine the previous data with the current data
    if (any(is.na(match(varKeep, names(streamPEPrev))))) {
      stop("streamPEPrev (if not NULL) must include these variables:",
        paste(varKeep, collapse=", "), ".")
    }
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

  a <- with(streamPECurr, table(paste(year, lscode)))
  if (sum(a>1)>1) {
    stop("streamPECurr should have only one row for each year-lscode combination.")
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

  a <- with(streamPE, table(paste(year, lscode)))
  if (sum(a>1)>1) {
    stop("No year-lscode combinations should be repeated in either streamPEPrev or streamPECurr.")
  }

  a <- with(streamPE, is.na(lake) | is.na(lscode) |
      !is.element(year, 1970:2050) |
      (!is.na(PEmr) & PEmr<0) | (!is.na(CVmr) & CVmr<0))
  if (sum(a)>0) {
    stop("Invalid value(s) for lake, lscode, year, PEmr, CVmr.")
  }

  # set index contributions for new data to PEmr
  streamPE$indexContrib[!streamPE$complete & selstreams] <-
    streamPE$PEmr[!streamPE$complete & selstreams]
  # fill in missing contributions in new data
  incompMiss <- !streamPE$complete & is.na(streamPE$PEmr) & selstreams
  if (any(incompMiss)) {
    sub <- streamPE[selstreams, ]
    indfit <- with(sub, aov(log(PEmr) ~ as.factor(lscode) + as.factor(year),
      weights=1/CVmr^2))
    # figure out estimable years (those with at least minNMR m-r estimate)
    n.mr <- tapply(!is.na(sub$PEmr), sub$year, sum)
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
  jack <- as.data.frame(jack)

  # subset the output to only include the year-lakes in streamPECurr
  uyl <- with(streamPECurr, unique(paste0(year, lake)))
  streamPEOut <- streamPE[with(streamPE, paste0(year, lake)) %in% uyl, varLong]
  lakeIndexOut <- jack[with(jack, paste0(year, lake)) %in% uyl, ]
  list(streamPE=streamPEOut, lakeIndex=lakeIndexOut)
}
