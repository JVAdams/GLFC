#' Prepare the Adult Index Data
#'
#' Read in the adult sea lamprey trapping data (collected for estimation of
#' the Adult Index) and prepare it for estimation.
#' @param csvDir
#'   A character scalar identifying the path where the csv files are
#'   stored, e.g., \code{csvDir = "C:\\temp\\mydir"}.
#' @param csvNew
#'   A character scalar identifying the name of the csv file
#'   with stream mark-recapture estimates for which Adult Indices
#'   will be estimated (typically from the current year).  The first row of
#'   the csv file should be column headers and must include: \code{year},
#'   \code{lake}, lake-stream ID \code{lscode}
#'   (see details), population estimate
#'   \code{PEmr}, coefficient of variation \code{CVmr}
#'   (100\% * sqrt(variance(PEmr)) / PEmr).   See details.
#' @param csvOld
#'   A character scalar identifying the name of the csv file
#'   with stream mark-recapture estimates for which Adult Indices
#'   have already been estimated (typically from previous years),
#'   with the same variables as in \code{csvNew} plus the
#'   previously estimated contribution \code{indexContrib}, default NULL.
#'   See details.
#' @param streamInfo
#'   A data frame with stream information to be combined with the estimates
#'   data (csvNew and csvOld), default \code{\link{trappedStreams}}.  Set to
#'   NULL if you don't want any stream information to be brought in.
#' @param indexStreams
#'   Lake-stream IDs identifying index streams, default lsIndex.
#' @param keepStreams
#'   Lake-stream IDs identifying streams which will continue to have ongoing
#'   trapping even if not part of the Adult Index, default lsKeep.
#' @details
#'   Lake-stream IDs are combination of lake ID and stream ID
#'   e.g., 1.064 = lake ID + (stream ID)/1000.
#'
#'   For both estimate files (csvNew and csvOld), there should be no missing
#'   values in \code{year}, \code{lake}, or \code{lscode}.
#'   Both files may contain variables other than those required.
#'   Those with the same name as variables in \code{streamInfo} will be
#'   replaced.  Others will be kept.
#' @return
#'   A single data frame that contains the information from all of the inputs,
#'   including new variables: \code{index}, a logical
#'   identifying the index streams; \code{maintain} a logical identifying the
#'   streams that will continue to have ongoing trapping even if not part of
#'   the Adult Index; \code{indexContrib} a numeric, the stream population
#'   estimate that will be used in the Adult Index (NA for csvNew); and
#'   \code{complete} a logical identifying streams and years for which the
#'   Adult Index has already been estimated (FALSE for csvNew).
#' @importFrom plyr rbind.fill
#' @export

AIprep <- function(csvDir, csvNew, csvOld=NULL, streamInfo=trappedStreams,
  indexStreams=lsIndex, keepStreams=lsKeep) {

  if (length(indexStreams) > length(unique(indexStreams))) {
    stop("indexStreams should be unique (without duplicates).")
  }

  varShort <- c("year", "lake", "lscode", "PEmr", "CVmr")
  varLong <- c("year", "lake", "lscode", "PEmr", "CVmr", "indexContrib")

  # bring in this year's stream data

  strnew <- read.csv(paste(csvDir, csvNew, sep="\\"), as.is=TRUE, header=TRUE)
  # identify index streams and streams with maintained trapping operations
  strnew$index <- with(strnew, lscode %in% unlist(indexStreams))
  strnew$maintain <- with(strnew, lscode %in% unlist(keepStreams))
  strnew$complete <- FALSE

  if (any(is.na(match(varShort, names(strnew))))) {
    stop("csvNew must include these variables:",
      paste(varShort, collapse=", "), ".")
  }

  a <- with(strnew, table(paste(year, lscode)))
  if (sum(a>1)>1) {
    stop("strnew should have only one row for each year-lscode combination.")
  }

  # bring in the old stream data

  if (!is.null(csvOld)) {
    strold <- read.csv(paste(csvDir, csvOld, sep="\\"), as.is=TRUE, header=TRUE)
    strold$complete <- TRUE

    if (any(is.na(match(varLong, names(strold))))) {
      stop("strold (if not NULL) must include these variables:",
        paste(varLong, collapse=", "), ".")
    }

    # combine the previous data with the current data
    streamPE <- plyr::rbind.fill(strold, strnew)
  } else {
    streamPE <- strnew
  }

  a <- with(streamPE, table(paste(year, lscode)))
  if (any(a>1)) {
    stop(paste("No year-lscode combinations should be repeated,",
    paste(names(a[a>1]), collapse=", ")))
  }

  a <- with(streamPE, is.na(lake) | is.na(lscode) |
      !is.element(year, 1970:2050) |
      (!is.na(PEmr) & PEmr<0) | (!is.na(CVmr) & CVmr<0))
  if (sum(a)>0) {
    stop("Invalid value(s) for lake, lscode, year, PEmr, or CVmr.")
  }

  # replace all the general information with info from streamInfo
  if (!is.null(streamInfo)) {
  replace <- names(streamInfo)[names(streamInfo)!="lscode"]
  streamPE <- merge(streamPE[, !(names(streamPE) %in% replace)], streamInfo)
  }

  streamPE <- streamPE[with(streamPE, order(lscode, year)), ]
  streamPE
}
