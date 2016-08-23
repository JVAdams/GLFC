#' Adult Index Preparation, Error Checking, and Estimation
#'
#' Carry out a series of steps in the Adult Index estimation process in one
#' fell swoop.  This function prepares and error checks the data, estimates
#' the Adult Index, calculates the targets, and generates a draft report.
#'
#' @param DIRECTORY
#'   A character scalar identifying the path where the csv files are
#'   stored, e.g., \code{DIRECTORY = "C:\\\\temp\\\\mydir"}.
#' @param NEWDATARAW
#'   A character scalar identifying the name of the csv file
#'   with stream mark-recapture estimates for which Adult Indices
#'   will be estimated (typically from the current year).  The first row of
#'   the csv file should be column headers and must include: \code{year},
#'   \code{lake}, population estimate \code{PEmr},
#'   coefficient of variation \code{CVmr}
#'   (100\% * sqrt(variance(PEmr)) / PEmr).
#' @param STREAMDATAPREV
#'   A character scalar identifying the name of the csv file
#'   with stream mark-recapture estimates for which Adult Indices
#'   have already been estimated (typically from previous years),
#'   with the same variables as in \code{NEWDATARAW} plus the
#'   previously estimated contribution \code{indexContrib}.
#' @param LAKEDATAPREV
#'   A character scalar identifying the name of the csv file
#'   with annual lake-wide Adult Index estimates (typically from previous
#'   years), with 5 columns: \code{lake}, \code{year}, \code{index}, and the
#'   lower and upper 95\% confidence intervals \code{ilo} and \code{ihi}.
#' @return
#'   Four data summaries are saved as csv files to \code{DIRECTORY}, where
#'   \code{YYYY} represents the most recent year of data in \code{NEWDATARAW}:
#'   \itemize{
#'     \item \code{AdultStreamYYYY.csv} - an updated version of
#'       \code{NEWDATARAW}
#'       with the additional column of the lake-stream IDs, \code{lscode},
#'       which are combination of lake ID and stream ID, e.g.,
#'       1.064 = lake ID 1 + (stream ID 64)/1000
#'     \item \code{AdultStreamThruYYYY.csv} - an updated version of
#'       \code{STREAMDATAPREV} with the latest year of data added
#'     \item \code{AdultLakeThruYYYY.csv} - an updated version of
#'       \code{LAKEDATAPREV}
#'       with the latest year of data added
#'     \item \code{AdultTargetYYYY.csv} - the calculated targets for the Adult
#'       Index
#'       of each Great Lake, with 2 columns: \code{lake} and \code{targInd}
#'   }
#'
#'   Two rich text documents are saved as doc files (so that MS Word will open
#'   them automatically) to \code{DIRECTORY}:
#'   \itemize{
#'     \item \code{YYYY Adult Index - error checking.doc} - error checking
#'       document
#'     \item \code{YYYY Adult Index - draft report.doc} - draft report document
#'   }
#'
#' @seealso \code{\link{AIprep}}, \code{\link{AIcheck}},
#'   \code{\link{AIestimate}}, \code{\link{AItarget}}, \code{\link{AIreport}}
#' @details
#'   For more details on the entire process and the steps involved,
#'   see the vignette
#'   \href{https://rawgit.com/JVAdams/GLFC/master/vignettes/Adult-Index.html}{Adult Index Estimation}.
#' @importFrom plyr rbind.fill
#' @export
#' @examples
#' \dontrun{
#'  library(GLFC)
#'  AIpresto(
#'    DIRECTORY = "C:\\TrappingData\\2015",
#'    NEWDATARAW = "TrapCatchSchaeferEstimate2015.csv",
#'    STREAMDATAPREV = "AdultStreamThru2014.csv",
#'    LAKEDATAPREV = "AdultLakeThru2014.csv")
#' }

AIpresto <- function(DIRECTORY, NEWDATARAW, STREAMDATAPREV, LAKEDATAPREV) {

  # make sure all needed packages are installed
  wantpkgs <- c("geosphere", "lubridate", "maps", "plotrix", "plyr", "rtf",
    "XLConnect")
  needpkgs <- setdiff(wantpkgs, row.names(installed.packages()))
  if(length(needpkgs) > 0) install.packages(needpkgs)

  message("\n\nDo you have the latest version of GLFC installed?",
    "\n\nIf not, download this zip file,",
    "\n  https://github.com/JVAdams/GLFC/raw/master/GLFC.zip",
    "\nand install it from the R menu:",
    "\n  Packages,\n  Install package(s) from local zip files...\n\n")


  #### Data Preparation ####

  # Prepare csv file with stream mark-recapture estimates
  # Header must include: year, lake, lscode, PEmr, CVmr
  new <- read.csv(paste(DIRECTORY, NEWDATARAW, sep="\\"), as.is=TRUE)
#  new$lscode <- new$lake + new$streamcode/1000
  # new2 <- new[, c("year", "lake", "lscode", "trapcatch", "PEmr", "CVmr",
  #   "comments")]

  YEAR <- max(new$year)
  STREAMDATANEW <- paste0("AdultStream", YEAR, ".csv")
  write.csv(new, paste(DIRECTORY, STREAMDATANEW, sep="\\"))

  # create some information tables for inclusion in error report

  # dates of the input files being used
  ins <- c(LAKEDATAPREV, STREAMDATAPREV, NEWDATARAW)
  dins <- paste(DIRECTORY, ins, sep="\\")
  finfo <- lapply(dins, function(x) file.info(x)$mtime)
  tabinfiles <- as.matrix(data.frame(
    inputs=c("LAKEDATAPREV", "STREAMDATAPREV", "STREAMDATANEW"),
    files=ins, date.modified=do.call(c, finfo)))
  tabinfiles <- apply(tabinfiles, 2, format)

  # index and maintenance streams for each lake
  tabstreams <- as.matrix(data.frame(lake=Lakenames,
    index=sapply(lsIndex, paste, collapse=","),
    keep=sapply(lsKeep, paste, collapse=",")))
  tabstreams <- apply(tabstreams, 2, format)

  # tables
  othtabs <- list("Input files used."=tabinfiles,
    "Index and maintained trapping streams for adult sea lamprey."=tabstreams)

  # prepare the data for error checking & estimation
  stream1 <- AIprep(csvDir=DIRECTORY, csvNew=STREAMDATANEW,
    csvOld=STREAMDATAPREV)
  lake1 <- read.csv(paste(DIRECTORY, LAKEDATAPREV, sep="\\"), as.is=TRUE,
    header=TRUE)



  ### Error Checking ####

  # create error checking report
  AIcheck(streamDat=stream1, csvDir=DIRECTORY, otherTabs=othtabs)



  ### Adult Index Estimation ####

  # generate estimates
  makeitso <- lapply(1:5, function(L) {
    AIestimate(streamDat=stream1[stream1$lake==L, ], minNMR=2)
    })
  streamcomp <- do.call(rbind, lapply(makeitso, "[[", 1))
  lakecomp <- do.call(rbind, lapply(makeitso, "[[", 2))



  #### Target Estimation ####

  targ <- AItarget(lakeIndex=lake1)

  # targ <- AItarget(lakeIndex=lake1,
  #   years=list(1994:1998, 1995:1999, 2006:2010, 1991:1995, 1993:1997),
  #   adjust=c(1, 0.5, 0.5, 1, 1))




  lakecomp2 <- lakecomp



  #### expand indices to supposed lake-wide PEs ####

  lakeInd <- plyr::rbind.fill(lake1, lakecomp2)
  lakeIndPE <- merge(lakeInd[, c("lake", "year", "index", "ilo", "ihi")],
    cbind(lake=1:5, i2pe=index2pe))
  pes <- lakeIndPE[, c("index", "ilo", "ihi")]*lakeIndPE$i2pe
  names(pes) <- c("pe", "pelo", "pehi")
  lakeIndPE <- cbind(lakeIndPE, pes)
  lakeIndPE <- lakeIndPE[with(lakeIndPE, order(lake, year)), ]



  #### Draft Report ####

  # proposed targets for Lake Michigan
  # using conversion factor (cv): old PE target for Lake Michigan 59,192
  prop.cf <- 59192 / index2pe[2]
  # using new years (ny): new years for Lake Michigan, 1995-1999
  prop.ny <- (5/8.9) *
    mean(with(lakeIndPE, lakeIndPE$index[lake==2 & year>=1995 & year<=1999]))
  pt <- data.frame(lake=2, targInd=c(prop.cf, prop.ny))

  # combine estimates with initially provided data
  streamPE <- plyr::rbind.fill(stream1[stream1$complete==TRUE, ], streamcomp)
  AIreport(streamPEs=streamPE, lakeIPEs=lakeIndPE, targets=targ,
    csvDir=DIRECTORY, outFile=NULL, proptargets=pt)



  #### Export Data ####
  OUTSTREAM <- paste0("AdultStreamThru", YEAR, ".csv")
  OUTLAKE <- paste0("AdultLakeThru", YEAR, ".csv")
  OUTTARG <- paste0("AdultTarget", YEAR, ".csv")
  write.csv(new, paste(DIRECTORY, STREAMDATANEW, sep="\\"), row.names=FALSE)
  write.csv(streamPE, paste(DIRECTORY, OUTSTREAM, sep="\\"), row.names=FALSE)
  write.csv(lakeIndPE, paste(DIRECTORY, OUTLAKE, sep="\\"), row.names=FALSE)
  write.csv(targ, paste(DIRECTORY, OUTTARG, sep="\\"), row.names=FALSE)



  #### Write to Screen ####

  message("\n\n", YEAR, " Adult Abundance Estimates in Streams")
  # cat(paste0("\n\n", YEAR, " Adult Abundance Estimates in Streams\n\n"))
  print(format(
    streamPE[streamPE$year==YEAR,
      c("lscode", "country", "estr", "strname", "index", "maintain", "year",
      "trapcatch", "PEmr", "CVmr", "indexContrib")]
    ), row.names=FALSE)

  message("\n\n", YEAR, " Adult Index in Lakes")
  #cat(paste0("\n\n", YEAR, " Adult Index in Lakes\n\n"))
  print(format(
    lakeIndPE[lakeIndPE$year==YEAR, ]
    ), row.names=FALSE)

  message("\n\n", YEAR, " Adult Targets")
  #cat(paste0("\n\n", YEAR, " Adult Targets\n\n"))
  print(format(
    targ
    ), row.names=FALSE)

  message("\n\nOutput *.csv and *.doc files in directory ", DIRECTORY, ".\n\n")

}
