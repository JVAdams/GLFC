#' Adult Index Report
#'
#' Create a draft template-style report of the Adult Index estimates of
#' sea lamprey in the Great Lakes.
#' @param streamPEs
#'   A data frame of "complete" stream mark-recapture estimates
#'   (meaning all contributions to the Adult Indices have
#'   already been calculated).  The data frame must
#'   include: \code{year},
#'   \code{lake}, lake-stream ID \code{lscode}
#'   (see details), population estimate
#'   \code{PEmr}, coefficient of variation \code{CVmr}
#'   (100% * sqrt(variance(PEmr)) / PEmr), \code{index}, a logical
#'   identifying the index streams; \code{maintain} a logical identifying the
#'   streams that will continue to have ongoing trapping even if not part of
#'   the Adult Index; \code{indexContrib} a numeric, the stream population
#'   estimate that will be used in the Adult Index (NA for new); and
#'   \code{complete} a logical identifying streams and years for which the
#'   Adult Index has already been estimated (should be all TRUE).
#' @param csvDir
#'   A character scalar identifying the path where the rtf file will be
#'   stored, e.g., \code{csvDir = "C:\\temp\\mydir"}.
#' @param outFile
#'   Name of the ouput rtf file, default NULL, in which case the file will be
#'   named "YYYY Adult Index - draft report.doc" where YYYY
#'   is the latest year represented in \code{streamDat}.
#' @details
#'   Lake-stream IDs are combination of lake ID and stream ID
#'   e.g., 1.064 = lake ID + (stream ID)/1000.
#' @return
#'   A draft report document as an rtf file (with the file type *.doc,
#'   so that MS Word will open it automatically).
#' @import
#'   jvamisc
#' @export
#'

AIreport <- function(streamDat, csvDir, outFile=NULL) {

# library(GLFC)
# streamDat=streamDat
# csvDir=DIRECTORY
# outFile=NULL

  YEAR <- max(streamDat$year)

  if (is.null(outFile)) {
    outFile  <- paste(YEAR, "Adult Index - draft report.doc")
  }

  # create a file for the draft report
  doc <<- startrtf(file=outFile, dir=csvDir)#, width=11, height=8.5)

  heading(paste("Draft Report of the ", YEAR,
    " Lake-Wide Adult Sea Lamprey Index"))
  heading(date(), 2)

  # start by printing any "other tables" first
  if (!is.null(otherTabs)) {
    for(i in 1:length(otherTabs)) {
      tabl(names(otherTabs)[i], TAB=otherTabs[[i]], row.names=FALSE)
    }
  }


  # create a nice looking, formatted data frame for printing out summaries
  df.nice <- prettytable(streamDat[, c("lake", "country", "strcode", "strname",
    "year", "trapcatch", "PEmr", "CVmr")], 4)
  df.nice$CVmr <- round(df.nice$CVmr)



  streamDat$cle <- with(streamDat,
    paste(casefold(substring(country, 1, 2), upper=TRUE),
    Lakeabbs[lake], estr, sep=" - "))
  sd <- with(streamDat, PEmr * CVmr/100)
  streamDat$PEplusSD <- with(streamDat, PEmr + sd)
  streamDat$PEminusSD <- with(streamDat, PEmr - sd)
  streamDat$PEminusSD[with(streamDat, !is.na(PEminusSD) & PEminusSD<0)] <- 0



  #### PLOTS (and tables) ####



  endrtf()
}
