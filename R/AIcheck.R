#' Error Check the Adult Index Data
#'
#' Check the adult sea lamprey trapping data (collected for estimation of
#' the Adult Index) for errors.
#' @param streamDat
#'   A data frame of old and new stream mark-recapture estimates
#'   used to estimate the lake-wide Adult Indices,
#'   typically the output from \code{\link{AIprep}}.  The data frame must
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
#'   Adult Index has already been estimated (FALSE for new).
#' @param csvDir
#'   A character scalar identifying the path where the rtf file will be
#'   stored, e.g., \code{csvDir = "C:\\temp\\mydir"}.
#' @param outFile
#'   Name of the ouput rtf file, default NULL, in which case the file will be
#'   named "YYYY Adult Index - error checking.doc" where YYYY
#'   is the latest year represented in \code{streamDat}.
#' @param otherTabs
#'   A list of other tables to be printed in error check report, default NULL.
#'   The list names will be used as captions.
#' @details
#'   Lake-stream IDs are combination of lake ID and stream ID
#'   e.g., 1.064 = lake ID + (stream ID)/1000.
#' @return
#'   An error checking document as an rtf file (with the file type *.doc,
#'   so that MS Word will open it automatically).
#' @import
#'   jvamisc
#' @export
#'

AIcheck <- function(streamDat, csvDir, outFile=NULL, otherTabs=NULL) {

# library(GLFC)
# streamDat=streamDat
# csvDir=DIRECTORY
# outFile=NULL
# otherTabs=othtabs

  YEAR <- max(streamDat$year)

  if (is.null(outFile)) {
    outFile  <- paste(YEAR, "Adult Index - error checking.doc")
  }

  # create a file with error checking results
  doc <<- startrtf(file=outFile, dir=csvDir)#, width=11, height=8.5)

  heading(paste("Error Checking the ", YEAR,
    " Lake-Wide Adult Sea Lamprey Data"))
  heading(date(), 2)

  # start by printing any "other tables" first
  if (!is.null(otherTabs)) {
    for(i in 1:length(otherTabs)) {
      tabl(names(otherTabs)[i], TAB=otherTabs[[i]], row.names=FALSE)
    }
  }


  # create a nice looking, formatted data frame for printing out errors ...
  df.nice <- prettytable(
    streamDat[, c("lake", "country", "strcode", "strname", "year", "trapcatch",
      "PEmr", "CVmr")], 0, bigseps=c("", ",")[c(2, 2, 2, 2, 1, 2, 2, 2)])

  with(streamDat, {

    # check paired nature of mark-recapture estimates and trap catches
    tab <- df.nice[((is.na(PEmr) & !is.na(CVmr)) | (!is.na(PEmr) & is.na(CVmr)) |
        (is.na(trapcatch) & !is.na(PEmr))) & lscode!=3.999, ]
    if (dim(tab)[1]>0) {
    	tabl("ERROR:  Something's missing ... either, trapcatch, PEmr, or CVmr.",
        TAB=tab, row.names=FALSE)
    } else para("OKAY:  Mark-recapture estimates and trap catches are",
      " either all there or all missing.")

    # check values of trap catch and population estimate
    tab <- df.nice[!is.na(PEmr) & !is.na(trapcatch) & PEmr<=trapcatch, ]
    if (dim(tab)[1]>0) {
    	tabl("ERROR:  Population estimates shouldn't be smaller than trap catches.",
        TAB=tab, row.names=FALSE)
    } else para("OKAY:  Population estimates are greater than trap catches.")

    sel1 <- year %in% (YEAR-(5:1))
    nts <- tapply(!is.na(trapcatch[sel1]) & trapcatch[sel1]>0, lscode[sel1], sum)
    streamz1 <- names(nts)[nts>0]
    sel2 <- year==YEAR
    nts <- tapply(!is.na(trapcatch[sel2]) & trapcatch[sel2]>0, lscode[sel2], sum)
    streamz2 <- names(nts)[nts<1]
    streamz <- as.numeric(intersect(streamz1, streamz2))
    if (length(streamz)>0) {
    	tab <- df.nice[lscode %in% streamz &
          ((sel1 & !is.na(trapcatch) & trapcatch>0) | sel2), ]
    	tabl("Streams with no trap catch in ", YEAR,
        ", but at least one trap catch in previous five years.",
        TAB=tab, row.names=FALSE)
    } else para("OKAY:  All streams with no trap catch in ", YEAR,
      ", also didn't have trap catch in the previous five years.")

    tc <- ifelse(is.na(trapcatch), 0, trapcatch)
    sel2 <- year==YEAR
    tnow <- tapply(tc[sel2], lscode[sel2], mean, na.rm=TRUE)
    sel1 <- year %in% (YEAR-(5:1)) & lscode %in% names(tnow)
    tmin <- tapply(tc[sel1], lscode[sel1], min)
    tmax <- tapply(tc[sel1], lscode[sel1], max)

    streamz1 <- names(tnow)[tnow > 1.5*tmax]
    streamz2 <- names(tnow)[tnow < tmin/2]
    if (length(streamz1)>0) {
    	tab <- df.nice[lscode %in% streamz1 &
          ((sel1 & !is.na(trapcatch) & trapcatch>0) | sel2), ]
    	tabl("Streams with a ", YEAR,
        " trap catch more than 50% greater than maximum from",
        " the previous five years.",
        TAB=tab, row.names=FALSE)
    } else para("OKAY:  No unusually large trap catches in ", YEAR, ".")
    if (length(streamz2)>0) {
    	tab <- df.nice[lscode %in% streamz2 &
          ((sel1 & !is.na(trapcatch) & trapcatch>0) | sel2), ]
    	tabl("Streams with a ", YEAR,
        " trap catch less than half of minimum from the previous five years.",
        TAB=tab, row.names=FALSE)
    } else para("OKAY:  No unusually small trap catches in ", YEAR, ".")

  })

  streamDat$cle <- with(streamDat,
    paste(casefold(substring(country, 1, 2), upper=TRUE),
    Lakeabbs[lake], estr, sep=" - "))
  sd <- with(streamDat, PEmr * CVmr/100)
  streamDat$PEplusSD <- with(streamDat, PEmr + sd)
  streamDat$PEminusSD <- with(streamDat, PEmr - sd)
  streamDat$PEminusSD[with(streamDat, !is.na(PEminusSD) & PEminusSD<0)] <- 0



  #### PLOTS (and tables) ####



  # exploratory plots of the data
  FIG.scatter <- function() {
  	with(streamDat, {
    	par(mfrow=c(3, 2), mar=c(4, 4, 2, 1), las=1, cex=1)
    	plotdf(streamDat[, c("lake", "country", "year")])
    	sel1 <- !is.na(trapcatch) & trapcatch>0 & !is.na(PEmr) & PEmr>0
    	plot(trapcatch[sel1], PEmr[sel1], log="xy",
        xlab="Trap Catch", ylab="M-R Estimate")
    	sel1 <- !is.na(CVmr) & CVmr>0 & !is.na(PEmr) & PEmr>0
    	plot(PEmr[sel1], CVmr[sel1], log="xy", xlab="M-R Estimate", ylab="CV")
  	})
  }

  figu("Exploratory plots of ", YEAR, " adult sea lamprey data.",
    FIG=FIG.scatter, newpage="port")



  # plot trap efficiencies
  FIG.trapEff <- function() {
    sub <- streamDat[with(streamDat, !is.na(PEmr) & !is.na(trapcatch)), ]
  	with(sub, {
      suby <- sub[with(sub, year==YEAR), ]
      suby <- suby[with(suby, order(lake, country, estr, strcode)), ]
      sucle <- suby$cle
      suls <- suby$lscode
      tecol <- colr(suby$trapEff, "blue", "orange")
      nrnc <- n2mfrow(length(suls))[2:1]
    	par(mfrow=nrnc, mar=c(0, 0, 0, 0), oma=c(4, 4, 3, 1), yaxs="i", cex=1.4)
    	for(i in seq(suls)) {
    		sel <- lscode==suls[i] # & trapEff<1
    		plot(year[sel], trapEff[sel], type="n",
          xlim=range(year), ylim=0:1, axes=FALSE, xlab="", ylab="")
    		abline(h=seq(0.25, 0.75, 0.25), col="lightgray")
    		abline(v=seq(1985, YEAR+2, 5), col="lightgray")
    		lines(year[sel], trapEff[sel], type="o", pch=20, cex=0.5, col=tecol[i])
    		if (i <= nrnc[2]) {
          axis(1, at=seq(1990, YEAR+2, 10), outer=TRUE, cex.axis=0.6, las=2,
            tcl=-0.3)
    		}
    		if (i%%nrnc[2] == 1) {
          axis(2, at=c(0.25, 0.75), labels=c("25", "75"), outer=TRUE,
            cex.axis=0.6, las=1, tcl=-0.3)
    		}
    		text(1985, 0.85, paste(sucle[i], strname[sel][1], sep="\n"),
          cex=0.6, adj=0)
        box(col="darkgray")
    	}
      mtext("Year", side=1, outer=TRUE, cex=1.2, line=3)
      mtext("Trap efficiency (%)", side=2, outer=TRUE, cex=1.2, line=3)
  	})
  }

  if(!is.null(streamDat$trapcatch)) {
    # calculate trap efficiency
    streamDat$trapEff <- with(streamDat, trapcatch/PEmr)
    figu("Trap efficiency estimates, 1984-", YEAR,
      ".  Color is used to indicate streams with highest (orange) and lowest",
      " (blue) trap efficiencies in ", YEAR, ".",
      FIG=FIG.trapEff, newpage="port")
  }



  # line plots showing summary of stream PEs over past 5 years
  FIG.strmest <- function() {
    selstreams <- with(streamDat, year > YEAR - 4.5 & !is.na(indexContrib))
    streams.pick <- sort(unique(streamDat$lscode[selstreams]))
    sub <- streamDat[with(streamDat,
      is.element(lscode, streams.pick) & year > YEAR - 4.5), ]
  	with(sub, {
      suby <- sub[with(sub, year==YEAR), ]
      suby <- suby[with(suby, order(lake, country, estr, strcode)), ]
      sucle <- suby$cle
      suls <- suby$lscode
      nrnc <- n2mfrow(length(suls))
    	par(mfrow=nrnc, mar=c(0, 2, 2, 0), oma=c(4, 2, 1, 1), yaxs="i", cex=1.4)
    	for(i in seq(suls)) {
    		sel <- lscode==suls[i]
        selsd <- lscode==suls[i] & !is.na(PEplusSD)
        mymax <- 1.1*max(c(PEplusSD[sel], PEmr[sel]), na.rm=TRUE)/1000
        plot(year[sel], PEmr[sel]/1000, type="n", axes=FALSE,
          xlim=range(year) + c(-1, 1)*0.3, ylim=c(0, mymax), xlab="", ylab="")
    		lines(year[sel], PEmr[sel]/1000, type="o", pch=16, cex=0.7)
    		arrows(year[selsd], PEminusSD[selsd]/1000,
          year[selsd], PEplusSD[selsd]/1000, length=0.05, angle=90, code=3)
    		if (i <= nrnc[2]) axis(1, outer=TRUE, cex.axis=0.6, tcl=-0.2)
      	axis(2, cex.axis=0.6, las=1, tcl=-0.2)
    		mtext(paste(sucle[i], strname[sel][1], sep="\n"), side=3,
          cex=0.8, adj=0.1)
    		box(col="gray")
    	}
      mtext("Year", side=1, outer=TRUE, cex=1.2, line=3)
      mtext("Adult sea lampreys  (thousands)", side=2, outer=TRUE, cex=1.2,
        line=1)
  	})
  }

  figu("Recent adult sea lamprey mark-recapture estimates on streams,",
    " plus or minus 1 standard deviation, ", YEAR-4, "-", YEAR, ".",
  	FIG=FIG.strmest, newpage="port")

  endrtf()
}
