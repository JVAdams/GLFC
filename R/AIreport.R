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
#'   (100\% * sqrt(variance(PEmr)) / PEmr), \code{index}, a logical
#'   identifying the index streams; \code{maintain} a logical identifying the
#'   streams that will continue to have ongoing trapping even if not part of
#'   the Adult Index; \code{indexContrib} a numeric, the stream population
#'   estimate that will be used in the Adult Index (NA for new);
#'   \code{indexContribCV} a numeric, the stream CV that will be used to
#'   generate 95\% confidence intervals for the Adult Index (NA for new); and
#'   \code{complete} a logical identifying streams and years for which the
#'   Adult Index has already been estimated (should be all TRUE).
#' @param lakeIPEs
#'   A data frame of annual lake-wide Adult Indices with 8 columns:
#'   \code{lake}, \code{year}, the Adult Index \code{index}, its associated
#'   lower and upper 95\% confidence interval \code{ilo} and \code{ihi},
#'   and the corresponding expansion to a supposed population estimate,
#'   \code{pe}, \code{pelo} and \code{pehi}.
#'   The data frame may contain variables other than those required.
#' @param targets
#'   A data frame with the calculated targets for the Adult Index and
#'   expanded PE of each Great Lake, with 5 rows (Superior, Michigan, Huron,
#'   Erie, Ontario) and 2 columns: \code{lake} and
#'   \code{targInd}, typically the output from \code{\link{AItarget}}.
#' @param csvDir
#'   A character scalar identifying the path where the rtf file will be
#'   stored, e.g., \code{csvDir = "C:\\temp\\mydir"}.
#' @param outFile
#'   Name of the output rtf file, default NULL, in which case the file will be
#'   named "YYYY Adult Index - draft report.doc" where YYYY
#'   is the latest year represented in \code{streamDat}.
#' @param proptargets
#'   A data frame with any proposed targets for the Adult Index,
#'   with 2 columns \code{lake} and \code{targInd}, default NULL.
#'   May have from zero to several rows for a single Great Lake.
#' @details
#'   Lake-stream IDs are combination of lake ID and stream ID
#'   e.g., 1.064 = lake ID + (stream ID)/1000.
#' @return
#'   A draft report document as an rtf file (with the file type *.doc,
#'   so that MS Word will open it automatically).
#' @importFrom maps map
#' @importFrom zoo rollapply
#' @importFrom plyr ddply .
#' @importFrom tidyr complete
#' @export
#'
AIreport <- function(streamPEs, lakeIPEs, targets, csvDir, outFile=NULL,
  proptargets=NULL) {

# library(GLFC)
# library(maps)
# streamPEs=streamPE
# lakeIPEs=lakeIndPE
# targets=oldtarg
# csvDir=DIRECTORY
# outFile="TestReport"
# proptargets=NULL

  YEAR <- max(streamPEs$year)

  if (is.null(outFile)) {
    outFile  <- paste(YEAR, "Adult Index - draft report.doc")
  }

  # calculate three-year running mean (moving average) for adult index
  lakeIPEs <- tidyr::complete(lakeIPEs, lake, year)
#  lakeIPEs <- lakeIPEs[order(lakeIPEs$lake, lakeIPEs$year), ]
  look <- plyr::ddply(.data=lakeIPEs[, c("lake", "index")],
    .variables=plyr::.(lake), .drop=FALSE, .fun=zoo::rollapply, width=3,
    FUN=mean, fill=c(NA, NA, NA), align="right")
  lakeIPEs <- cbind(lakeIPEs, index.3mn=look[, 2])

  # create nice looking table with latest year of estimates and targets
  targ2 <- with(lakeIPEs,
    SRstatus(bydat=lake, timedat=year, measdat=index,
      targdat=targets$targInd[1:5],
      status.length=3, response.stat=c("", "***"))[,
        c("bydat", "stspan", "stmean", "targdat", "status")]
  )
  # targ2 <- merge(lakeIPEs[lakeIPEs$year==YEAR, ], targets, all=TRUE)
  row.names(targ2) <- Lakenames
  # targ2$above <- with(targ2,
  #   ifelse(!is.na(index) & !is.na(targInd) & index > targInd, "***", ""))
  targ2$above <- targ2$status
  targ2$targInd <- targ2$targdat
  targ2$index <- targ2$stmean
  targ2$above[with(targ2, is.na(index) | is.na(targInd))] <- " ? "
  targ2 <- targ2[, c("above", "targInd", "index")]
  TAB.targs <- prettytable(targ2, c(0, 0, 0))

  # plot lake-wide totals w/ confidence intervals on different scales
  FIG.lakeCI <- function(lakeids=1:5, k=index2pe) {
    with(lakeIPEs, {
      par(mfrow=c(3, 2), mar=c(3, 3, 2, 3), oma=c(2, 2, 0, 2), cex=1)
      for(i in seq(lakeids)) {
        j <- lakeids[i]
        sel <- lake==j
        mymax <- max(ihi[sel & year>=1985], na.rm=TRUE)/1000
        plot(1, 1, type="n", xlim=range(year), ylim=c(0, mymax),
          xlab="", ylab="", main=Lakenames[i], las=1)
        abline(h=targets$targInd[j]/1000, lty=2)
        if(!is.null(proptargets)) {
          abline(h=proptargets$targInd[proptargets$lake==j]/1000,
            col="gray", lwd=2, lty=2)
        }
        lines(year[sel], index.3mn[sel]/1000, col="#fb8072", lwd=2)
        arrows(year[sel], ilo[sel]/1000, year[sel], ihi[sel]/1000, length=0.03,
          angle=90, code=3, col="lightgray")
        points(year[sel], index[sel]/1000, col="darkgray")
        p4 <- pretty(k[i]*c(0, mymax))
        axis(4, at=p4/k[i], labels=p4, las=1)
        if (i==1) {
          frame()
        }
      }
      mtext("Year", outer=TRUE, side=1, cex=1.4)
      mtext("Adult index  (thousands)", outer=TRUE, side=2, cex=1.4)
      mtext("Lake-wide adult abundance  (thousands)", outer=TRUE, side=4, cex=1.4)
    })
  }



  streamPEs$categ <- "Non-index"
  streamPEs$categ[with(streamPEs, index & !is.na(PEmr))] <-
    "Index w/ mark-recap"
  streamPEs$categ[with(streamPEs, index & is.na(PEmr))] <-
    "Index w/o mark-recap"

  streamPEs$cle <- with(streamPEs,
    paste(casefold(substring(country, 1, 2), upper=TRUE),
      Lakeabbs[lake], estr, sep=" - "))

  streamPEs$cleplus <- with(streamPEs, paste(
    paste(cle, strname, sep="  "),
    paste("    ", format(round(indexContrib), big.mark=",")), sep="\n"))

#     df=streamPEs[streamPEs$year==YEAR, ]
#     group="categ"
#     var="indexContrib"
#     lab="cleplus"
#     sug=c("Index w/ mark-recap", "Index w/o mark-recap", "Non-index")
#     cols=blindcolz[1+(1:length(sug))]
#     legat="topright"
#     leginset=c(0, 0)
#     dr=range(sqrt(df[, var]), na.rm=TRUE)
#     cr=c(0.04, 0.5)
#     ox=-44
#     oy=64

  FIG.bubble1 <- function(df, group, var, lab, sug,
    cols=blindcolz[1:length(sug)], lonR=-c(92.14, 75.97),
    latR=c(41.36, 49.02), legat="topright", leginset=c(0, 0),
    dr=range(sqrt(df[, var]), na.rm=TRUE), cr=c(0.04, 0.25), ox=-44, oy=64) {

    g <- df[, group]
    v <- df[, var]
    n <- length(g)

    xr <- lonR
    yr <- latR
    xrw <- diff(xr)
    yrw <- diff(yr)
    bufx <- xrw/40
    bufy <- yrw/40

    magic <- 20
    par(mar=c(0, 0, 0, 0))
    maps::map("world", type="n", xlim=xr + c(-1, 1)*bufx,
      ylim=yr + c(-magic, 1)*bufy, mar=c(0, 0, 0, 0))
    maps::map("lakes", col="gray", lwd=0.5, add=TRUE)
    pusr <- par("usr")
    with(df, {
      textx <- rep(NA, dim(df)[1])
      textx <- seq(pusr[1], pusr[2],
        length=n+2)[-c(1, n+2)][rank(long, ties.method="first")]

      for(i in seq_along(sug)) {
        sel <- g==sug[i]
        if (sum(sel)>0) {
          circles(long[sel], lat[sel], sqrt(v)[sel], data.range=dr,
            circle.size.range=cr, outx=ox, outy=oy, add=TRUE, fg=cols[i], lwd=3)
          text(textx[sel], yr[1] - (magic-1)*bufy, df[sel, lab],
            adj=0, srt=90, col=cols[i], cex=0.8)
          segments(textx[sel], yr[1] - 2*bufy, long[sel], lat[sel], col=cols[i],
            lty=2)
        }
      }
    })

    par(xpd=NA)
    legend(legat, sug, col=cols, lwd=3, bty="n", inset=leginset, cex=1.4)
  }

  FIG.bubble2 <- function() {
    FIG.bubble1(
      df=streamPEs[streamPEs$year==YEAR & streamPEs$categ!="Non-index", ],
      group="categ", var="indexContrib", lab="cleplus",
      sug=c("Index w/ mark-recap", "Index w/o mark-recap"))
  }

  ### bar plot of individual index stream PEs
  outcex <- 1.2
  YEARb <- 1995
  col7 <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462",
    "#b3de69")
  FIG.bar <- function() {
    par(mar=c(2.5, 2.5, 1, 1), mfrow=c(3, 2), yaxs="i", oma=c(1.5, 1.5, 0, 0),
      cex=1.2)
    for(i in 1:5) {
      mystreamdf <- with(streamPEs,
        streamPEs[lake==i & index==TRUE & year >= YEARb, ])
      p <- with(mystreamdf,
        tapply(indexContrib, list(year, substring(strname, 1, 10)), mean))
      p <- p[, rev(order(apply(p, 2, median, na.rm=TRUE)))]
      yrz <- as.numeric(dimnames(p)[[1]])
      pyrz <- pretty(yrz)
      a <- barplot(t(p)/1000, las=1, col=col7, axes=FALSE,
        names.arg=rep("", dim(p)[1]),
        ylim=1.03*c(0, max(apply(p, 1, sum, na.rm=TRUE)))/1000,
        xlab="", ylab="", main=Lakenames[i], border=NA)
      abline(h=targets$targInd[i]/1000)
      if(!is.null(proptargets)) {
        abline(h=proptargets$targInd[proptargets$lake==i]/1000, lty=2)
      }
      axis(1, at=a[match(pyrz, yrz)], pyrz)
      axis(2, las=1)
      box()
      legend("topleft", rev(colnames(p)), fill=rev(col7[1:dim(p)[2]]), cex=0.5,
        bty="n", border=NA)
      if (i==1) {
        frame()
        }
      }
    mtext("Year", outer=TRUE, side=1, cex=outcex)
    mtext("Adults  (thousands)", outer=TRUE, side=2, cex=outcex)
    }


  # create a file for the draft report
  doc <<- startrtf(file=outFile, dir=csvDir)

  heading("D R A F T")
  heading(paste0(YEAR, " Lake-Wide Adult Sea Lamprey Index"))
  para("Authors ...")
  para(format(Sys.time(), "%B %d, %Y"))

  para("<<<  This is a rough draft to be used as a starting point in creating the final report.  First, save the document as a *.docx Word file (even though it has a *.doc file extension already, it's really just an *.rtf file). Then, select all text in the document (Ctrl-a) and increase the font size to 12. Finally, delete this paragraph, add author names, edit text and insert/delete page breaks as needed.  >>>")

  # merge this year and last years' estimates
  both <- merge(lakeIPEs[lakeIPEs$year==YEAR-1, ],
    lakeIPEs[lakeIPEs$year==YEAR, ],
    by="lake", suffixes = c(".last",".this"), all=TRUE)

  thyr <- round(both$index.this)
  layr <- round(both$index.last)
  hier <- with(both, ilo.this > ihi.last)
  loer <- with(both, ihi.this < ilo.last)
  phrase <- rep("not significantly different from", 5)
  delta <- with(both, round(100*abs(index.this - index.last) / index.last))

  phrase[is.na(hier)] <- "not comparable to"
  phrase[!is.na(hier) & hier] <-
    paste0(delta[!is.na(hier) & hier], "% higher than")
  phrase[!is.na(hier) & loer] <-
    paste0(delta[!is.na(hier) & loer], "% lower than")
  abta <- sum(TAB.targs$above=="***")
  beta <- sum(TAB.targs$above=="")

  insert1 <- ""
  insert2 <- ""
  if (abta>0) {
    insert1 <-
      paste(rownames(TAB.targs)[TAB.targs$above=="***"], collapse=", ")
  }
  if (beta>0) {
    insert2 <-
      paste(rownames(TAB.targs)[TAB.targs$above==""], collapse=", ")
  }
  para("The index of adult sea lamprey abundance is estimated annually for each Great Lake. Based on the mean over the last 3 years (", YEAR-2, "-", YEAR, "), lakes ", insert2, " were less than the targets and lakes ", insert1, " were greater than the targets (Table 1, Figure 1).  Index targets were determined for each lake as average abundance observed during a 5-year period when wounding rates were at an acceptable level. Adult sea lamprey indices and lake-wide abundances from 1985 to ", YEAR, " are reported in Tables 2 and 3.")

  if(!is.null(proptargets)) {
    ptl <- proptargets
    ptl$targInd <- format(round(ptl$targInd), big.mark=",")
    ptl <- split(ptl, ptl$lake)
    pttext <- paste0(Lakenames[as.numeric(names(ptl))], ": ",
      lapply(ptl, function(df) paste(df$targInd, collapse=", ")), collapse="; ")
    para("In addition to the accepted targets, there are also the following proposed targets, ", pttext, ".  <<< Explain further. >>>")
  }

  insert1 <- ""
  insert2 <- ""
  insert3 <- ""
  sel1 <- !is.na(hier) & loer
  sel2 <- !is.na(hier) & !hier & !loer
  sel3 <- !is.na(hier) & hier
  if (sum(sel1)>0) {
    insert1 <- paste(Lakenames[both$lake[sel1]], collapse=", ")
  }
  if (sum(sel2)>0) {
    insert2 <- paste(Lakenames[both$lake[sel2]], collapse=", ")
  }
  if (sum(sel3)>0) {
    insert3 <- paste(Lakenames[both$lake[sel3]], collapse=", ")
  }
  para("Comparing the 95% confidence intervals of the single year ", YEAR, " estimates with those in ", YEAR-1, ", the number of adults significantly decreased in lakes ", insert1, "; remained the same in lakes ", insert2, "; and significantly increased in lakes ", insert3, " (Figure 1).")

  para("The contribution from individual streams to the adult index is shown in Figure 2.")

  misspe <- sum(with(streamPEs, index==TRUE & year==YEAR &
      (is.na(PEmr) | is.na(CVmr))))
  allstr <- sum(with(streamPEs, index==TRUE & year==YEAR))
  para("The distribution of the ", YEAR, " stream estimates around the Great Lakes is shown in Figure 3.  Mark-recapture estimates of adult sea lamprey abundance were available for ", allstr-misspe, " of the ", allstr, " index streams.")

prettyTAB.targs <- TAB.targs[, 1:3]
names(prettyTAB.targs) <- c("> Target", "Target", "3-Yr Avg Index")

  tabl("The judgement of whether a lake is above target is based on the mean adult index over the last 3 years.",
    TAB=prettyTAB.targs)

  extraphrase <- ""
  if(!is.null(proptargets)) {
    extraphrase <- "  Dashed horizontal lines represent proposed targets."
  }

  figu("Adult index values for each Great Lake through ", YEAR, ", with 3-year averages shown as red lines.  Individual estimates with 95% confidence intervals are shown in gray.  Targets are represented by the horizontal lines.", extraphrase,
    FIG=FIG.lakeCI, newpage="port")#, w=6.5, h=7.5)

  TAB.lakewide1 <- with(lakeIPEs, tapply(index, list(year, lake), mean))
  colnames(TAB.lakewide1) <- Lakenames
  tabl("Adult Indices, 1985-", YEAR, ".",
    TAB=prettytable(TAB.lakewide1, 0))

  TAB.lakewide2 <- with(lakeIPEs, tapply(pe, list(year, lake), mean))
  colnames(TAB.lakewide2) <- Lakenames
  tabl("Lake-wide adult sea lamprey abundances, 1985-", YEAR, ", which are based on the adult index estimates multiplied by lake-specific conversion factors (", paste(names(index2pe), as.numeric(index2pe), collapse=", "), ").",
    TAB=prettytable(TAB.lakewide2, -3), newpage="port")

  figu("Adult sea lamprey abundance estimates for index streams.  Targets are represented by the horizontal lines.", extraphrase,
    FIG=FIG.bar, newpage="port", w=6, h=7.5)

  figu("Relative size of adult sea lamprey population estimates (PEs) in Great Lakes index streams, ", YEAR, ".  Circle size represents size of PE, circle color represents the source of PE.",
    FIG=FIG.bubble2, newpage="land", h=5.7)


  endrtf()

}
