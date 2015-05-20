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
#' @param lakeIPEs
#'   A data frame of annual lake-wide Adult Indices with 8 columns:
#'   \code{lake}, \code{year}, the Adult Index \code{index}, its associated
#'   lower and upper jackknifed range \code{jlo} and \code{jhi},
#'   and the corresponding expansion to a supposed population estimate,
#'   \code{pe}, \code{pelo} and \code{pehi}.
#'   The data frame may contain variables other than those required.
#' @param targets
#'   A data frame with the calculated targets for the Adult Index and
#'   expanded PE of each Great Lake, with 3 columns: \code{lake},
#'   \code{targInd}, and \code{targPE}, typically the output from
#'   \code{\link{AItarget}}.
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
#'   jvamisc maps
#' @export
#'
AIreport <- function(streamPEs, lakeIPEs, targets, csvDir, outFile=NULL) {

# library(GLFC)
# library(maps)
# streamPEs=streamPE
# lakeIPEs=lakeIndPE
# targets=oldtarg
# csvDir=DIRECTORY
# outFile=NULL

  YEAR <- max(streamPEs$year)

  if (is.null(outFile)) {
    outFile  <- paste(YEAR, "Adult Index - draft report.doc")
  }

  # create nice looking table with latest year of estimates and targets
  targ2 <- merge(lakeIPEs[lakeIPEs$year==YEAR, ], targets, all=TRUE)
  row.names(targ2) <- Lakenames
  targ2$above <- with(targ2,
    ifelse(!is.na(index) & !is.na(targInd) & index > targInd, "***", ""))
  targ2$above[with(targ2, is.na(index) | is.na(targInd))] <- " ? "
  targ2 <- targ2[, c("above", "targInd", "index", "jlo", "jhi",
    "targPE", "pe", "pelo", "pehi")]
  TAB.targs <- prettytable(targ2, c(0, 0, 0, 0, 0, -3, -3, -3, -3))



  # plot lake-wide totals w/ confidence intervals on different scales
  FIG.lakeCI <- function(lakeids=1:5, k=index2pe) {
  	attach(lakeIPEs)
  	par(mfrow=c(3, 2), mar=c(3, 3, 2, 3), oma=c(2, 2, 0, 2), cex=1)
    frame()
  	for(i in seq(lakeids)) {
  		j <- lakeids[i]
  		sel <- lake==j
      mymax <- max(jhi[sel & year>=1985])/1000
  		plot(1, 1, type="n", xlim=range(year), ylim=c(0, mymax),
        xlab="", ylab="", main=Lakenames[i], las=1)
  		abline(h=oldtarg$targInd[j]/1000, col="gray", lwd=2)
  		points(year[sel], index[sel]/1000)
  		arrows(year[sel], jlo[sel]/1000, year[sel], jhi[sel]/1000, length=0.03,
        angle=90, code=3)
      p4 <- pretty(k[i]*c(0, mymax))
      axis(4, at=p4/k[i], labels=p4, las=1)
  		}
  	mtext("Year", outer=TRUE, side=1, cex=1.4)
    mtext("Adult Index  (thousands)", outer=TRUE, side=2, cex=1.4)
    mtext("Lake-wide adult abundance  (thousands)", outer=TRUE, side=4, cex=1.4)
  	detach(lakeIPEs)
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
#     ptgrp=3
#     cols=blindcolz[1+(1:length(sug))]
#     mymap=map5
#     legat="topright"
#     leginset=c(0, 0)
#     dr=range(sqrt(df[, var]), na.rm=TRUE)
#     cr=c(0.04, 0.5)
#     ox=-44
#     oy=64

  FIG.bubble1 <- function(df, group, var, lab, sug, ptgrp=NULL,
    cols=blindcolz[1+(1:length(sug))], mymap=map5,
    legat="topright", leginset=c(0, 0), dr=range(sqrt(df[, var]), na.rm=TRUE),
    cr=c(0.04, 0.5), ox=-44, oy=64) {

    g <- df[, group]
    v <- df[, var]
    if(is.null(ptgrp)) {
      n <- length(g)
      seln <- rep(TRUE, n)
    } else {
      n <- sum(table(g)[-ptgrp])
      seln <- g!=sug[ptgrp]
    }

  	xr <- range(mymap$x, na.rm=TRUE)
  	yr <- range(mymap$y, na.rm=TRUE)
  	xrw <- diff(xr)
  	yrw <- diff(yr)
  	bufx <- xrw/40
  	bufy <- yrw/40

    magic <- 20
    par(mar=c(0, 0, 0, 0))
  	map(xlim=xr + c(-1, 1)*bufx, ylim=yr + c(-magic, 1)*bufy, col=NA,
      mar=c(0, 0, 0, 0))
  	pusr <- par("usr")
  	lines(mymap$x, mymap$y, col="gray", lwd=0.5)
    attach(df)
    textx <- rep(NA, dim(df)[1])
  	textx[seln] <- seq(pusr[1], pusr[2],
      length=n+2)[-c(1, n+2)][rank(long[seln], ties.method="first")]

  	for(i in seq_along(sug)) {
  		sel <- g==sug[i]
      if (sum(sel)>0) {
        if (is.null(ptgrp) | ptgrp!=i) {
      		circles(long[sel], lat[sel], sqrt(v)[sel], data.range=dr,
            circle.size.range=cr, outx=ox, outy=oy, add=TRUE, fg=cols[i], lwd=3)
      		text(textx[sel], yr[1] - (magic-1)*bufy, df[sel, lab],
    				adj=0, srt=90, col=cols[i], cex=0.8)
    			segments(textx[sel], yr[1] - 2*bufy, long[sel], lat[sel], col=cols[i],
            lty=2)
        } else {
          points(long[sel], lat[sel], pch=3, col=cols[i], lwd=3)
        }
      }
  	}
    detach(df)

    par(xpd=NA)
  	legend(legat, sug, col=cols, lwd=3, bty="n", inset=leginset, cex=1.4)
  }

  FIG.bubble2 <- function() {
    FIG.bubble1(df=streamPEs[streamPEs$year==YEAR, ], group="categ",
        var="indexContrib", lab="cleplus",
        sug=c("Index w/ mark-recap", "Index w/o mark-recap", "Non-index"),
        ptgrp=3)
  }


  # create a file for the draft report
  doc <<- startrtf(file=outFile, dir=csvDir)

  heading(paste0("Draft Report of the ", YEAR,
    " Lake-Wide Adult Sea Lamprey Index"))
  heading(date(), 2)

  para("Authors ...", Sys.time())
  para("This is a rough draft to be used as a starting point in creating the",
    " final report.",
    "  First, save the document as a *.docx Word file (even though it has a",
    " *.doc file extension already, it's really just an *.rtf file).",
  	"  Then, select all text in the document (Ctrl-a) and increase the font",
    " size to 12.",
  	"  Finally, delete this paragraph, add page numbers and author names,",
    " edit text and insert/delete page breaks as needed.")

  para("Lake-wide estimates of the adult sea lamprey population were ...")

  # merge this year and last years' estimates
  both <- merge(lakeIPEs[lakeIPEs$year==YEAR-1, ],
    lakeIPEs[lakeIPEs$year==YEAR, ],
    by="lake", suffixes = c(".last",".this"), all=TRUE)

  attach(both)
  thyr <- round(index.this)
  layr <- round(index.last)
  hier <- jlo.this > jhi.last
  loer <- jhi.this < jlo.last
  phrase <- rep("not significantly different from", 5)
  delta <- round(100*abs(index.this - index.last) / index.last)
  detach(both)

  phrase[is.na(hier)] <- "not comparable to"
  phrase[!is.na(hier) & hier] <-
    paste0(delta[!is.na(hier) & hier], "% higher than")
  phrase[!is.na(hier) & loer] <-
    paste0(delta[!is.na(hier) & loer], "% lower than")
  sentence <- vector("list", 5)
  abta <- sum(TAB.targs$above=="***")
  beta <- sum(TAB.targs$above=="")
  for(i in 1:5) {
  	sentence[[i]] <- paste(c("For Lake ",
      Lakenames[i], ", the index in ",
      YEAR, " (",
      format(thyr[i], big.mark=","), ") was ",
      phrase[i], " the index in ",
      YEAR-1, " (",
      format(layr[i], big.mark=","), ")."), collapse="")
  	}
  para("Comparing the jacknifed ranges of the Adult Indices in ",
    YEAR, " with those in ",
    YEAR-1, ", the number of adults significantly increased in ",
    numbers2words(sum(!is.na(hier) & hier)),
    " lakes, significantly decreased in ",
  	numbers2words(sum(!is.na(hier) & loer)),
    " lakes, and stayed the same in ",
    numbers2words(sum(!is.na(hier) & !hier & !loer)),
    " lakes.", "  ",
    sentence[[1]], "  ",
    sentence[[2]], "  ",
    sentence[[3]], "  ",
    sentence[[4]], "  ",
    sentence[[5]], "  Based on the ",
    YEAR, " point estimates, ",
    numbers2words(abta), " lakes were above targets and ",
    numbers2words(beta), " were within targets (Table 2).")

  heading("REFERENCES", 2)
  para("Mullett, K. M., J. W. Heinrich, J. V. Adams, R. J. Young,",
    " M. P. Henson, R. B. McDonald, and M. F. Fodale.  2003.",
    "  Estimating lake-wide abundance",
    " of adult sea lampreys (Petromyzon marinus) in the Great Lakes:",
  	" extrapolating from sampled streams using regression models.",
    "  Journal of Great Lakes Research 29(Supplement 1):240-252.")


  TAB.lakewide1 <- with(lakeIPEs, tapply(index, list(year, lake), mean))
  colnames(TAB.lakewide1) <- Lakenames
  tabl("Adult Indices, 1985-", YEAR, ".",
    TAB=prettytable(TAB.lakewide, 0))


  TAB.lakewide2 <- with(lakeIPEs, tapply(pe, list(year, lake), mean))
  colnames(TAB.lakewide2) <- Lakenames
  tabl("Lake-wide adult population estimates, 1985-", YEAR, ".",
    TAB=prettytable(TAB.lakewide2, -3))


  tabl("Adult Indices and lake-wide adult abundance estimates with targets.",
  	"  The judgement of whether a lake is above target is based on the",
    " Adult Indices themselves, ignoring the jackknifed ranges.",
  	TAB=TAB.targs)


  figu("Adult Index estimates (with jackknifed ranges) and targets",
    " for each Great Lake through ", YEAR, ".",
  	FIG=FIG.lakeCI, newpage="port")#, w=6.5, h=7.5)


  figu("Distribution of adult sea lampreys in the Great Lakes index streams, ",
    YEAR, ".",
  	"  Circle size represents size of population estimate,",
    " circle color represents the source of the population estimate.",
  	FIG=FIG.bubble2, newpage="land")



  endrtf()

}
