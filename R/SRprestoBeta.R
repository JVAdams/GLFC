#' Sea Lamprey Status Report Generation
#'
#' Carry out a series of steps in the Sea Lamprey Status Report generation
#' process in one fell swoop.
#'
#' @param FOLDER
#'   A character scalar identifying the path where all the files are
#'   stored, e.g., \code{DIRECTORY = "C:\\\\temp\\\\mydir"}.
#' @param INDEX.LAKE
#'   A character scalar identifying the name of the csv file
#'   with historic lake-wide sea lamprey adult index data, typically
#'   \code{AdultLakeThruYYYY.csv} the output
#'   from \code{\link{AIpresto}}.
#' @param INDEX.STREAM
#'   A character scalar identifying the name of the csv file
#'   with historic stream-specific sea lamprey adult index data, typically
#'   \code{AdultStreamThruYYYY.csv} the output
#'   from \code{\link{AIpresto}}.
#' @param MAXLARVAE
#'   A character scalar identifying the name of the csv file
#'   with stream-specific maximum age-1 and older sea lamprey larvae population
#'   estimates from QAS/RS data, typically dating back to 1995.
#'   Column headers should include: "Lake", "Stream", "Estimate", "STREAM_NAM",
#'   "LATITUDE", and "LONGITUDE".
#' @param CONTROL
#'   A character scalar identifying the name of the csv file
#'   with historic lake-wide sea lamprey control data, default
#'   "ControlTable.csv".
#'   The first column is the year of control.
#'   The remaining columns are "Field Days", "TFM", and "Bayer" repeated for
#'   each Great Lake (columns 2-4 for Superior, 5-7 Michigan, 8-10 Huron,
#'   11-13 Erie, 14-16 Ontario) and the totals across all five lakes (columns
#'   17-19). ??? UNITS
#' @param TROUTSUP
#'   A character scalar identifying the name of the csv file
#'   with historic lake-wide lake trout data for Lake Superior
#'   (both catch per unit effort
#'   and sea lamprey wounding rate), default "TroutSuperior.csv".
#'   Column headers should include: "year", "cpe", "lo", "hi", and "wound".
#' @param TROUTMIC
#'   A character scalar identifying the name of the csv file
#'   with historic lake-wide lake trout data for Lake Michigan
#'   (both catch per unit effort
#'   and sea lamprey wounding rate), default "TroutMichigan.csv".
#'   Column headers should include: "year", "cpe", "lo", "hi", "wound",
#'   "wound.upper", and "wound.lower".
#' @param TROUTHUR
#'   A character scalar identifying the name of the csv file
#'   with historic lake-wide lake trout data for Lake Huron
#'   (both catch per unit effort
#'   and sea lamprey wounding rate), default "TroutHuron.csv".
#'   Column headers should include: "year", "cpe", "lo", "hi", and "wound".
#' @param TROUTERI
#'   A character scalar identifying the name of the csv file
#'   with historic lake-wide lake trout data for Lake Erie
#'   (both catch per unit effort
#'   and sea lamprey wounding rate), default "TroutErie.csv".
#'   Column headers should include: "year", "cpe", "wound",
#'   "wound.upper", and "wound.lower".
#' @param TROUTONT
#'   A character scalar identifying the name of the csv file
#'   with historic lake-wide lake trout data for Lake Ontario
#'   (both catch per unit effort
#'   and sea lamprey wounding rate), default "TroutOntario.csv".
#'   Column headers should include: "year", "cpe", "wound",
#'   "wound.upper", and "wound.lower".
#' @return
#'   Two data summaries are saved as csv files to \code{FOLDER}:
#'   \itemize{
#'     \item \code{StatusTargetsdd-Mon-YYYY.csv)} - Lake-wide lake trout
#'     wounding rate targets and sea lamprey Adult Index targets.
#'     \item \code{StatusMetricsdd-Mon-YYYY.csv} - Annual lake-wide
#'     summary of all the Status Report metrics.
#'   }
#'   One rich text documents are saved as doc files (so that MS Word will open
#'   them automatically) to \code{FOLDER}:
#'   \itemize{
#'     \item \code{Status Report Rough Draft dd-Mon-YYYY.doc} - Rough draft
#'     of the sea lamprey Status Report.
#'   }
#' @seealso \code{\link{AIpresto}}
#' @importFrom plyr ddply
#' @import zoo rtf
#' @export
#' @examples
#' \dontrun{
#'  SRprestoBeta(
#'   FOLDER="C:/JVA/GLFC/People/Siefkes/Status graphs/",
#'   INDEX.LAKE="AdultLakeThru2016.csv",
#'   INDEX.STREAM="AdultStreamThru2016.csv",
#'   MAXLARVAE="MaxLarvalEstimatesSummary2013mod2016-08-25.xls")
#' }

SRprestoBeta <- function(FOLDER, INDEX.LAKE, INDEX.STREAM, MAXLARVAE,
  CONTROL="ControlTable.csv", TROUTSUP="TroutSuperior.csv",
  TROUTMIC="TroutMichigan.csv", TROUTHUR="TroutHuron.csv",
  TROUTERI="TroutErie.csv", TROUTONT="TroutOntario.csv") {

  # set global options
  oldsci <- options("scipen")
  oldsaf <- options("stringsAsFactors")
  options(scipen=10, stringsAsFactors=FALSE)

  # set the level of the historic adult sea lamprey population for each lake
  # Superior, Michigan, Huron, Erie, Ontario
  HIST.SPAWN <- c(780, 600, 700, 40, 450)*1000

  #### Functions and vectors that will be used later ####

  # color-blind friendly colors
  col.spa <- 6 # blue
  col.day <- 2 # orange
  col.tar <- 1 # black
  col.tfm <- 5 # yellow
  col.bay <- 8 # reddish purple
  col.trt <- 3 # sky blue
  col.wou <- 4 # bluish green

  # aspect ratios for lake maps, height/width
  ASPEX <- c(5/7, 7/5, 8/7, 4/7, 4/7)

  TODAY <- format(Sys.time(), "%d-%b-%Y")

  suln <- c(Lakenames, "All")

  bubbles <- function(long, lat, group, var, thick, thicknamz, lab.dia=0.4,
    lab.space=0.1, legat="topright", leginset=c(0, 0),
  	dr=range(var), cr=c(0.04, 0.5),
    cols=blindcolz[c(2, 6, 4)], textcols=cols, ox=-44, oy=64,
    mymap=mapp, lwds=c(1, 3)) {
  	sug <- sort(unique(group))
  	xr <- range(mymap$x, na.rm=TRUE)
  	yr <- range(mymap$y, na.rm=TRUE)
  	xrw <- diff(xr)
  	yrw <- diff(yr)
  	bufx <- xrw/40
  	bufy <- yrw/40
  	map(xlim=xr + c(-1, 3)*bufx, ylim=yr + c(-1, 1)*bufy, col=NA,
      mar=c(0, 0.5, 0, 0.5))
  	pusr <- par("usr")
  	lines(mymap$x, mymap$y, col="darkgray")
  	# repeat provided text colors to be same length as unique groups
  	textcols <- rep(textcols, length=length(sug))
  	par(xpd=NA)
  	for(i in seq(sug)) {
  		sel <- group==sug[i]
  		selb <- thick & group==sug[i]
  		circles(long[sel], lat[sel], var[sel], data.range=dr,
  		  circle.size.range=cr,
        outx=ox, outy=oy, add=TRUE, fg=cols[i], lwd=lwds[1])
  		if (sum(selb)>0) {
  			text(long[selb], lat[selb]-lab.dia*var[selb]/dr[2]-lab.space,
          thicknamz[selb], srt=0, col=textcols[i], cex=1, font=2)
  			circles(long[selb], lat[selb], var[selb], data.range=dr,
          circle.size.range=cr, outx=ox, outy=oy,	add=TRUE, fg=cols[i],
          lwd=lwds[2])
  			}
  		}
  	}

  top50pct <- function(dfvar, xvar, byvar) {
  	# identify streams with the highest estimates (xvars) combining for
    #   more than half the lake (byvar) total
  	ord <- order(-dfvar[, xvar])
  	dfsplit <- split(dfvar[ord, ], dfvar[ord, byvar])
  	dfsplitbig <- lapply(dfsplit, function(df) {
  		percent <- 100*df[, xvar]/sum(df[, xvar])
  		cumpercent <- cumsum(percent)
  		selbig <- (1:dim(df)[1]) <= (sum(cumpercent<50)+1)
  		df[selbig, ]
  		})
  	do.call(rbind, dfsplitbig)
  	}


  #####


  docname <- paste0("Status Report Rough Draft ", TODAY, ".doc")
  doc <<- startrtf(file=docname, dir=FOLDER)
  heading(paste("= = = = = = =   Status Report Tables and Figures -", TODAY,
    "  = = = = = = ="))

  para("This is an automatically generated document named ", docname,
  	" stored in the directory ", FOLDER, ".",
  	" It was created from the following input files: ",
  	INDEX.LAKE, ", ", INDEX.STREAM, ", ", CONTROL, ", ", TROUTSUP, ", ",
    TROUTMIC, ", ",
  	TROUTHUR, ", ", TROUTERI, ", and ", TROUTONT,
  	" using the SRprestoBeta() function of the R package GLFC authored by Jean V. Adams.")

  para("To use the information, first select everything in the document (Ctrl-a) and change the text to the desired font and font size. Second, save the file as a Word document with the extension *.doc or *.docx, because even though it looks like a Word document initially, it's really just an rtf (rich text format) file. Finally, insert descriptive text as needed, or cut and paste into the primary document.")



  #### read in data ####



  # Lakewide adult index
  adult <- read.csv(paste0(FOLDER, INDEX.LAKE))
  names(adult)[names(adult)=="year"] <- "spawner.year"
  names(adult)[names(adult)=="ilo"] <- "index.lo"
  names(adult)[names(adult)=="ihi"] <- "index.hi"



  # Lakewide treatment effort
  control1 <- read.csv(paste0(FOLDER, CONTROL), skip=1)
  names(control1) <- casefold(names(control1))
  days <- unlist(control1[, grep("day", names(control1))])
  tfm <- unlist(control1[, grep("tfm", names(control1))])
  bayer <- unlist(control1[, grep("bayer", names(control1))])
  nobs <- length(control1$year)
  reps <- length(days)/nobs
  year <- rep(control1$year, reps)
  lake <- rep(1:6, rep(nobs, 6))
  control <- data.frame(treat.year=year, lake=lake, staff.days=days,
    tfmkgai=tfm, bayerkgai=bayer)
  control <- control[control$lake < 5.5, ]
  control$spawner.year <- control$treat.year + 2
  control <- control[control$lake < 5.5, ]
  rm(CONTROL, control1, days, tfm, bayer, nobs, reps, year, lake)



  # Lakewide lake trout relative abundance estimates and wounding rates
  troutfiles <- c(TROUTSUP, TROUTMIC, TROUTHUR, TROUTERI, TROUTONT)
  trouts <- vector("list", 5)

  # if trout data is missing a desired column,
  #   create a column of missing values for it
  selcols <- c("year", "lake", "cpe", "lo", "hi", "wound", "wlo", "whi")
  for(i in seq(troutfiles)) {
  	df <- read.csv(paste0(FOLDER, troutfiles[i]))
  	df$lake <- i
  	need.names <- setdiff(selcols, names(df))
  	if (length(need.names) > 0) {
  		df[, need.names] <- NA
  		}
  	trouts[[i]] <- df[, selcols]
  	}
  trout <- do.call(rbind, trouts)
  names(trout) <- c("trout.year", "lake", "trout", "trout.lo", "trout.hi",
    "rate", "rate.lo", "rate.hi")


  # Only Superior and Huron trout are assessed in spring
  trout$spawner.year <- ifelse((trout$lake==1 | trout$lake==3),
    trout$trout.year, trout$trout.year+1)
  trout$wound.unit <- ifelse(trout$lake==5, '# A1 marks per 100 LAT >17"',
    '# A1-A3 marks per 100 LAT >21"')

  rm(troutfiles, trouts, selcols, TROUTSUP, TROUTMIC, TROUTHUR, TROUTERI,
    TROUTONT)



  #### Targets ####
  sptargyrz <- list(1994:1998, 1995:1999, 1989:1993, 1991:1995, 1993:1997)
  TARGET <- data.frame(lake=1:5, wound.target=rep(c(5, 2), c(4, 1)),
    wound.units=rep(c("A1-A3", "A1"), c(4, 1)),
  	index.target=rep(NA, 5), intarg.lo=rep(NA, 5), intarg.hi=rep(NA, 5))
  for(i in 1:5) {
  	pick5 <- adult$index[adult$lake==i & is.element(adult$spawner.year,
      sptargyrz[[i]])]
  	TARGET$index.target[i] <- mean(pick5)
  	n <- length(pick5)
  	ci <- qnorm(1 - 0.05/2) * sqrt(var(pick5)) / sqrt(n)
  	TARGET[i, 5:6] <- mean(pick5) + c(-1, 1)*ci	# using z dist (known variance)
  	if (i==2) TARGET[i, 4:6] <- (5/8.9)*TARGET[i, 4:6]
  	if (i==3) TARGET[i, 4:6] <- 0.25*TARGET[i, 4:6]
  	}
  targyrz <- apply(sapply(sptargyrz, range), 2, paste, collapse="-")
  name.targ <- paste0("StatusTargets", TODAY, ".csv")
  write.csv(TARGET, paste0(FOLDER, name.targ), row.names=FALSE)



  #### bubble maps - adult estimates and maximum larval estimates ####

  # adult data
  stream <- read.csv(paste0(FOLDER, INDEX.STREAM))
  stream$row <- 1:dim(stream)[1]
  YEAR <- max(stream$year)
  dfthis <- stream[stream$year == YEAR, ]
  dfthis <- dfthis[order(dfthis$lake, -dfthis$indexContrib), ]

  # prepare to plot adult estimates on  map, showing source of estimate ...
  #   label those with highest estimates
  max.dia.lat <- c(0.42, 0.39, 0.28, 0.28, 0.23)
  space.below.lat <- c(0.15, 0.19, 0.16, 0.09, 0.07)

  # max larval data
  maxlarvae <- read.csv(paste0(FOLDER, MAXLARVAE))
  names(maxlarvae) <- make.names(casefold(names(maxlarvae)), unique=TRUE,
    allow_=FALSE)
  maxlarvae$row <- 1:dim(maxlarvae)[1]
  # drop "river" or "creek" from stream name
  namesplit <- strsplit(maxlarvae$stream.nam, " ")
  nameend <- sapply(namesplit, function(x) rev(x)[1])
  suffixes <- c("Brook", "Cr.", "Creek", "R.", "River", "Rivers")
  maxlarvae$stream.nam <- sapply(namesplit, function(x) {
  	keep <- if (rev(x)[1] %in% suffixes) {
      rev(x)[-1]
    } else {
      rev(x)
    }
  	paste(rev(keep), collapse=" ")
  	})

  # streams that make up top 50% of max larvae over many years
  top50maxlarvae <- top50pct(dfvar=maxlarvae, xvar="estimate", byvar="lake")
  top50maxlarvae$textpaste <- paste(top50maxlarvae$stream.nam,
    format(signif(top50maxlarvae$estimate, 2), big.mark=",", trim=TRUE))





  #### All lakewide data combined ####
  a <- merge(adult, control, all=TRUE)
  ALL <- merge(a, trout, all=TRUE)
  ALL$treat.year <- ALL$spawner.year - 2
  ALL$wound.year <- ALL$spawner.year - 1

  # the season for Lake Huron has been confirmed by Ji He 4/13/09
  ALL$season[is.element(ALL$lake, c(1, 3))] <- "spring"
  ALL$season[is.element(ALL$lake, c(2, 4, 5))] <- "fall"
  sel <- is.na(ALL$trout.year)
  ALL$trout.year[sel] <- ifelse(ALL$season[sel]=="spring",
    ALL$spawner.year[sel], ALL$spawner.year[sel]-1)

  # calculate three-year running mean for adults, marks, and trout
  ALL <- ALL[order(ALL$lake, ALL$spawner.year), ]
  varz <- c("index", "trout", "rate")
  look <- ddply(.data=ALL[, c("lake", varz)], .variables=.(lake), .drop=FALSE,
    .fun=rollapply, width=3, FUN=CI, fill=c(NA, NA, NA), align="right")
  names(look)[-(1:4)] <- paste(rep(varz, rep(3, length(varz))),
    rep(c("3mn", "3lo", "3hi"), length(varz)), sep=".")
  ALL <- cbind(ALL, look[, -(1:4)])

  name.dat <- paste0("StatusMetrics", TODAY, ".csv")
  write.csv(ALL, paste0(FOLDER, name.dat), row.names=FALSE)
  rm(adult, control, a, trout, varz, look)


  attach(ALL)

  #### status and trends ####
  addPageBreak(this=doc, width=8.5, height=11, omi=c(1, 1, 1, 1))
  heading("STATUS OF SEA LAMPREY POPULATIONS")

  para("ISSUE: This information item describes the current status of sea lamprey populations in the Great Lakes.")

  # adult 3-year status and 5-year trend
  # I changed these from spawner.year to wound.year
  sp <- SRstatus(bydat=lake, timedat=wound.year,
  	measdat=index, targdat=TARGET$index.target[1:5])
  sp$stattrnd <- paste0(sp$status, ", ", sp$trend)
  # sp1 <- SRstatus(bydat=lake, timedat=wound.year,
  # 	measdat=index, targdat=TARGET$index.target[1:5],
  #   status.length=1, trend.length=NULL)

  # wounding 3-year status and 5-year trend
  # I changed these from trout.year to wound.year
  wo <- SRstatus(bydat=lake, timedat=wound.year,
  	measdat=rate, targdat=TARGET$wound.target[1:5])
  wo$stattrnd <- paste(wo$status, wo$trend, sep=", ")
  # wo1 <- SRstatus(bydat=lake, timedat=wound.year,
  # 	measdat=rate, targdat=TARGET$wound.target[1:5],
  #   status.length=1, trend.length=NULL)
  # trout 5-year trend
  tr <- SRstatus(bydat=lake, timedat=wound.year,
  	measdat=trout, targdat=NULL, status.length=NULL)

  para("SUMMARY: Sea lamprey control program success is measured by index estimates of adult sea lamprey abundance, sea lamprey marking rates on lake trout, and lake trout relative abundance. The overall status of these metrics is presented in this table; status is based on the mean over the last 3 years relative to target, trends are based on the slope over the last 5 years. (See also the interactive status metrics, http://www.glfc.org:3838/slcp/.)")

  tab <- cbind(sp[, c("bydat", "stattrnd")], wo[, "stattrnd"], tr[, "trend"])
  tab$bydat <- Lakenames[tab$bydat]
  names(tab) <- c("Lake", "Sea Lamprey", "Marks", "Lake Trout")
  tabl("", TAB=tab, row.names=FALSE)

  detach(ALL)

  # REPORT CARD
  # merge metrics data with targets
  ALL2 <- merge(ALL, TARGET[, c("lake", "wound.target", "index.target")],
    all.x=TRUE)
  ALL2 <- ALL2[!is.na(ALL2$spawner.year), ]

  attach(ALL2)

  last5 <- (YEAR - spawner.year) < 4.5
  last1 <- spawner.year == YEAR

  labsep <- function(x, minsep=diff(range(x))/(2*(length(x) - 1))) {
    ord <- order(x)
    dif1 <- diff(x[ord])
    dif2 <- dif1
    dif2[dif2<minsep] <- minsep
    dif3 <- dif2 * sum(dif1) / sum(dif2)
    y <- x
    y[ord] <- cumsum(c(min(x), dif3))
    y
  }

  xr <- range(spawner.year[(YEAR - spawner.year) < 4.5 &
      (!is.na(index) | !is.na(rate))]) - 1
  yr <- range((index/index.target)[last5 & !is.na(index)],
    (rate/wound.target)[last5 & !is.na(rate)], na.rm=TRUE)
  fig <- function() {
  	par(mfrow=c(1, 2), mar=c(2, 3, 2, 4), oma=c(1, 1, 0, 0))
  	plot(wound.year, index/index.target, type="n", las=1,
      xlim=xr, ylim=yr, xlab="", ylab="")
  	mtext("Adult sea lamprey index", side=3, line=0.5)
  	pusr <- par("usr")
  	polygon(pusr[c(1, 2, 2, 1)], c(pusr[c(3, 3)], 1, 1), col="lightgray",
      border=NA)
  	box()
  	labeloc <- labsep((index/index.target)[spawner.year==YEAR])
  	for(i in 1:5) {
  		sel <- lake==i
  		lines(wound.year[sel], (index/index.target)[sel],
        col=blindcolz[c(2, 3, 4, 6, 8)[i]], lwd=2, lty=i,
        lend="square")
  		mtext(Lakenames[i], at=labeloc[i], col=blindcolz[c(2, 3, 4, 6, 8)[i]],
  		  side=4, las=2, line=0.5)
  		}
  	plot(wound.year, rate/wound.target, type="n", las=1,
  	  xlim=xr, ylim=yr, xlab="", ylab="")
  	mtext("Lake trout marking rate", side=3, line=0.5)
  	pusr <- par("usr")
  	polygon(pusr[c(1, 2, 2, 1)], c(pusr[c(3, 3)], 1, 1), col="lightgray",
      border=NA)
  	box()
  	labeloc <- labsep(with(ALL2, tapply(rate/wound.target, lake,
  	  function(x) rev(x[!is.na(x)])[1])))
  	for(i in 1:5) {
  		sel <- lake==i
  		lines(wound.year[sel], (rate/wound.target)[sel],
        col=blindcolz[c(2, 3, 4, 6, 8)[i]], lwd=2, lty=i,
        lend="square")
  		mtext(Lakenames[i], at=labeloc[i], col=blindcolz[c(2, 3, 4, 6, 8)[i]],
  		  side=4, las=2, line=0.5)
  		}
  	mtext("Marking year", side=1, outer=TRUE)
  	mtext("Relative to target", side=2, outer=TRUE)
  }

  para("Status metrics, relative to target, for each of the Great Lakes from marking years ", paste0(xr, collapse="-"), " are graphed below.")

  figu("", FIG=fig, h=3.29, w=6.5)




  #### Plots of all lakes together ####

  sul <- sort(unique(lake))

  # adults, wounding, trout, all lakes

  bigfig1 <- function() {
  	par(mfcol=c(3, 5), mar=c(3, 3, 0, 0), oma=c(2, 3, 2, 1), yaxs="i",
      cex=1, bty="l")
  	mycex <- par("cex")
  	xr <- range(wound.year[!is.na(index) | !is.na(rate) | !is.na(trout)],
  	  na.rm=TRUE)
  	ind2targmax <- max(index.hi/index.target,  na.rm=TRUE)
  	rat2targmax <- max(rate/wound.target, rate.hi/wound.target, na.rm=TRUE)

  	for(i in 1:5) {
  		sel <- lake==sul[i]
  		indyr <- 1.02*c(0, ind2targmax*TARGET$index.target[i])
  		propfac <- 100*index2pe[i]/HIST.SPAWN[i]
  		ratyr <- 1.02*c(0, rat2targmax*TARGET$wound.target[i])
  		trtmax <- max(trout[sel], trout.hi[sel], na.rm=TRUE)
  		trtyr <- 1.02*c(0, trtmax)

			plot(wound.year[sel], propfac*index[sel], pch=18,
			  col=blindcolz[col.spa], xlim=xr, ylim=propfac*indyr,
			  xlab="", ylab="")
			abline(h=propfac*TARGET$index.target[i],
			  col=blindcolz[col.tar], lty=3)
			arrows(wound.year[sel], propfac*index.lo[sel], wound.year[sel],
			  propfac*index.hi[sel],
			  angle=90, length=0.02, code=3, col=blindcolz[col.spa])
			if (i==1) mtext("Adult sea lamprey\nindex  (% historic)",
			  side=2, line=3, cex=1.5*mycex)
			mtext(suln[i], side=3, line=0.2, cex=1.5*mycex)

			plot(wound.year[sel], rate[sel], pch=16, col=blindcolz[col.wou],
        xlim=xr, ylim=ratyr, xlab="", ylab="")
			arrows(wound.year[sel], rate.lo[sel], wound.year[sel], rate.hi[sel],
				angle=90, length=0.02, code=3, col=blindcolz[col.wou])
			abline(h=wo$targdat[i], col=blindcolz[col.tar], lty=3)
 			if (i==1) mtext("Lake trout\nmarking rate", side=2, line=3,
			  cex=1.5*mycex)

			plot(wound.year[sel], trout[sel], pch=15, col=blindcolz[col.trt],
        xlim=xr, ylim=trtyr, xlab="", ylab="")
			arrows(wound.year[sel], trout.lo[sel], wound.year[sel], trout.hi[sel],
				angle=90, length=0.02, code=3, col=blindcolz[col.trt])
			if (i==1) mtext("Lake trout\nabundance  (CPE)", side=2, line=3,
        cex=1.5*mycex)

  		}
  	mtext("Marking year", side=1, outer=TRUE, line=0.2, cex=1.5*mycex)
  	}



  # staff days, tfm, all lakes
  bigfig2 <- function() {
    par(mfcol=c(3, 5), mar=c(3, 3, 0, 0), oma=c(2, 3, 2, 1), yaxs="i",
      cex=1, bty="l")
    mycex <- par("cex")
  	xr <- range(wound.year[!is.na(staff.days) | !is.na(tfmkgai) |
  	    !is.na(bayerkgai)], na.rm=TRUE)
  	dayyr <- 1.02*c(0, max(staff.days, na.rm=TRUE))
  	tfmyr <- 1.02*c(0, max(tfmkgai, na.rm=TRUE))
  	byryr <- 1.02*c(0, max(bayerkgai, na.rm=TRUE))

  	for(i in 1:5) {
  		sel <- lake==sul[i]

			plot(wound.year[sel], staff.days[sel]/100, type="n",
        xlab="", ylab="", xlim=xr, ylim=dayyr/100)
			symbols(wound.year[sel], staff.days[sel]/200,
        rectangle=cbind(0.6, staff.days[sel]/100),
				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.day])
			if (i==1) mtext("Control field days\n(hundreds)", side=2, line=3,
        cex=1.5*mycex)
  		mtext(suln[i], side=3, line=0.2, cex=1.5*mycex)

			plot(wound.year[sel], tfmkgai[sel]/1000, type="n",
			  xlab="", ylab="",	xlim=xr, ylim=tfmyr/1000)
			symbols(wound.year[sel], tfmkgai[sel]/2000,
        rectangle=cbind(0.6, tfmkgai[sel]/1000),
				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.tfm])
			if (i==1) mtext("TFM kg AI\n(thousands)", side=2, line=3, cex=1.5*mycex)

			plot(wound.year[sel], bayerkgai[sel], type="n", xlab="", ylab="",
				xlim=xr, ylim=byryr)
			symbols(wound.year[sel], bayerkgai[sel]/2,
        rectangle=cbind(0.6, bayerkgai[sel]),
				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.bay])
			if (i==1) mtext("Bayer kg AI", side=2, line=3, cex=1.5*mycex)
  		}
  	mtext("Marking year", side=1, outer=TRUE, line=0.2, cex=1.5*mycex)
  	}





  #### plots of individual lakes separately ####

  # adults, wounding, trout, staff days, tfm
  for(i in 1:5) {
  	GLFCenv$figcount <- 1
  	sel1 <- lake==sul[i]

  	addPageBreak(this=doc, width=8.5, height=11, omi=c(1, 1, 1, 1))
  	heading(paste("STATUS OF SEA LAMPREY CONTROL IN LAKE",
  	  casefold(Lakenames[i], upper=TRUE)))

  	heading("Adult Sea Lamprey:", 2)

  	para("* Highlights in bullet points ...")
  	para(" ")

		fig <- function() {
			par(mar=c(4, 5, 0.5, 0.5), oma=c(0, 4, 0, 0), yaxs="i", las=1)
		  sel <- sel1 & !is.na(index)
		  xr <- range(wound.year[sel]) + c(-3, 0)
		  yr <- 1.02*range(0, index.hi[sel]/1000)
			plot(wound.year[sel], index[sel]/1000, type="n", axes=FALSE,
			  xlim=xr, ylim=yr,
        xlab="Marking year", ylab="Population estimate  (thousands)")
      abline(h=TARGET$index.target[i]/1000, col=blindcolz[col.tar], lty=3)
			arrows(wound.year[sel], index.lo[sel]/1000, wound.year[sel],
        index.hi[sel]/1000,
				angle=90, length=0.02, code=3, col=blindcolz[col.spa])
			points(wound.year[sel], index[sel]/1000, pch=18,
        col=blindcolz[col.spa])
			axis(1)
			box(bty="l")

			plab <- pretty(yr * index2pe[i])
			axis(2, at=plab/index2pe[i], labels=plab)

			axis(2, outer=TRUE)
			mtext("Adult sea lamprey index  (thousands)", side=2, line=8, las=0,
			  at=mean(par("usr")[3:4]))

			mtext(paste0("\u25B2\n \n", round(HIST.SPAWN[i]/1000), "\nHist."),
			  side=3, line=-4, at=xr[1]+1, font=2, col=blindcolz[col.spa])
			mtext("|", side=3, line=-1.5, at=xr[1]+1, font=2,
			  col=blindcolz[col.spa])
			}
    targphrase <- paste0("the mean of indices during a period with",
      " acceptable marking rates (")
    if(i==2) targphrase <- c("5/8.9 times the mean of indices (")
    if(i==3) targphrase <- c("0.25 times the mean of indices (")
		figu("Adult sea lamprey index with 95% confidence intervals and historic pre-control abundance (on the PE scale).  (See background for explanation of target.)",
		  FIG=fig, h=2.64, w=3.96)


  	# bubble plots
  	fig <- function() {
  		mapp <<- mapL[[i]]
  		par(mar=c(0, 0, 0, 0), xpd=TRUE, mfrow=c(1, 2))
  		# adults, one year
  		df <- dfthis[dfthis$lake==i, ]
  		selbig <- rep(TRUE, dim(df)[1])
  		bubbles(long=df$long, lat=df$lat, group=(df$index==TRUE & is.na(df$PEmr)),
        var=df$indexContrib, thick=selbig, thicknamz=df$strname,
  			lab.dia=max.dia.lat[i], lab.space=space.below.lat[i], cr=c(0.04, 0.2),
        cols=blindcolz[c(6, 2)], lwds=c(2, 2))
  		# larvae, multiple years
  		df <- maxlarvae[maxlarvae$lake==i, ]
  		selbig <- df$row %in% top50maxlarvae$row[top50maxlarvae$lake==i]
  		bubbles(long=df$longitude, lat=df$latitude, group=rep(TRUE, dim(df)[1]),
        var=df$estimate, thick=selbig, thicknamz=df$stream.nam,
  			lab.dia=max.dia.lat[i], lab.space=space.below.lat[i], cr=c(0.04, 0.2),
        cols=blindcolz[3], textcols="black")
  		}
  	figu("LEFT: Adult sea lamprey index during the ", YEAR, " spring spawning migration, (marking year ", YEAR-1, "). Circle size corresponds to estimated number of adults from mark-recapture studies (blue) and model predictions (orange) in index streams. RIGHT: Maximum estimated number of larval sea lampreys in each stream surveyed during 1995-2012. More than half of the lake-wide larval production is attributed to ",
  	  length(top50maxlarvae$textpaste[top50maxlarvae$lake==i]),
  		" tributaries (",
  		paste(top50maxlarvae$textpaste[top50maxlarvae$lake==i], collapse="; "),
      ").", FIG=fig,
  		h=ASPEX[i]*3.24, w=6.5)

  	heading("Lake Trout Marking and Relative Abundance:", 2)

  	para("* Highlights in bullet points ...")
  	para(" ")

  	# Figure Captions, in this order: Superior, Michigan, Huron, Erie, Ontario
  	mark.fig.captions <- c(
  	  "Lake trout surveys conducted in April and May of year t reflect marking in year t-1.",
  	  "Lake trout surveys conducted in August and November reflect marking in the same year.",
  	  "Lake trout surveys conducted during April-June of year t reflect marking in year t-1.",
  	  "Lake trout surveys conducted in August reflect marking in the same year.",
  	  "Lake trout surveys conducted in September reflect marking in the same year.")

  	utarg <- TARGET$wound.units[i]
  	mark.time <- c("", "during August-November ", "", "", "")
  	sel <- sel1
		fig <- function() {
			par(mar=c(4.5, 5, 0.5, 1), yaxs="i", bty="l")
			plot(wound.year[sel], rate[sel], type="n",
        xlim=range(wound.year[sel & !is.na(rate)], na.rm=TRUE),
			  ylim=1.02*range(0, rate[sel], rate.hi[sel], na.rm=TRUE),
        xlab="Marking year", ylab="Lake trout\nmarking rate")
			arrows(wound.year[sel], rate.lo[sel], wound.year[sel], rate.hi[sel],
				angle=90, length=0.02, code=3, col=blindcolz[col.wou])
			abline(h=wo$targdat[i], col=blindcolz[col.tar], lty=3)
			points(wound.year[sel], rate[sel], pch=16, col=blindcolz[col.wou])
			}
		figu("Number of ", utarg, " marks per 100 lake trout > ", rep(c(532, 431),
      c(4, 1))[i], " mm",
			" from standardized assessments ", mark.time[i],
      "plotted against the sea lamprey marking year.  ", mark.fig.captions[i],
		  FIG=fig, h=2.64, w=3.96)

    # Figure Captions, in this order: Superior, Michigan, Huron, Erie, Ontario
		trout.fig.captions <- c(
		  'Lake trout relative abundance (using 4.5 inch gillnets) plotted against sea lamprey marking year.  CPE = fish/km/net night of lean lake trout > 532 mm (21") total length.',
		  paste0("Lake trout relative abundance plotted against sea lamprey marking year. CPE = fish/1000'/net night", ' of lean lake trout > 532 mm (21") total length caught in the Lake Wide Assessment Plan nets (the plan began in the late 1990s).'),
		  'Lake trout relative abundance from standardized surveys (2-6 inch mesh) in U.S. waters of the main basin plotted against sea lamprey marking year. CPE = geometric mean of fish/km/net night of lean lake trout > 532 mm (21") total length.',
		  "Lake trout relative abundance from standardized ?SPRING? surveys plotted against sea lamprey marking year. CPE = number per lift of lean lake trout age 5 and older.",
		  'Lake trout relative abundance plotted against sea lamprey marking year. CPE = fish/km/net night of lean lake trout > 431 mm (17") total length.')

		fig <- function() {
			par(mar=c(4.5, 5, 0.5, 1), yaxs="i", bty="l")
			plot(wound.year[sel], trout[sel], pch=15, col=blindcolz[col.trt],
        xlim=range(wound.year[sel & !is.na(trout)], na.rm=TRUE),
			  ylim=1.02*range(0, trout[sel], trout.hi[sel], na.rm=TRUE),
        xlab="Marking year", ylab="Lake trout\nabundance  (CPE)")
			arrows(wound.year[sel], trout.lo[sel], wound.year[sel],
			  trout.hi[sel],
				angle=90, length=0.02, code=3, col=blindcolz[col.trt])
			}
		figu(trout.fig.captions[i], FIG=fig, h=2.64, w=3.96)

  	heading("Lampricide Control - Field Days, TFM, and Bayluscide:", 2)

  	para("* Highlights in bullet points ...")
  	para(" ")

  	fig <- function() {
  		par(mar=c(3, 5, 0.5, 5), oma=c(2, 0, 0, 0), mfrow=c(3, 1), yaxs="i",
  		  cex=1, bty="l")
  		mycex <- par("cex")
  		xr <- range(wound.year[sel &
  		  (!is.na(staff.days) | !is.na(tfmkgai) | !is.na(bayerkgai))], na.rm=TRUE)
			plot(wound.year[sel], staff.days[sel]/100, type="n",
        xlim=xr, ylim=1.02*range(0, staff.days[sel]/100, na.rm=TRUE),
			  xlab="", ylab="Control field days\n(hundreds)")
			symbols(wound.year[sel], staff.days[sel]/200,
        rectangle=cbind(0.6, staff.days[sel]/100),
				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.day])
			plot(wound.year[sel], tfmkgai[sel]/1000, type="n",
			  xlim=xr, ylim=1.02*range(0, tfmkgai[sel]/1000, na.rm=TRUE), xlab="",
			  ylab="TFM kg AI\n(thousands)")
			symbols(wound.year[sel], tfmkgai[sel]/2000,
        rectangle=cbind(0.6, tfmkgai[sel]/1000),
				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.tfm])
			plot(wound.year[sel], bayerkgai[sel], type="n",
			  xlim=xr, ylim=1.02*range(0, bayerkgai[sel], na.rm=TRUE), xlab="",
			  ylab="Bayer kg AI")
			symbols(wound.year[sel], bayerkgai[sel]/2,
        rectangle=cbind(0.6, bayerkgai[sel]),
				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.bay])
  		mtext("Marking year", side=1, outer=TRUE, cex=1.2*mycex)
  		}
  	figu("Number of control field days, TFM used (kg active ingredient), and Bayluscide used (kg active ingredient). Field days, TFM, and Bayluscide are offset by 1 year (e.g., effort in 2015 is plotted on the 2016 marking year, when control would first reduce the sea lamprey population in the lake).",
  	  FIG=fig, h=6, w=3.24)
  	}



  heading("BACKGROUND")
  heading("Index Estimates of Adult Sea Lamprey Abundance", 2)

  tab <- TARGET
  tab$Lake <- Lakenames
  tab$Years <- sapply(sptargyrz, function(x) paste(range(x), collapse="-"))
  tab$Adjust <- round(c(1, 5/8.9, 0.25, 1, 1), 2)
  tab$Target <- round(tab$index.target)
  tab <- tab[, c("Lake", "Years", "Adjust", "Target")]
  tabl("", TAB=tab, row.names=FALSE)

  heading("Sources of Sea Lampreys", 2)
  heading("Lake Trout Marking Rates", 2)
  heading("Lake Trout Relative Abundance", 2)
  heading("Lampricide Control Effort", 2)
  heading("Current Status of Sea Lamprey Control in the Great Lakes", 2)
  heading("Sea Lamprey Status in the St. Marys River", 2)
  heading("Current Status of Sea Lamprey Control in the St. Marys River", 2)



  GLFCenv$figcount <- 1



  figbig("Row 1: Index and 95% confidence interval of adult sea lamprey index as the percentage of historic levels. Target is dashed line.  Row 2: A1-A3 marking rates on lake trout > 532 mm (for Lake Ontario, A1 only on lake trout > 431 mm). Target is dashed line. Row 3: Lake trout abundance. For lakes Superior and Huron, CPE is fish/km/net night of lean lake trout > 532 mm total length; for Lake Michigan, CPE is fish/1000'/net night of lean lake trout > 532 mm total length; for Lake Erie, CPE is relative abundance of age 5 and older lake trout sampled in east basin gill nets; and for Lake Ontario, CPE is fish/0.1476 km/net night of lean lake trout > 431 mm total length. All metrics plotted against the sea lamprey marking year.",
  	FIG=bigfig1, newpage="land")

  figbig("Row 1: Number of control field days. Row 2: TFM used (kg active ingredient). Row 3: Bayluscide used (kg active ingredient). All metrics plotted against the sea lamprey marking year. Control metrics are offset by 1 year, e.g., control applied during 2015 is plotted on the 2016 marking year, when the treatment effect would first affect the juvenile sea lamprey population in the lake).",
   	FIG=bigfig2, newpage="land")



  detach(ALL2)

  # return options to original settings
  options(scipen=oldsci, stringsAsFactors=oldsaf)

  endrtf()

}
