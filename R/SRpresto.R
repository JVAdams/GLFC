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
#'   A character scalar identifying the name of the Excel spreadsheet
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
#' @param AXISRANGES
#'   A character scalar identifying the name of the Excel spreadsheet
#'   with specified axis ranges for the plots in the Status Report, default
#'   "AxisRanges.xls".
#'   Column headers should include: "order", "metric", "order", "lake", "from",
#'   "to", "by", "from", "to", and "by" corresponding to the metric order and
#'   name, lake order and name, then the minimum (from), maximum (to) and
#'   spacing (by) tick marks for individual graphs (columns 5-7) and the big
#'   one-page graphs (columns 8-10).
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
#' @importFrom readxl read_excel
#' @importFrom plotrix rescale
#' @importFrom plyr ddply
#' @import ggrepel
#' @import ggplot2
#' @import rtf
#' @import maps
#' @export
#' @examples
#' \dontrun{
#' SRpresto(
#'   FOLDER="C:/JVA/GLFC/People/Siefkes/Status graphs/",
#'   INDEX.LAKE="AdultLakeThru2016.csv",
#'   INDEX.STREAM="AdultStreamThru2016.csv",
#'   MAXLARVAE="MaxLarvalEstimatesSummary2013mod2016-08-25.xls"
#' )
#' }

SRpresto <- function(FOLDER, INDEX.LAKE, INDEX.STREAM, MAXLARVAE,
  CONTROL="ControlTable.csv", TROUTSUP="TroutSuperior.csv",
  TROUTMIC="TroutMichigan.csv", TROUTHUR="TroutHuron.csv",
  TROUTERI="TroutErie.csv", TROUTONT="TroutOntario.csv",
  AXISRANGES="AxisRanges.xls") {

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

  top50pct <- function(dfvar, xvar, byvar) {
  	# identify streams with the highest estimates (xvars) combining for
    #   more than half the lake (byvar) total
  	ord <- order(-dfvar[[xvar]])
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

  # set global options
  oldsci <- options("scipen")
  oldsaf <- options("stringsAsFactors")
  options(scipen=10, stringsAsFactors=FALSE)

  docname <- paste0("Status Report Rough Draft ", TODAY, ".doc")
  doc <<- startrtf(file=docname, dir=FOLDER)
  heading(paste("= = = = = = =   Status Report Tables and Figures -", TODAY,
    "  = = = = = = ="))

  para("This is an automatically generated document named ", docname, " stored in the directory ", FOLDER, ". It was created from the following input files: ", INDEX.LAKE, ", ", INDEX.STREAM, ", ", CONTROL, ", ", TROUTSUP, ", ", TROUTMIC, ", ", TROUTHUR, ", ", TROUTERI, ", ", TROUTONT, ", and ", AXISRANGES, " using the SRpresto() function of the R package GLFC authored by Jean V. Adams.")

  para("To use the information, first select everything in the document (Ctrl-a) and change the text to the desired font and font size. Second, save the file as a Word document with the extension *.doc or *.docx, because even though it looks like a Word document initially, it's really just an rtf (rich text format) file. Finally, insert descriptive text as needed, or cut and paste into the primary document.")



  #### read in data ####

  # bring in the range data for the x-axis and all the y-axes
  axisr <- read_excel(paste0(FOLDER, AXISRANGES), skip=2, na="NA",
    col_names=c("m.order", "metric", "l.order", "lake",
      "ind.from", "ind.to", "ind.by", "big.from", "big.to", "big.by")) %>%
    replace_na(list(big.from=0, big.to=0, big.by=0))

  # store the tick mark info in an array of lists
  um <- unique(axisr$metric)
  axisra <- array(list(NULL), dim=c(length(um), 5, 2),
    dimnames=list(um, 1:5, c("Indiv", "BigFig")))
  for(i in seq(um)) {
  for(j in 1:5) {
  	sel <- axisr$metric==um[i] & axisr$l.order==j
  	axisra[[i, j, 1]] <-
  	  seq(axisr$ind.from[sel], axisr$ind.to[sel], axisr$ind.by[sel])
  	axisra[[i, j, 2]] <-
  	  seq(axisr$big.from[sel], axisr$big.to[sel], axisr$big.by[sel])
  	}}



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
  sptargyrz <- list(1994:1998, 1988:1992, 1989:1993, 1991:1995, 1993:1997)
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
  maxlarvae <- read_excel(paste0(FOLDER, MAXLARVAE))
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

  # the season for Lake Huron has been confirmed by Ji He 4/13/09
  ALL$season[is.element(ALL$lake, c(1, 3))] <- "spring"
  ALL$season[is.element(ALL$lake, c(2, 4, 5))] <- "fall"
  sel <- is.na(ALL$trout.year)
  ALL$trout.year[sel] <- ifelse(ALL$season[sel]=="spring",
    ALL$spawner.year[sel], ALL$spawner.year[sel]-1)

  # calculate three-year running mean for adults, marks, and trout
  a1 <- SRrun3(ALL, "index", "lake", "spawner.year")
  a2 <- SRrun3(a1, "trout", "lake", "spawner.year")
  a3 <- SRrun3(a2, "rate", "lake", "spawner.year")

  maxtrtyrz <- a3 %>%
    filter(!is.na(rate) | !is.na(trout)) %>%
    group_by(season) %>%
    summarise(mty=max(spawner.year))

  ALL <- a3 %>%
    full_join(maxtrtyrz, by="season") %>%
    mutate(
      # make sure moving averages don't push into the future
      index.3mn = ifelse(spawner.year>YEAR, NA, index.3mn),
      trout.3mn = ifelse(spawner.year>mty, NA, trout.3mn),
      rate.3mn = ifelse(spawner.year>mty, NA, rate.3mn)
    ) %>%
    select(-mty)

  name.dat <- paste0("StatusMetrics", TODAY, ".csv")
  write.csv(ALL, paste0(FOLDER, name.dat), row.names=FALSE)
  rm(adult, control, a, a1, a2, a3, trout)


  #### status and trends ####
  addPageBreak(this=doc, width=8.5, height=11, omi=c(1, 1, 1, 1))
  heading("STATUS OF SEA LAMPREY POPULATIONS")

  para("Issue: This information item describes the current status of sea lamprey populations in the Great Lakes.")




  a1 <- SRtrend5(ALL, "index", "lake", "spawner.year", lasttime=YEAR)
  a2 <- SRtrend5(ALL, "trout", "lake", "spawner.year", lasttime=YEAR)
  a3 <- SRtrend5(ALL, "rate", "lake", "spawner.year", lasttime=YEAR)

  allsmry <- ALL %>%
    filter(spawner.year==YEAR) %>%
    select(lake, spawner.year, index, trout, rate, index.3mn, trout.3mn,
      rate.3mn) %>%
    full_join(a1, by=c("lake", "spawner.year")) %>%
    full_join(a2, by=c("lake", "spawner.year")) %>%
    full_join(a3, by=c("lake", "spawner.year")) %>%
    full_join(TARGET, by="lake")

  status.table <- allsmry %>%
    mutate(
      Lake = Lakenames,
      sltarg = ifelse(index.3mn<=index.target, "Met", "Above"),
      sltrnd = case_when(
        index.5pv >= 0.05 ~ "Steady",
        index.5sl > 0 ~ "Increasing",
        TRUE ~ "Decreasing"),
      mxtarg = ifelse(rate.3mn<=wound.target, "Met", "Above"),
      mxtrnd = case_when(
        rate.5pv >= 0.05 ~ "Steady",
        rate.5sl > 0 ~ "Increasing",
        TRUE ~ "Decreasing"),
      `Lake Trout` = case_when(
        trout.5pv >= 0.05 ~ "Steady",
        trout.5sl > 0 ~ "Increasing",
        TRUE ~ "Decreasing"),
      `Sea Lamprey` = paste(sltarg, sltrnd, sep=", "),
      Marks = paste(mxtarg, mxtrnd, sep=", ")
    ) %>%
    select(Lake, `Sea Lamprey`, Marks, `Lake Trout`)

  para("Summary: Sea lamprey control program success is measured by index estimates of adult sea lamprey abundance, sea lamprey marking rates on lake trout, and lake trout relative abundance. The overall status of these metrics in each lake is presented in the table below. The status of sea lamprey abundance and marking rates on lake trout are based on the mean over the last 3 years relative to target and trends are based on the slope over the last 5 years. Lake trout abundance is also reported using a 3-year average and 5-year trend, but there are no targets for lake trout abundance in the context of the sea lamprey control program. Single year point estimates can fluctuate and can have wide error bars, thus the focus on 3-year averages and 5-year trends.")

  tabl(" ", TAB=status.table, row.names=FALSE)

  # REPORT CARD
  # 3 year mean of adult index and wounding rates relative to target
  # merge metrics data with targets
  ALL2 <- merge(ALL, TARGET[, c("lake", "wound.target", "index.target")],
    all.x=TRUE) %>%
    mutate(
      index.rat = index.3mn/index.target,
      wound.rat = rate.3mn/wound.target,
      Lake = factor(Lakenames[lake], levels=Lakenames)
    ) %>%
    select(Lake, spawner.year, index.rat, wound.rat) %>%
    pivot_longer(cols=index.rat:wound.rat, names_to="metric",
      values_to="ratio") %>%
    mutate(
      metric = ifelse(metric=="index.rat", "Adult index (3-yr mean)",
        "Marking rate (3-yr mean)")
    ) %>%
    filter(
      !is.na(ratio),
      spawner.year %in% (YEAR - (4:0))
      )

  fig <- function() {
    p <- ggplot(ALL2, aes(spawner.year, ratio, color=Lake)) +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=1,
        fill="lightgray", color = NA) +
      geom_line(size=1) +
      facet_wrap(~ metric) +
      labs(x="Spawning year", y="Relative to target") +
      theme_bw(base_size = 12)
    print(p)
  }


  attach(ALL)

  figu("Status metrics, relative to target, for each of the Great Lakes ",
    paste(range(ALL2$spawner.year), collapse="-"), ".",
    FIG=fig, h=3.29, w=6.5)




  #### Plots of all lakes together ####

  sul <- sort(unique(lake))

  # figure out max ylim for each spawner index plot
  # so that the plots all have the target at the same level
  maxindex <- ALL %>%
    group_by(lake) %>%
    summarise(maxi=max(index, na.rm=TRUE)) %>%
    mutate(
      targ = TARGET$index.target,
      relmaxi = maxi/targ,
      mrm = max(relmaxi), # maximum relative max.
      ylimmax = 1.05*mrm*targ
    )

  # adults, wounding, trout, all lakes
  bigfig1 <- function() {
  	par(mfcol=c(3, 5), mar=c(3, 3, 0, 0), oma=c(2, 3, 2, 1), xaxs="i", yaxs="i",
      cex=1)
  	mycex <- par("cex")
  	for(i in 1:5) {
  		sel <- lake==sul[i]
  		year.tk <- axisra[["Year", i, 2]]
  		trout.tk <- axisra[["Trout", i, 2]]
  		wound.tk <- axisra[["Wounds", i, 2]]

  		if (sum(!is.na(index[sel]))>0) {
  			plot(spawner.year[sel], index[sel]/1000, type="n", axes=FALSE,
          xlim=range(year.tk),
  				ylim=c(0, maxindex$ylimmax[i])/1000, xlab="", ylab="")
  		  lines(spawner.year[sel], index.3mn[sel]/1000, col=blindcolz[col.spa])
  			abline(h=TARGET$index.target[i]/1000, col=blindcolz[col.tar])
  			arrows(spawner.year[sel], index.lo[sel]/1000, spawner.year[sel],
          index.hi[sel]/1000,
  				angle=90, length=0.02, code=3, col=blindcolz[col.spa])
  			points(spawner.year[sel], index[sel]/1000, pch=18,
          col=blindcolz[col.spa])
  			if (i==1) mtext("Adult sea lamprey\nindex  (thousands)", side=2,
          line=3, cex=1.5*mycex)
  			axis(1, at=year.tk)
  			axis(2, las=2, adj=1)
  			box(bty="l")
  			mtext(suln[i], side=3, line=0.2, cex=1.5*mycex)
  			} else {
          frame()
  			}

  		if (sum(!is.na(rate[sel]))>0) {
  			plot(spawner.year[sel], rate[sel], type="n", axes=FALSE,
          xlim=range(year.tk), ylim=range(wound.tk), xlab="", ylab="")
  		  lines(spawner.year[sel], rate.3mn[sel], col=blindcolz[col.wou])
  			arrows(spawner.year[sel], rate.lo[sel], spawner.year[sel], rate.hi[sel],
  				angle=90, length=0.02, code=3, col=blindcolz[col.wou])
  			abline(h=allsmry$wound.target[i], col=blindcolz[col.tar])
  			points(spawner.year[sel], rate[sel], pch=16, col=blindcolz[col.wou])
  			if (i==1) mtext("Lake trout\nmarking rate", side=2, line=3,
  			  cex=1.5*mycex)
  			axis(1, at=year.tk)
  			axis(2, las=2, adj=1, at=wound.tk)
  			box(bty="l")
  			} else {
          frame()
  			}

  		if (sum(!is.na(trout[sel]))>0) {
  			plot(spawner.year[sel], trout[sel], type="n", axes=FALSE,
          xlim=range(year.tk), ylim=range(trout.tk), xlab="", ylab="")
  		  lines(spawner.year[sel], trout.3mn[sel], col=blindcolz[col.trt])
  			arrows(spawner.year[sel], trout.lo[sel], spawner.year[sel],
  			  trout.hi[sel],
  				angle=90, length=0.02, code=3, col=blindcolz[col.trt])
  			points(spawner.year[sel], trout[sel], pch=15, col=blindcolz[col.trt])
  			if (i==1) mtext("Lake trout\nabundance  (CPE)", side=2, line=3,
          cex=1.5*mycex)
  			axis(1, at=year.tk)
  			axis(2, las=2, adj=1, at=trout.tk)
  			box(bty="l")
  			} else {
          frame()
  			}

  		}
  	mtext("Spawning year", side=1, outer=TRUE, line=0.2, cex=1.5*mycex)
  	}



  # staff days, tfm, all lakes
  bigfig2 <- function() {
  	par(mfcol=c(3, 5), mar=c(3, 3, 0, 3), oma=c(2, 2, 2, 3), xaxs="i", yaxs="i",
      cex=1)
  	mycex <- par("cex")
  	for(i in 1:5) {
  		sel <- lake==sul[i]
  		year.tk <- axisra[["Year", i, 2]]
  		days.tk <- axisra[["Staff Days", i, 2]]
  		tfm.tk <- axisra[["TFM", i, 2]]
  		bayer.tk <- axisra[["Bayer", i, 2]]

  		if (sum(!is.na(staff.days[sel]))>0) {
  			plot(spawner.year[sel], staff.days[sel]/100, type="n", axes=FALSE,
          xlab="", ylab="",
  				xlim=range(year.tk), ylim=range(days.tk)/100)
  			symbols(spawner.year[sel], staff.days[sel]/200,
          rectangle=cbind(0.6, staff.days[sel]/100),
  				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.day])
  			if (i==5) mtext("Control field days\n(hundreds)", side=4, line=4,
          cex=1.5*mycex)
  			axis(1, at=year.tk)
  			axis(4, las=2, adj=1, at=days.tk/100)
  			box(bty="l")
  			} else {
          plot(1, 1, type="n", xlab="", ylab="")
  			}
  		mtext(suln[i], side=3, line=0.2, cex=1.5*mycex)
  		if (sum(!is.na(index[sel]))>0) {
  			par(new=TRUE)
  			plot(spawner.year[sel], index[sel]/1000, type="n", axes=FALSE, xlab="",
          ylab="",
  				xlim=range(year.tk), ylim=c(0, maxindex$ylimmax[i])/1000)
  			points(spawner.year[sel], index[sel]/1000, type="o", pch=18, lwd=2,
          cex=mycex, col=blindcolz[col.spa])
  			axis(2, las=2, adj=1)
  			}

  		if (sum(!is.na(tfmkgai[sel]))>0) {
  			plot(spawner.year[sel], tfmkgai[sel]/1000, type="n", axes=FALSE, xlab="",
          ylab="",
  				xlim=range(year.tk), ylim=range(tfm.tk)/1000)
  			symbols(spawner.year[sel], tfmkgai[sel]/2000,
          rectangle=cbind(0.6, tfmkgai[sel]/1000),
  				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.tfm])
  			if (i==5) mtext("TFM kg AI\n(thousands)", side=4, line=4, cex=1.5*mycex)
  			axis(1, at=year.tk)
  			axis(4, las=2, adj=1, at=tfm.tk/1000)
  			box(bty="l")
  			} else {
          plot(1, 1, type="n", xlab="", ylab="")
  			}
  		if (sum(!is.na(index[sel]))>0) {
  			par(new=TRUE)
  			plot(spawner.year[sel], index[sel]/1000, type="n", axes=FALSE, xlab="",
          ylab="",
  				xlim=range(year.tk), ylim=c(0, maxindex$ylimmax[i])/1000)
  			points(spawner.year[sel], index[sel]/1000, type="o", pch=18, lwd=2,
          cex=mycex, col=blindcolz[col.spa])
  			axis(2, las=2, adj=1)
  			}

  		if (sum(!is.na(bayerkgai[sel]))>0) {
  			plot(spawner.year[sel], bayerkgai[sel], type="n", axes=FALSE, xlab="",
          ylab="",
  				xlim=range(year.tk), ylim=range(bayer.tk))
  			symbols(spawner.year[sel], bayerkgai[sel]/2,
          rectangle=cbind(0.6, bayerkgai[sel]),
  				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.bay])
  			if (i==5) mtext("Bayer kg AI", side=4, line=4, cex=1.5*mycex)
  			axis(1, at=year.tk)
  			axis(4, las=2, adj=1, at=bayer.tk)
  			box(bty="l")
  			} else {
          plot(1, 1, type="n", xlab="", ylab="")
  			}
  		if (sum(!is.na(index[sel]))>0) {
  			par(new=TRUE)
  			plot(spawner.year[sel], index[sel]/1000, type="n", axes=FALSE, xlab="",
          ylab="",
  				xlim=range(year.tk), ylim=c(0, maxindex$ylimmax[i])/1000)
  			points(spawner.year[sel], index[sel]/1000, type="o", pch=18, lwd=2,
          cex=mycex, col=blindcolz[col.spa])
  			axis(2, las=2, adj=1)
  			}
  		}
  	mtext("Spawning year", side=1, outer=TRUE, line=0.2, cex=1.5*mycex)
  	mtext("Adult sea lamprey index  (thousands)",
      side=2, outer=TRUE, line=0.2, cex=1.5*mycex)
  	}





  #### plots of individual lakes separately ####

  # adults, wounding, trout, staff days, tfm
  for(i in 1:5) {
  	GLFCenv$figcount <- 1
  	sel <- lake==sul[i]
  	sel.yr <- sel & spawner.year==YEAR
  	year.tk <- axisra[["Year", i, 1]]
  	index.tk <- axisra[["Spawners", i, 1]]
  	pe.lab <- pretty(index.tk * index2pe[i])/1000
  	pe.tk <- pe.lab/index2pe[i]
  	trout.tk <- axisra[["Trout", i, 1]]
  	wound.tk <- axisra[["Wounds", i, 1]]
  	days.tk <- axisra[["Staff Days", i, 1]]
  	tfm.tk <- axisra[["TFM", i, 1]]
  	bayer.tk <- axisra[["Bayer", i, 1]]

  	addPageBreak(this=doc, width=8.5, height=11, omi=c(1, 1, 1, 1))
  	heading(paste("STATUS OF SEA LAMPREY CONTROL IN LAKE",
  	  casefold(Lakenames[i], upper=TRUE)))

  	heading("Adult Sea Lamprey:", 2)
  	if (sum(!is.na(index[sel]))>0) {
  		fig <- function() {
  			par(mar=c(4, 4, 0.5, 4), yaxs="i")
  			plot(spawner.year[sel], index[sel]/1000, type="n", axes=FALSE,
          xlim=range(year.tk), ylim=range(index.tk)/1000, xlab="Spawning year",
          ylab="Adult sea lamprey index  (thousands)")
        mtext("Population estimate  (thousands)", side=4, line=3)
        lines(spawner.year[sel], index.3mn[sel]/1000, col=blindcolz[col.spa])
  			abline(h=TARGET$index.target[i]/1000, col=blindcolz[col.tar])
  			arrows(spawner.year[sel], index.lo[sel]/1000, spawner.year[sel],
          index.hi[sel]/1000,
  				angle=90, length=0.02, code=3, col=blindcolz[col.spa])
  			points(spawner.year[sel], index[sel]/1000, pch=18,
          col=blindcolz[col.spa])
  			axis(1, at=year.tk)
  			axis(2, las=2, adj=1)
  			axis(4, las=2, adj=1, at=pe.tk, labels=pe.lab)
  			box(bty="l")
  			mtext(paste0("\u25B2\n \n", round(HIST.SPAWN[i]/1000), "\nHist."),
  			  side=3, line=-4, at=max(year.tk)-1, font=2, col=blindcolz[col.spa])
  			mtext("|",
  			  side=3, line=-1.5, at=max(year.tk)-1, font=2, col=blindcolz[col.spa])
  			}
      targphrase <- paste0("the mean of indices during a period with",
        " acceptable marking rates (")
      if(i==2) targphrase <- c("5/8.9 times the mean of indices (")
      if(i==3) targphrase <- c("0.25 times the mean of indices (")
  		figu("Index estimates with 95% confidence intervals (vertical bars) of adult sea lampreys, including historic pre-control abundance (as a population estimate) and the three-year moving average (line). The population estimate scale (right vertical axis) is based on the index-to-PE conversion factor of ", index2pe[i], ". The adult index in ", YEAR, " was ", format(signif(index[sel.yr], 2), big.mark=","), " with 95% confidence interval (", format(signif(index.lo[sel.yr], 2), big.mark=","), "-", format(signif(index.hi[sel.yr], 2), big.mark=","), "). The three-year (", YEAR-2, "-", YEAR, ") average of ", format(signif(allsmry$index.3mn[i], 2), big.mark=","), c(" met", " was above")[(allsmry$index.3mn[i] > allsmry$index.target[i]) + 1], " the target of ", format(signif(allsmry$index.target[i], 2), big.mark=","), ".", " The index target was estimated as ", targphrase, targyrz[i], ").",
  		  FIG=fig, h=2.64, w=3.96)
  		}


  	# bubble plots
  	fig <- function() {
  		mapp <<- mapL[[i]]
  		# adults, one year
  		df <- dfthis %>%
  		  filter(
  		    lake==i,
  		    index==TRUE
  		  ) %>%
  		  mutate(
  		    selbig = !is.na(PEmr)
  		  )
    	p1 <- ggplot(df, aes(long, lat)) +
    	  coord_quickmap() +
    	  geom_path(data=mapp, aes(x, y), color="lightgray", na.rm=TRUE) +
    	  geom_point(data=filter(df, is.na(indexContrib)), shape=4, na.rm=TRUE) +
    	  geom_point(aes(size=indexContrib, color=selbig), shape=1, stroke=2,
    	    na.rm=TRUE) +
    	  scale_size_continuous(range=c(1, 10)) +
    	  scale_color_manual(values=unname(blindcolz[c(2, 6)])) +
    	  geom_label_repel(aes(label=strname), label.size=NA, segment.color=NA,
    	    fill=alpha(c("white"), 0.7), label.padding=0.01, point.padding=0.1) +
    	  ggmap::theme_nothing()
  		# larvae, multiple years
  		df <- maxlarvae[maxlarvae$lake==i, ]
  		selbig <- df$row %in% top50maxlarvae$row[top50maxlarvae$lake==i]
    	p2 <- ggplot(df, aes(longitude, latitude)) +
    	  coord_quickmap() +
    	  geom_path(data=mapp, aes(x, y), color="lightgray", na.rm=TRUE) +
    	  geom_point(aes(size=estimate), shape=1, stroke=1+selbig,
    	    color=blindcolz[3], na.rm=TRUE) +
    	  scale_size_continuous(range=c(1, 10)) +
    	  geom_label_repel(data=df[selbig, ], aes(label=stream.nam),
    	    label.size=NA, fill=alpha(c("white"), 0.7), label.padding=0.01,
    	    segment.color=NA) +
    	  ggmap::theme_nothing()
    	egg::ggarrange(p1, p2, nrow=1)
  	}


  	figu("LEFT: Estimated index of adult sea lampreys during the spring spawning migration, ", YEAR, ". Circle size corresponds to estimated number of adults from mark-recapture studies (blue) and model predictions (orange). All index streams are labelled. RIGHT: Maximum estimated number of larval sea lampreys in each stream surveyed during 1995-2012. Tributaries composing over half of the estimated maximum lake-wide larval population are identified (", paste(top50maxlarvae$textpaste[top50maxlarvae$lake==i], collapse="; "),").",
  	  FIG=fig, h=ASPEX[i]*3.24, w=6.5)

  	para("* Sources of concern bullet points ...")
  	para(" ")

  	heading("Lake Trout Marking and Relative Abundance:", 2)
  	utarg <- TARGET$wound.units[i]
  	mark.time <- c("", "during August-November ", "", "", "")
  	if (sum(!is.na(rate[sel]))>0) {
  		fig <- function() {
  			par(mar=c(4.5, 5, 0.5, 1), xaxs="i", yaxs="i")
  			plot(spawner.year[sel], rate[sel], type="n", axes=FALSE,
          xlim=range(year.tk), ylim=range(wound.tk),
          xlab="Spawning year", ylab="Lake trout\nmarking rate")
  			lines(spawner.year[sel], rate.3mn[sel], col=blindcolz[col.wou])
  			arrows(spawner.year[sel], rate.lo[sel], spawner.year[sel], rate.hi[sel],
  				angle=90, length=0.02, code=3, col=blindcolz[col.wou])
  			abline(h=allsmry$wound.target[i], col=blindcolz[col.tar])
  			points(spawner.year[sel], rate[sel], pch=16, col=blindcolz[col.wou])
  			axis(1, at=year.tk)
  			axis(2, las=2, adj=1, at=wound.tk)
  			box(bty="l")

  			# add a second axis for lake trout survey year
  			tyticksel <- sel & trout.year %in% year.tk
  			tyticklab <- trout.year[tyticksel]
  			tytickat <- spawner.year[tyticksel]
  			suppressWarnings(par(mgp=c(3, -1.3, 0)))
  			axis(1, at=tytickat, labels=tyticklab, tck=0.015, cex.axis=0.8,
  			  col.axis=blindcolz[col.wou], col.ticks=blindcolz[col.wou])
  			par(mgp=c(3, 1, 0))
  			}
  		figu("Number of ", utarg, " marks per 100 lake trout > ", rep(c(532, 431), c(4, 1))[i], " mm from standardized assessments ", mark.time[i], " plotted against the sea lamprey spawning year, including the three-year moving average (line). The three-year (", YEAR-2, "-", YEAR, ") average marking rate of ", signif(allsmry$rate.3mn[i], 2), c(" met", " was above")[(allsmry$rate.3mn[i] > allsmry$wound.target[i]) + 1], " the target of ", allsmry$wound.target[i], " ", utarg, " marks per 100 lake trout > ", rep(c(532, 431), c(4, 1))[i], " mm (horizontal line). A second x-axis shows the year the lake trout were surveyed.",
  		  FIG=fig, h=2.64, w=3.96)
  		}

  # Figure Captions, in this order: Superior, Michigan, Huron, Erie, Ontario
  	trout.fig.captions <- c(
  		paste0('Lake trout relative abundance (May assessments using 4.5 inch',
      ' gillnets) plotted against sea lamprey spawning year, including the',
      ' three-year moving average (line). CPE',
      ' = fish/km/net night of lean lake trout > 532 mm (21") total length.'),
  		paste0("Lake trout relative abundance plotted against sea lamprey",
      " spawning year, including the three-year moving average (line). CPE = fish/1000'/net night",
      ' of lean lake trout > 532 mm (21") total length caught in the',
      ' Lake Wide Assessment Plan nets (the plan began in the late 1990s).'),
  		paste0('Lake trout relative abundance from standardized surveys (spring',
      ' 2-6 inch mesh) in U.S. waters of the main basin plotted against sea',
      ' lamprey spawning year, including the three-year moving average (line)',
      '. CPE = geometric mean of fish/km/net night',
      ' of lean lake trout > 532 mm (21") total length.'),
  		paste0("Lake trout relative abundance from standardized spring surveys",
      " plotted against sea lamprey spawning year, including the three-year",
      " moving average (line). CPE = number per",
      " lift of lean lake trout age 5 and older."),
  		paste('Lake trout relative abundance plotted against sea lamprey',
      ' spawning year, including the three-year moving average (line)',
      '. CPE = fish/km/net night of lean lake trout > 431 mm',
      ' (17") total length.'))

  	if (sum(!is.na(trout[sel]))>0) {
  		fig <- function() {
  			par(mar=c(4.5, 5, 0.5, 1), xaxs="i", yaxs="i")
  			plot(spawner.year[sel], trout[sel], type="n", axes=FALSE,
          xlim=range(year.tk), ylim=range(trout.tk),
          xlab="Spawning year", ylab="Lake trout\nabundance  (CPE)")
  			lines(spawner.year[sel], trout.3mn[sel], col=blindcolz[col.trt])
  			arrows(spawner.year[sel], trout.lo[sel], spawner.year[sel],
  			  trout.hi[sel],
  				angle=90, length=0.02, code=3, col=blindcolz[col.trt])
  			points(spawner.year[sel], trout[sel], pch=15, col=blindcolz[col.trt])
  			axis(1, at=year.tk)
  			axis(2, las=2, adj=1, at=trout.tk)
  			box(bty="l")
  			}
  		figu(trout.fig.captions[i], FIG=fig, h=2.64, w=3.96)
  		}

  	para("* Marking rate bullet points ...")
  	para(" ")

  	heading("Lampricide Control - Adults vs. Field Days, TFM, and Bayluscide:",
      2)
  	fig <- function() {
  		par(mar=c(3, 3, 0.5, 5), oma=c(2, 2, 0, 0), mfrow=c(3, 1), xaxs="i",
        yaxs="i", cex=1)
  		mycex <- par("cex")
  		if (sum(!is.na(staff.days[sel]))>0) {
  			plot(spawner.year[sel], staff.days[sel]/100, type="n", axes=FALSE,
          xlim=range(year.tk),
  				ylim=range(days.tk)/100, xlab="", ylab="")
  			symbols(spawner.year[sel], staff.days[sel]/200,
          rectangle=cbind(0.6, staff.days[sel]/100),
  				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.day])
  			mtext("Control field days\n(hundreds)", side=4, line=3.5, cex=1.2*mycex)
  			axis(1, at=year.tk)
  			axis(4, las=2, adj=1, at=days.tk/100)
  			box(bty="u")
  			} else {
          plot(1, 1, type="n", xlab="", ylab="")
  			}
  		if (sum(!is.na(index[sel]))>0) {
  			par(new=TRUE)
  			plot(spawner.year[sel], index[sel]/1000, type="n", axes=FALSE,
  				xlim=range(year.tk), ylim=range(index.tk)/1000, xlab="", ylab="")
  			lines(spawner.year[sel], index[sel]/1000, lwd=2, col=blindcolz[col.spa])
  			axis(2, las=2, adj=1)
  			}
  		if (sum(!is.na(tfmkgai[sel]))>0) {
  			plot(spawner.year[sel], tfmkgai[sel]/1000, type="n", axes=FALSE,
          xlim=range(year.tk),
  				ylim=range(tfm.tk)/1000, xlab="", ylab="")
  			symbols(spawner.year[sel], tfmkgai[sel]/2000,
          rectangle=cbind(0.6, tfmkgai[sel]/1000),
  				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.tfm])
  			mtext("TFM kg AI\n(thousands)", side=4, line=3.5, cex=1.2*mycex)
  			axis(1, at=year.tk)
  			axis(4, las=2, adj=1, at=tfm.tk/1000)
  			box(bty="u")
  			} else {
          plot(1, 1, type="n", xlab="", ylab="")
  			}
  		if (sum(!is.na(index[sel]))>0) {
  			par(new=TRUE)
  			plot(spawner.year[sel], index[sel]/1000, type="n", axes=FALSE,
  				xlim=range(year.tk), ylim=range(index.tk)/1000, xlab="", ylab="")
  			lines(spawner.year[sel], index[sel]/1000, lwd=2, col=blindcolz[col.spa])
  			axis(2, las=2, adj=1)
  			}
  		if (sum(!is.na(bayerkgai[sel]))>0) {
  			plot(spawner.year[sel], bayerkgai[sel], type="n", axes=FALSE,
          xlim=range(year.tk),
  				ylim=range(bayer.tk), xlab="", ylab="")
  			symbols(spawner.year[sel], bayerkgai[sel]/2,
          rectangle=cbind(0.6, bayerkgai[sel]),
  				add=TRUE, inches=FALSE, fg="black", bg=blindcolz[col.bay])
  			mtext("Bayer kg AI", side=4, line=3.5, cex=1.2*mycex)
  			axis(1, at=year.tk)
  			axis(4, las=2, adj=1, at=bayer.tk)
  			box(bty="u")
  			} else {
          plot(1, 1, type="n", xlab="", ylab="")
  			}
  		if (sum(!is.na(index[sel]))>0) {
  			par(new=TRUE)
  			plot(spawner.year[sel], index[sel]/1000, type="n", axes=FALSE,
  				xlim=range(year.tk), ylim=range(index.tk)/1000, xlab="", ylab="")
  			lines(spawner.year[sel], index[sel]/1000, lwd=2, col=blindcolz[col.spa])
  			axis(2, las=2, adj=1)
  			}
  		mtext("Spawning year", side=1, outer=TRUE, cex=1.2*mycex)
  		mtext("Adult sea lamprey index  (thousands)", side=2, outer=TRUE,
        cex=1.2*mycex)
  		}
  	figu("Index of adult sea lampreys (blue lines) and number of control field days (orange bars), TFM used (kg active ingredient; yellow bars), and Bayluscide used (kg active ingredient; purple bars). Field days, TFM, and Bayluscide are offset by 2 years (e.g., field days, TFM, and Bayluscide applied during 1985 is plotted on the 1987 spawning year, when the treatment effect would first be observed in adult sea lamprey populations).",
  	  FIG=fig, h=6, w=3.24)

  	para("* Treatment bullet points ...")
  	para(" ")

  	}






  GLFCenv$figcount <- 1


  figbig("Row 1: Index and 95% confidence interval of adult sea lampreys",
    " estimated as the sum of mark-recapture estimates in index streams.",
  	" The index target was estimated as the mean of indices during a period",
  	" with acceptable marking rates.",
  	" Row 2: A1-A3 marking rates on lake trout > 532 mm (for Lake Ontario, A1",
    " only on lake trout > 431 mm).",
  	" Target marking rate is indicated by the horizontal line.",
  	" Row 3: Lake trout abundance.",
  	" For lakes Superior and Huron, CPE is fish/km/net night of lean lake",
    " trout > 532 mm total length;",
  	" for Lake Michigan, CPE is fish/1000'/net night of lean lake trout > 532",
    " mm total length;",
  	" for Lake Erie, CPE is relative abundance of age 5 and older lake trout",
    " sampled in east basin gill nets;",
  	" and for Lake Ontario, CPE is fish/0.1476 km/net night of lean lake trout",
    " > 431 mm total length.",
  	" All rows: All metrics plotted against the sea lamprey spawning year.",
  	FIG=bigfig1, newpage="land")

  figbig("Row 1: Number of control field days (orange bars).",
  	" Row 2: TFM used (kg active ingredient, yellow bars).",
  	" Row 3: Bayluscide used (kg active ingredient, purple bars).",
  	" All rows: Index of adult sea lampreys is shown with blue lines.",
  	" All metrics plotted against the sea lamprey spawning year.",
  	" Control metrics are offset by 2 years, e.g., control applied during 2006",
    " is plotted on the 2008 spawning year -",
  	" the year the treatment effect would first be observed in the adult sea",
    " lamprey population.",
  	FIG=bigfig2, newpage="land")


  detach(ALL)


  # return options to original settings
  options(scipen=0, stringsAsFactors=FALSE)



  endrtf()

}
