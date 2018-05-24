#' Prepare the Deepwater Electrofishing Data
#'
#' Read in the deepwater electofishing data (including information on the
#' lamprey catch, the lamprey lengths, and the identification of plots that
#' were treated) and prepare them for estimation.
#' @param Dir
#'   A character scalar identifying the path where the data files are
#'   stored.  Use forward slashes, e.g., \code{Dir = "C:/temp/mydir"}.
#' @param CatchFile
#'   A character scalar identifying the name of the *.xl* file with
#'   catch data.  The file should have at least the following 19 columns,
#'   named in the header row:
#'   SAMPID, LATITUDE, LONGITUDE, STIME, BOAT, SAMPLE, DEPTH, SUB_MAJOR,
#'   SUB_MINOR1, SUB_MINOR2, GPSDATE, HAB_TYPE, SL_TOTAL, AB_TOTAL, I_TOTAL,
#'   COMMENT, NEW_NUMB, INBPLOT, REGION
#'   (these last 3 columns are added in using ArcInfo).
#'   See details.
#' @param LengthsFile
#'   A character vector identifying the names of the *.xl* files with
#'   the lengths data.  The files should have at least the following 2 columns,
#'   named in the header row: SAMPID, LENGTH.
#'   See details.
#' @param PlotsFile
#'   A character vector identifying the name of the *.xl* file with
#'   the treatment plots data.  The files should have at least the following 3
#'   columns, named in the header row:
#'   AREA, Plot_09, Treat_YYYY, where YYYY is the current year.
#'   Treat_YYYY is equal to 1 if the plot was treated that year, equal to 0
#'   otherwise.  If a plot was treated twice in one year, it will be listed
#'   on two separate rows, each with Treat_YYYY=1.
#'   See details.
#' @param TRTtiming
#'   A character scalar identifying the timing of the assessment survey relative
#'   to treatment.  "AFTER" if all plots were surveyed AFTER they were treated
#'   (the default), "BEFORE" if all plots were survey BEFORE they were treated,
#'   "NONE" if no plots were treated, and "MIXED" if some plots were surveyed
#'   before and some plots were surveyed after treatment.
#' @param b4plots
#'   A numeric vector identifying the plots that were surveyed BEFORE they
#'   were treated.  A value for this should only be provided if
#'   \code{TRTtiming} is set to "MIXED" (default NULL).
#' @details
#'   The order of the columns and the case of the column names in the
#'   \code{CatchFile}, \code{LengthsFile}, \code{PlotsFile} files are
#'   unimportant.
#'   Additional columns may be also be part of theses files,
#'   but they will be ignored.
#' @return
#'   A list with DWEF catch, length, and plot data in three data frames (CAT,
#'   LEN, PLT) and a character vector of the SOURCE directory and file names.
#'   The plot data is reorganized to have only one row per plot, with the
#'   trtd variable indicating the number of treatments each plot received
#'   that year.
#' @import readxl
#' @export

DWEFprep <- function(Dir, CatchFile, LengthsFile, PlotsFile, TRTtiming="AFTER",
  b4plots=NULL) {

  # treatment timing
  tt <- casefold(substring(TRTtiming, 1, 1))
  if(tt=="m" & is.null(b4plots)) stop('If some plots were surveyed',
    ' before and some plots were surveyed after treatment,',
    ' TRTtiming="MIXED", then b4plots should identify the plots that were',
    ' surveyed BEFORE they were treated, e.g., b4plots=c(18, 39, 112).')
  if(tt!="m" & !is.null(b4plots)) stop('No plot numbers should be provided',
    ' for b4plots unless TRTtiming="MIXED".')
  PERIOD <- recode(tt, c("n", "a", "b", "m"), c(0, 1, -1, NA))

  # bring in lengths data
  lens <- vector("list", length(LengthsFile))
  for(i in seq(length(lens))) {
    filei <- LengthsFile[i]
    if(length(grep("\\.xl", filei, ignore.case=TRUE))==1) {
      lens[[i]] <- read_excel(paste(Dir, filei, sep="/"), sheet=1)
      } else {
      stop("LengthsFile file must be file type *.XLS*")
      }
    }
  lens <- do.call(rbind, lens)
  names(lens) <- casefold(names(lens))
  lens <- lens[!is.na(lens$length), ]
  lens$sampid <- as.numeric(lens$sampid)
  lens$id <- as.numeric(lens$id)
  lens$length <- as.numeric(lens$length)
  lens$sl.larv.n <- ifelse(lens$class=="A", 1, 0)
  lens$sl.meta.n <- ifelse(lens$class=="T", 1, 0)
  # calculate catch adjusted for length-based gear efficiency
  lens$sl.larv.adj = ifelse(lens$class=="A", DWEFgec(lens$length), 0)
  lens$sl.meta.adj = ifelse(lens$class=="T", DWEFgec(lens$length), 0)
  larvtots <- aggregate(cbind(sl.larv.n, sl.larv.adj, sl.meta.n, sl.meta.adj) ~
      sampid, data=lens, sum)
  lvarnamz <- c("sampid", "class", "length", "sl.larv.n", "sl.larv.adj",
    "sl.meta.n", "sl.meta.adj")

  # bring in catch data
  if(length(grep("\\.xl", filei, ignore.case=TRUE))==1) {
    fin <- read_excel(paste(Dir, CatchFile, sep="/"), sheet=1)
    } else {
    stop("CatchFile file must be file type *.XLS*")
    }
  names(fin) <- make.names(casefold(names(fin)), unique=TRUE, allow_=FALSE)
  gps <- strsplit(fin$gpsdate, "/")

  # sometimes date is entered as month first, other times day first,
  # should be consistent within boat
  d1 <- as.numeric(lapply(gps, "[", 1))
  d2 <- as.numeric(lapply(gps, "[", 2))
  # when a date field is greater than 12,
  # that is an indication that the field is NOT the MONTH (nm)
  nm1 <- d1>12
  nm2 <- d2>12
  boat <- fin$boat
  bd <- aggregate(cbind(nm1, nm2) ~ boat, FUN=mean, na.rm=TRUE)
  fin <- merge(fin, bd, all.x=TRUE)
  fin$dd <- ifelse(fin$nm1 > fin$nm2, d1, d2)
  fin$mm <- ifelse(fin$nm1 > fin$nm2, d2, d1)

  fin$year <- as.numeric(lapply(gps, "[", 3))
  miss <- rep(NA, dim(fin)[1])
  fin$site <- fin$id
  fin$transamp <- miss
  fin$transect <- miss
  fin$label <- miss
  fin$cluster <- miss
  fin$plot.num <- miss
  fin$comment[is.na(fin$comment)] <- "No comment"
  fin$commentwrap <- sapply(strwrap(fin$comment, width=30, simplify=FALSE),
    paste, collapse="\n")

  # if inbplot column is called inplot, change it
  if("inplot" %in% names(fin)) fin$inbplot <- fin$inplot

  fin2 <- merge(fin, larvtots, all=TRUE)
  rm(gps, d1, d2, nm1, nm2, boat, bd, miss, larvtots)

  # determine year of interest
  if(var(fin2$year, na.rm=TRUE)>0) {
    stop(paste("CatchFile data contains more than one year of data:",
      sort(unique(fin2$year))))
    }

  # bring in plot information
  if(length(grep("\\.xl", filei, ignore.case=TRUE))==1) {
    plotinfo <- read_excel(paste(Dir, PlotsFile, sep="/"), sheet=1)
    } else {
    stop("Plot information file must be file type *.XLS*")
    }
  orig.names <- names(plotinfo)
  names(plotinfo) <- make.names(casefold(names(plotinfo)), unique=TRUE,
    allow_=FALSE)
  plotinfo$new.numb <- plotinfo$plot.09
  treatvarname <- names(plotinfo)[grep("treat", names(plotinfo))]
  plotinfo$trtd <- plotinfo[[treatvarname]]
  plotinfo$area.ha <- plotinfo$area
  area.treated <- sum(plotinfo$area.ha[plotinfo$trtd==1])
  plotinfo2 <- aggregate(trtd ~ area.ha + new.numb, data=plotinfo, FUN=sum)

  varnamz <- c("year", "mm", "dd", "stime", "period", "sampid", "transamp",
    "transect", "site", "boat", "latitude", "longitude", "region", "label",
    "inbplot", "plot.num", "new.numb", "sample", "cluster", "depth", "hab.type",
    "sub.major", "sub.minor1", "sub.minor2", "ab.total", "i.total", "sl.total",
    "comment", "commentwrap")
  rm(orig.names, treatvarname, filei)

  # if plot numbers were entered, make sure they make sense
  if(is.na(PERIOD)) {
    check <- match(b4plots, fin2$new.numb)
    if(sum(is.na(check))>0) stop(paste(
      "No catch data corresponds to the hotspots numbers you provided:",
      b4plots[is.na(check)]))
    fin2$period <- ifelse(fin2$new.numb %in% b4plots, -1, 1)
    } else {
    fin2$period <- PERIOD
    }


  # any variable names missing?
  missvar.c <- varnamz[is.na(match(varnamz, names(fin2)))]
  if(length(missvar.c)>0) stop(paste(
    "CatchFile is missing required variable(s):",
    paste(missvar.c, collapse=", ")))
  missvar.l <- lvarnamz[is.na(match(lvarnamz, names(lens)))]
  if(length(missvar.l)>0) stop(paste(
    "LengthsFile is missing required variable(s):",
    paste(missvar.l, collapse=", ")))

  # create data frames with just the specified variable names
  smr <- fin2[, varnamz]
  lens <- lens[, lvarnamz]
  rm(fin, fin2, PERIOD, missvar.c, missvar.l, b4plots, varnamz, lvarnamz)


  # prepare catch data
  list.time <- strsplit(smr$stime, ":")
  smr$hr <- as.numeric(sapply(list.time, "[", 1))
  smr$mn <- as.numeric(sapply(list.time, "[", 2))
  smr$dec.time <- smr$hr + smr$mn/60
  smr$stime <- 100*smr$hr + smr$mn
  smr$date <- rep(NA, dim(smr)[1])
  sel <- !is.na(smr$year)
  smr$date[sel] <- as.Date(paste(smr$year[sel], smr$mm[sel], smr$dd[sel],
    sep="-"))
  class(smr$date) <- "Date"
  row.names(smr) <- 1+(1:dim(smr)[1])
  smr <- smr[order(smr$boat, smr$date, smr$dec.time), ]
  rm(list.time, sel, i)

  list(CAT=smr, LEN=lens, PLT=plotinfo2,
    SOURCE=c(Dir=Dir, CatchFile=CatchFile,
      LengthsFile=paste(LengthsFile, collapse=", "), PlotsFile=PlotsFile))
}
