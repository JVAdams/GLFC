#' Generate Estimates from the Deepwater Electrofishing Data
#'
#' Generate Estimates of larval sea lamprey abundance from
#' the deepwater electofishing data.
#' @param Dir
#'   A character scalar identifying the path where output files will be
#'   stored.  Use forward slashes, e.g., \code{Dir = "C:/temp/mydir"}.
#' @param CatchClean
#'   A data frame with the cleaned catch data, typically the \code{CAT2}
#'   output from \code{\link{DWEFerror}}.
#' @param LengthsClean
#'   A data frame with the cleaned lengths data, including information from
#'   larvae only (no metamorphosing juveniles), typically the \code{LEN2}
#'   output from \code{\link{DWEFerror}}.
#' @param Plots
#'   A vector data frame with the plot data, typically the \code{PLT} output
#'   from \code{\link{DWEFerror}}.
#' @param Downstream
#'   Logical scalar indicating whether the downstream portion of the St. Marys
#'   River was surveyed (TRUE) or if just the upstream portion of the river was
#'   surveyed (FALSE).
#' @param Errors
#'   A character vector of table numbers corresponding to the document
#'   produced by \code{\link{DWEFerror}}, indicating unresolved errors
#'   remaining the DWEF data, typically the \code{ERR} output from
#'   \code{\link{DWEFerror}}.
#' @param Outfiles
#'   A character vector of length three with names for the catch, lengths,
#'   and plot output csv files.
#' @param StratArea
#'   Data frame with three variables: \code{inbplot} indicating whether the
#'   stratum is in (=1) a high larval density area or not (=0), \code{region}
#'   indicating the general location in the river (1 = North Channel,
#'   2 = turning basin, 3 = widening part, 4 = Neebish channels, and
#'   5 = most upstream part), and \code{haStrat} area of the stratum in
#'   hectares.  Strata of the St. Marys River larval sea lamprey survey are
#'   defined by \code{region} and \code{inbplot}.  By default the 2013 areas
#'   are provided, \code{\link{SMRStratArea}}.
#' @param bkill
#'   Numeric scalar indicating the assumed effectiveness of Bayluscide in
#'   treated plots, expressed as the proportion of larval sea lampreys killed,
#'   default 0.75.
#' @details
#'   It is assumed that this function will be run immediately after the
#'   \code{\link{DWEFerror}} function, in which case the rtf file created by
#'   \code{\link{DWEFerror}} will be continued and completed by
#'   \code{DWEFreport}.
#' @return
#'   Three csv files are written to \code{Dir}, with the final catch, lengths,
#'   and plot data.
#' @importFrom maps map
#' @importFrom plotrix rescale
#' @importFrom lubridate year mday month
#' @import survey
#' @export

DWEFreport <- function(Dir, CatchClean, LengthsClean, Plots, Downstream,
  Errors, Outfiles, StratArea=SMRStratArea, bkill=0.75) {

# Dir <- mydat$SOURCE["Dir"]
# CatchClean <- myclean$CAT2
# LengthsClean <- myclean$LEN2
# Plots <- mydat$PLT
# Downstream <- downstream.survey=="YES"
# Errors <- myclean$ERR
# Outfiles <- myclean$OUT
# StratArea <- data.frame(
#   inbplot=c(0, 0, 0, 0, 0, 1, 1, 1, 1),
#   region = c(1, 2, 3, 4, 5, 1, 2, 3, 5),
#   haStrat = c(846.86, 1000.57, 3366.97, 1807.39, 203.78, 140.99, 475.95,
#     248.08, 56.65))
# bkill <- 0.75




  strat.name <- function(df) {
    df$stratum <- paste(df$region, df$inbplot, sep="-")
    df
  }

  nonmissing <- !is.na(CatchClean$year) & !is.na(CatchClean$hab.type) &
    is.element(CatchClean$hab.type, 1:3) & !is.na(CatchClean$sl.total) &
    CatchClean$sl.total > -1
  printvars <- c("sampid", "date", "region", "new.numb", "depth", "hab.type",
    "sl.total", "commentwrap")

  if(length(unique(CatchClean$year))!=1) stop("There should be just one year",
    " of data in the Catch file.")
  YEAR <- CatchClean$year[1]

  if(length(unique(CatchClean$period))!=1) stop("There should be just one",
    " value for the variable period in the Catch file.")
  WHEN <- recode(CatchClean$period[1], c(0, 1, -1, NA), c("No treatment",
    "Post-treatment", "Pre-treatment", "Pre- and post-treatment"))

  TODAY <- format(Sys.time(), "%d-%b-%Y")



  # estimation

  # whole river crunching

  heading(paste0("TITLE:  Draft Results of St. Marys River Larval Assessment ",
    YEAR), 2)
  para("AUTHORS:  (Your name here), Adams, and (any others you think should be",
    " included)")
  para("DATE:  ", TODAY)

  # reset table and figure counts
  GLFCenv$tabcount <- 1
  GLFCenv$figcount <- 1

  # report if there are any errors left
  if(!is.null(Errors)) {
    heading(paste0("These results are PRELIMINARY.  Unresolved errors remain",
    " as indicated in ", paste(Errors, collapse=", "),
      " on the previous pages."), 2)
    }

  # did the survey extend south of 46 deg 26 min N latitude?
  latcut <- 46 + 26/60
  latcuttxt <- "46o26'N"
  nupper <- sum(CatchClean$latitude >= latcut)
  nlower <- sum(CatchClean$latitude < latcut)

  CatchClean2 <- merge(CatchClean, Plots[, c("new.numb", "trtd")], all.x=TRUE)
  CatchClean2$trtd[is.na(CatchClean2$trtd)] <- 0

  if(Downstream) {
    uponly <- FALSE
    muf <- 1
    sigf <- 1
    para("The larval sea lamprey survey in ", YEAR,
      " included both the upper river (", nupper,
      " samples) and the lower river (", nlower,
      " samples) portion of the St. Marys River, demarcated by latitude ",
      latcuttxt, ".")
    # create another version of the catch file, with only the upper river
    # this will be used to calculate the upper river muhat and sigmahat, to
    # update the expansion factors used in future years
    smr2up <- CatchClean2[CatchClean2$latitude >= latcut, ]
    if(nlower>100) {
      cat("Great.  That makes estimation easier.\n\n")
    } else {
      cat(paste0("Hmmm.  That's weird.  There were fewer than usual (", nlower,
        ") samples taken south of latitude ", latcuttxt, ".\n\n"))
    }
  } else {
    uponly <- TRUE
    # these expansion factors are based on means from source.years
    source.years <- "2002, 2003, 2008, 2012, and 2017"
    muf <- 0.570202977
    sigf <- 0.517269331
    last.year <- rev(strsplit(source.years, " ")[[1]])[1]
    para("The larval sea lamprey survey in ", YEAR,
      " included only the upper river (", nupper,
      " samples) portion of the St. Marys River, demarcated by the latitude ",
      latcuttxt,
      " (there were ", nlower, " samples in the lower river).",
      "  So, expansion factors (", signif(muf, 3), " for the mean and ",
      signif(sigf, 3),
      " for the standard deviation, based on whole river surveys conducted in ",
      source.years, ") were applied to the ", YEAR,
      " survey data to get the whole river estimate of abundance.")
    agef <- YEAR - as.numeric(last.year)
    if(agef > 4) {
      para("These expansion factors should probably be updated, since they",
        " are ", agef, " years old, and both the upper and lower portions of",
        " the St. Marys River should be sampled every four years.")
    }
    if(nlower<1) {
      cat("Okay.  We will have to apply an expansion factor to the survey data",
        " to get a whole river estimate of abundance.\n\n")
    } else {
      cat(paste0("Hmmm.  That's weird, because there were ", nlower,
        " samples taken south of latitude ", latcuttxt,
        ", indicating some sampling in the lower river.\n\n"))
    }
    rm(source.years, last.year, agef)
  }
  rm(nupper, nlower)

  treated.plots <- with(Plots, new.numb[trtd>0])

  # if lower river plots were treated, but only the upper river was surveyed,
  # we can only estimate abundance for the time period sampled
  if(sum(CatchClean2$latitude[CatchClean2$new.numb %in% treated.plots] <
      latcut) > 0 & uponly) {
    para("Because some lower river hotspots were treated, but only the upper",
      " river was surveyed,",
      " we can only estimate abundance for the time period sampled.")
    }


  areas2 <- StratArea
  areas2 <- strat.name(areas2)
  areas3 <- aggregate(haStrat ~ stratum, data=areas2, sum)
  # Ai (area of single sample, 2.44e-4 ha by default)
  Ai <- 2.44e-4
  rm(StratArea, areas2)


  # bring in catch information
  smr3 <- CatchClean2
  smr3$new.numb[smr3$new.numb==0] <- NA
  smr3 <- strat.name(smr3)

  # calculate sampling intensities, hectares per sample

  # in plots
  sel <- smr3$inbplot==1
  nsamples.in.plots <- sum(sel)
  surveyed.plots <- sort(unique(smr3$new.numb[sel]))
  surveyed.plots.area <-
    sum(Plots$area.ha[match(surveyed.plots, Plots$new.numb)])
  int.in <- surveyed.plots.area/nsamples.in.plots

  # out of plots - upper
  sel <- smr3$inbplot==0 & smr3$latitude >= latcut
  nsamples.out.upper <- sum(sel)
  surveyed.strats <- sort(unique(smr3$stratum[sel]))
  surveyed.strats.area <-
    sum(areas3$haStrat[match(surveyed.strats, areas3$stratum)])
  int.out.up <- surveyed.strats.area/nsamples.out.upper

  # out of plots - lower
  sel <- smr3$inbplot==0 & smr3$latitude < latcut
  if(sum(sel)>0) {
    nsamples.out.lower <- sum(sel)
    surveyed.strats <- sort(unique(smr3$stratum[sel]))
    surveyed.strats.area <-
      sum(areas3$haStrat[match(surveyed.strats, areas3$stratum)])
    int.out.lo <- surveyed.strats.area/nsamples.out.lower
    phrase <- paste0("one per ", signif(int.out.lo, 2), " ha")
    rm(nsamples.out.lower, int.out.lo)
    } else {
    phrase <- "none"
    }

  surveyed.strats <- sort(unique(smr3$stratum))
  matchstrat <- match(surveyed.strats, areas3$stratum)
  if(any(is.na(matchstrat))) stop(paste0("Some surveyed strata (",
    paste0(surveyed.strats[is.na(matchstrat)], collapse=", "),
    ") do not match any of\nthe strata we have areas for (",
    paste0(areas3$stratum, collapse=", "), ").\n\n"))
  Ah <- areas3$haStrat[matchstrat]
  A.expanded <- sum(Ah)

  # for pre-trt estimates, adjust the catch from post-trt surveys in the
  # treated plots by 1/(1-bkill)
  df.pre.est <- smr3
  sel <- df.pre.est$period==1
  df.pre.est$sl.larv.adj[sel] <- with(df.pre.est[sel, ],
    sl.larv.adj * ((1/(1-bkill))^trtd) )

  # for post-trt estimates, adjust the catch from pre-trt surveys in the
  # treated plots by (1-bkill)
  df.post.est <- smr3
  sel <- df.post.est$period==-1
  df.post.est$sl.larv.adj[sel] <- with(df.post.est[sel, ],
    sl.larv.adj * ((1-bkill)^trtd) )

  # estimation
  doit <- function(df) {

    # larval density
    df$slden <- df$sl.larv.adj/Ai

    # calculate weights (inverse probabilities) for each sample
    strcount <- table(df$stratum)
    strcount2 <- data.frame(stratum=names(strcount), n=as.numeric(strcount),
      stringsAsFactors=FALSE)
    areas3$n <- strcount2$n[match(areas3$stratum, strcount2$stratum)]
    areas3$n[is.na(areas3$n)] <- 0
    areas3$prob <- with(areas3, n/haStrat)
    areas3$weight <- 1/areas3$prob
    df$w <- areas3$weight[match(df$stratum, areas3$stratum)]
    # note that sum of weights should be the area of the river
    # sum(df$w)
    # A.expanded

    # old way
    # coch <- stratCochran(yhi=df$sl.larv.adj/Ai, hi=df$stratum,
    #   Wh=Ah/A.expanded, N=A.expanded)
    # muhat.f <- muf*coch["ybarst"]
    # sigmahat.f <- sigf*coch["seybarst"]

    # new way
    dw <- svydesign(ids=~1, strata=~stratum, weights=~w, data=df)
    coch <- svymean(~slden, design=dw)
    muhat.f <- muf*coef(coch)
    sigmahat.f <- sigf*as.vector(SE(coch))
    out <- list(muhat.f=muhat.f, sigmahat.f=sigmahat.f,
      PE=muhat.f*A.expanded, PE.sd=sigmahat.f*A.expanded,
      PE.cv=sigmahat.f/muhat.f,
      PE.ci=A.expanded*(muhat.f + c(-1, 1) * 1.96*sigmahat.f))
    lapply(out, as.numeric)
    }
  pre.ests <- doit(df.pre.est)
  post.ests <- doit(df.post.est)

  cat("\n\n\n*** Send these estimates to Jean.\n")
  cat("*** She will use them to update the SMR Assessment Plan metrics.\n")

  cat("\n    Pre-treatment whole-river estimate\n\n")
  print(lapply(pre.ests[-(1:2)], format, big.mark=","))

  cat("\n    Post-treatment whole-river estimate\n\n")
  print(lapply(post.ests[-(1:2)], format, big.mark=","))


  # repeat estimation for upper river only, if the whole river was sampled
  if(!uponly) {
    smr3up <- smr2up
    smr3up <- strat.name(smr3up)
    surveyed.strats <- sort(unique(smr3up$stratum))
    matchstrat <- match(surveyed.strats, areas3$stratum)
    Ah <- areas3$haStrat[matchstrat]
    A.expanded <- sum(Ah)
    # for post-trt estimates, adjust the catch from pre-trt surveys in the
    # treated plots by (1-bkill)
    dpe <- smr3up
    sel <- dpe$new.numb %in% treated.plots & dpe$period==-1
    dpe$sl.larv.adj[sel] <- with(dpe[sel, ],
      sl.larv.adj * ((1-bkill)^trtd) )
    peup <- doit(dpe)
    cat("\n\n\n*** Send these numbers to Jean, too.\n")
    cat("*** She will use them to update the expansion factors for",
      "future surveys.\n")
    cat("\n  * Whole river *")
    cat(paste0("\n     Post-trt muhat = ", post.ests$muhat.f, ", sigmahat = ",
      post.ests$sigmahat.f))
    cat("\n  * Upper river *")
    cat(paste0("\n     Post-trt muhat = ", peup$muhat.f, ", sigmahat = ",
      peup$sigmahat.f, "\n\n"))
    rm(smr2up, smr3up, dpe, peup)
    }

  rm(CatchClean, areas3, nsamples.in.plots, surveyed.plots, surveyed.plots.area,
    nsamples.out.upper, surveyed.strats, surveyed.strats.area,
    matchstrat, df.pre.est, sel, doit, Ah, A.expanded, muf, sigf)


  # plot-specific crunching
  # post-treatment adjusted catch IN plots
  post.in <- df.post.est[
    smr3$inbplot==1 & !is.na(smr3$new.numb) & smr3$new.numb>0, ]
  # estimated larval density with SD
  post.in.dens <- data.frame(
    new.numb = sort(unique(post.in$new.numb)),
    meanlat = tapply(post.in$latitude, post.in$new.numb, mean),
    meanlong = tapply(post.in$longitude, post.in$new.numb, mean),
    n.samp = as.numeric(table(post.in$new.numb)),
    catch = tapply(post.in$sl.larv.n, post.in$new.numb, sum),
    meannperha = tapply(post.in$sl.larv.adj, post.in$new.numb, mean)/2.44e-4,
    sd.dens = sqrt(tapply(post.in$sl.larv.adj, post.in$new.numb, var))/2.44e-4
    )
  # bring in plot area and no. of times treated
  pid2 <- merge(post.in.dens, Plots, all.x=TRUE)

  # end of growing season, November 23 of every year
  EOGS <- as.Date(paste(YEAR, "11", "23", sep="-"))
  # average daily growth in mm
  ADG <- 0.135
  # upper lakes transformer curve coefficients
  b0 <- -19.22319
  b1 <- 0.1343101

  # associate dates with lengths (class "A" LARVAL lengths only,
  # no class "T" transformer lengths)
  lens2 <- merge(post.in[, c("sampid", "date", "new.numb")], LengthsClean,
    all=FALSE)
  lens2$finalen <- lens2$length + as.numeric(EOGS - lens2$date)*ADG
  lens2$ntran <- lens2$sl.larv.adj * (1 / (1 + exp(-(b0 + b1*lens2$finalen))))
  lens2$nbig <- lens2$sl.larv.adj * as.numeric(lens2$finalen>100)

  # calculate probabilities
  probs <- data.frame(
    new.numb = sort(unique(lens2$new.numb)),
    ptran = tapply(lens2$ntran, lens2$new.numb, sum)/
      tapply(lens2$sl.larv.adj, lens2$new.numb, sum),
    pbig = tapply(lens2$nbig, lens2$new.numb, sum)/
      tapply(lens2$sl.larv.adj, lens2$new.numb, sum)
    )

  # combine densities and probabilities
  pid3 <- merge(pid2, probs, all=TRUE)
  pid3$year <- rep(YEAR, dim(pid3)[1])
  pid3$period <- rep(1, dim(pid3)[1])
  pid3$larvpe <- pid3$meannperha*pid3$area.ha
  pid3$tranpe <- pid3$ptran*pid3$larvpe
  pid3$tranpe[is.na(pid3$tranpe)] <- 0
  pid3$bigpe <- pid3$pbig*pid3$larvpe
  pid3$bigpe[is.na(pid3$bigpe)] <- 0
  pid3$bigdens <- pid3$bigpe/pid3$area.ha
  pid3 <- pid3[order(
    -pid3$bigdens, -pid3$meannperha, pid3$n.samp, pid3$n.samp/pid3$area.ha), ]


  # text
  para(WHEN, " sampling for larval sea lampreys was conducted from ",
    mday(min(smr3$date, na.rm=TRUE)),
    " ", month(min(smr3$date, na.rm=TRUE), label=TRUE, abbr=FALSE),
    " to ", mday(max(smr3$date, na.rm=TRUE)), " ",
    month(max(smr3$date, na.rm=TRUE), label=TRUE, abbr=FALSE),
    " ", YEAR, " in the St. Marys River.",
    "  Stratified, systematic sampling was used, with strata defined according",
    " to region and whether the sample was",
    " inside or outside of a historical hotspot.",
    "  Samples were regularly spaced (systematically sampled) at intensities",
    " of about one sample per ", signif(int.in, 2),
    " ha inside the historical hotspots,",  " one per ", signif(int.out.up, 2),
    " ha outside the hotspots in the upper river,",
    " and ", phrase, if(!uponly) " outside the hotspots",
    " in the lower river (Figure ", GLFCenv$figcount, ").",
    "  Systematic sampling was introduced in 2004 to ensure good spatial",
    " coverage of the river in general,",
    " and of the hotspots in particular.",
    "  Adaptive sampling was discontinued in 2009 to make logistics more",
    " straightforward (planning, field collection, data processing,",
    " and estimation).")

  bkilltalk <- if(WHEN=="Post-treatment") "" else
    paste0("  For hotspots that were surveyed prior to treatment,",
    " post-treatment estimates  were based on a combination of the survey",
    " results and an assumed Bayluscide effectiveness of ",
      round(100*bkill), "%.")
  para("A total of ", sum(smr3$sl.larv.n), " larval and ", sum(smr3$sl.meta.n),
    " metamorphosing sea lampreys were collected from ",
    dim(smr3)[1], " samples (Table ", GLFCenv$tabcount, ").",
    "  The ", YEAR,
    " post-treatment population estimate for the whole river was ",
    format(signif(post.ests$PE, 3), big.mark=","),
    " larval sea lampreys, with a CV of ", round(100*post.ests$PE.cv),
    "% (Table ", GLFCenv$tabcount+1, ").",
    "  The length distribution of the ", YEAR,
    " post-treatment larval population is shown in Figure ",
    GLFCenv$figcount+1, ".", bkilltalk)

  para("Information from the survey was also used to generate hotspot-specific",
    " estimates of large larvae for use in",
    " ranking the St. Marys River hotspots with other chemical options.",
    "  Larvae were defined as large if they were greater than 100 mm at the",
    " end of the growing season, ",
    mday(EOGS), " ", month(EOGS, label=TRUE, abbr=FALSE), " ", year(EOGS),
    ", using an average growth rate of ", ADG, " mm/day.",
    "  A total of ", dim(pid3)[1],
    " historical hotspots were sampled during the ", YEAR, " survey, ",
    sum(pid3$larvpe>0),
    " of these had at least one positive catch, and ", sum(pid3$bigpe>0),
    " had at least one large larva (Table ", GLFCenv$tabcount+2,
    ", Figure ", GLFCenv$figcount+2, ").")


  rm(int.in, int.out.up, phrase)



  # tables

  # summary of larval survey overall
  regs <- c(as.character(1:5), "")
  region.names <- c("North Channel", "Turning Basin", "Lake Nicolet",
    "Neebish Channels", "Locks", "Total")
  nsamples <- table(smr3$region, smr3$inbplot, useNA="ifany")
  nsamples <- rbind(nsamples, apply(nsamples, 2, sum, na.rm=TRUE))
  catch <- tapply(smr3$sl.larv.n, list(smr3$region, smr3$inbplot), sum,
    na.rm=TRUE)
  catch <- rbind(catch, apply(catch, 2, sum, na.rm=TRUE))
  dens <- tapply(smr3$sl.larv.adj/Ai/10000, list(smr3$region, smr3$inbplot),
    mean, na.rm=TRUE)
  dens <- rbind(dens, tapply(smr3$sl.larv.adj/Ai/10000, smr3$inbplot, mean,
    na.rm=TRUE))
  regions <- dimnames(nsamples)[[1]]
  tab <- data.frame(regions, region.names[match(regions, regs)], nsamples[, 1],
    catch[, 1], dens[, 1], nsamples[, 2], catch[, 2], dens[, 2])
  names(tab) <- c(" ", "Region", "Out.N", "Out.Catch", "Out.Dens", "In.N",
    "In.Catch", "In.Dens")
  tab$Out.Dens <- format(signif(tab$Out.Dens, 2))
  tab$In.Dens <- format(signif(tab$In.Dens, 2))
  tab$In.Dens[grep("NA", tab$In.Dens)] <- "-"
  # reorder the regions, from upper to lower river
  ord <- match(c(5, 1:4, ""), tab[, 1])
  tab <- tab[ord[!is.na(ord)], ]
  tabl("Sampling effort (N) and catch of larval sea lampreys in the St. Marys",
    " River, ", YEAR,
    ", summarized for each region and hotspot (in or out) stratum.",
    "  Each sample represents 4 deepwater electrofisher drops covering 0.61 m2",
    " each (2.44 m2 total).",
    "  Mean density (Dens.) is reported in no./m2 (adjusted for length-based",
    " gear efficiency).", TAB=tab, row.names=FALSE)
  rm(regs, region.names, nsamples, catch, dens, regions, Ai, ord)

  # whole river estimates
  area.treated <- with(Plots, sum(area.ha*trtd))

  tab <- data.frame(matrix(NA, ncol=8, nrow=3, dimnames=list(NULL,
    c("Year", "Period", "Trt.ha", "PE", "SD", "CV", "CI.lo", "CI.up"))))
  tab$Year <- rep(YEAR, 3)
  tab$Period <- c("Pre-", "Trt", "Post-")
  tab$Trt.ha <- c(" ", format(round(area.treated), big.mark=","), " ")
  tab$PE <- c(format(signif(pre.ests$PE/1000000, 2)), " ",
    format(signif(post.ests$PE/1000000, 2)))
  tab$SD <- c(format(signif(pre.ests$PE.sd/1000000, 2)), " ",
    format(signif(post.ests$PE.sd/1000000, 2)))
  tab$CV <- c(format(signif(pre.ests$PE.cv, 2)), " ",
    format(signif(post.ests$PE.cv, 2)))
  tab$CI.lo <- c(format(signif(pre.ests$PE.ci[1]/1000000, 2)), " ",
    format(signif(post.ests$PE.ci[1]/1000000, 2)))
  tab$CI.up <- c(format(signif(pre.ests$PE.ci[2]/1000000, 2)), " ",
    format(signif(post.ests$PE.ci[2]/1000000, 2)))
  tabl("Pre- and post-treatment population estimates of larval sea lampreys",
    " (PE, in millions) in the St. Marys River in ", YEAR,
    ", with the standard deviation (SD) of the estimates,",
    " coefficients of variation (CV), and",
    " 95% confidence intervals (CI.lo and CI.up).", TAB=tab, row.names=FALSE)
  rm(area.treated)

  # plot-specific estimates
  tab <- pid3[pid3$larvpe>0, c("new.numb", "area.ha", "trtd", "n.samp", "catch",
    "meannperha", "sd.dens", "larvpe", "pbig", "bigpe", "bigdens")]
  tab$hotspot <- tab$new.numb
  tab$cum.area <- cumsum(tab$area.ha)
  tab <- tab[, c("hotspot", "area.ha", "cum.area", "trtd", "n.samp", "catch",
    "meannperha", "sd.dens", "larvpe", "pbig", "bigpe", "bigdens")]
  tab$area.ha <- round(tab$area.ha)
  tab$cum.area <- round(tab$cum.area)
  tab$meannperha <- format(round(tab$meannperha), big.mark=",")
  tab$sd.dens <- format(round(tab$sd.dens), big.mark=",")
  tab$larvpe <- format(round(tab$larvpe), big.mark=",")
  tab$pbig <- format(round(tab$pbig, 2), nsmall=2)
  tab$bigpe <- format(round(tab$bigpe), big.mark=",")
  tab$bigdens <- format(round(tab$bigdens), big.mark=",")
  dimnames(tab)[[1]] <- seq(dim(tab)[1])
  tabl("Post-treatment ", YEAR, " population estimates of larval sea lampreys",
    " in St. Marys River hotspots in rank order.",
    "  Rank order is based on the density of large larvae (> 100 mm), then the",
    " density of all larvae.",
    "  Metrics include hotspot number (hotspot), area of hotspot (area.ha in",
    " ha), cumulative area of hotspots (cum.area), number of times the hotspot",
    " was treated in ", YEAR,  "(trtd), number of samples (n.samp), total sea",
    " lamprey catch (catch), and estimates of larval density (meannperha in",
    " no./ha) with standard deviation (sd.dens), and total number of larvae",
    " (larvpe), as well as the proportion (pbig), number (bigpe), and density",
    " (bigdens) of large larvae.",
    "  Only those hotspots with positive catch are shown.", TAB=tab)


  # figures

  # river map coordinates
  # limit river map to that portion that's been sampled

  del <- with(mapSMR, {
    !is.na(x) &
    (x < -84.186 & y < 46.248) |
    (x > -84.155 & x < -84.120 & y < 46.241) |
    (y < 46.222) |
    (x > -84.213 & y > 46.414 & y < 46.514) |
    (y > -22.733 - 0.821*x & y < 46.414)
  })
  mapSMR2 <- mapSMR[!del, ]

  addsmr <- function(df=mapSMR2, ...) {
    par.usr <- par("usr")
    sel <- df$x > par.usr[1] & df$x < par.usr[2] & df$y > par.usr[3] &
      df$y < par.usr[4]
    lines(df$x[sel], df$y[sel], ...)
    }

  # map of larval survey
  sel.trt <- smr3$new.numb %in% treated.plots
  sel.hit <- smr3$sl.total > 0
  fig <- function() {
    par(yaxs="i", xaxs="i")
    maps::map(type="n", xlim=range(smr3$longitude, na.rm=TRUE)+c(-0.002, 0.002),
      ylim=range(smr3$latitude, na.rm=TRUE), mar=c(0, 0, 0, 0))
    # all plots
    points(smr3$longitude[smr3$inbplot==1], smr3$latitude[smr3$inbplot==1],
      pch=16, cex=1, col="lightgreen")
    # treated plots
    points(smr3$longitude[sel.trt], smr3$latitude[sel.trt], pch=16, cex=1,
      col="yellow")
    # river boundary
    addsmr(col="gray")
    # hits and misses (excluding adaptive misses)
    points(smr3$longitude[!sel.hit], smr3$latitude[!sel.hit], pch=16,
      col="blue", cex=0.4)
    points(smr3$longitude[sel.hit], smr3$latitude[sel.hit], pch=16, col="red",
      cex=0.6)
    legend("right",
      c("Catch > 0", "No Catch", "Untreated Hotspot", "Treated Hotspot"),
      pch=16,  col=c("red", "blue", "lightgreen", "yellow"),
      pt.cex=1.5*c(0.6, 0.4, 1, 1), cex=1.5)
    }
  figu("Design and catch of the ", YEAR,
    " larval sea lamprey survey in the St. Marys River.", FIG=fig,
    newpage="port")
  rm(sel.trt, sel.hit)



  # identify plot (and period) in which each measured LARVAE was found
  lens.plus <- merge(LengthsClean,
    smr3[, c("sampid", "period", "inbplot", "new.numb", "trtd")], all.x=TRUE)
  # for post-trt lfr, adjust the catch from pre-trt surveys in the treated plots
  # by (1-bkill)
  sel <- lens.plus$period==-1
  lens.plus$sl.larv.adj[sel] <- with(lens.plus[sel, ],
    sl.larv.adj * ((1-bkill)^trtd))
  # then scale up to whole river estimate
  lens.plus$big <- lens.plus$sl.larv.adj*post.ests$PE/
    sum(lens.plus$sl.larv.adj)

  # LFR expanded to PE
  maxfreq <- max(tapply(lens.plus$big, lens.plus$len5, sum))
  all5 <- seq(min(lens.plus$len5), max(lens.plus$len5), 5)
  am2 <- matrix(NA, nrow=dim(lens.plus)[1], ncol=length(all5),
    dimnames=list(NULL, all5))
  am <- tapply(lens.plus$big, list(seq(dim(lens.plus)[1]), lens.plus$len5),
    sum)
  am2[, match(dimnames(am)[[2]], all5)] <- am
  am2[is.na(am2)] <- 0
  fig <- function() {
    par(mar=c(4, 5, 1, 1), cex=1.2)
    barplot(am2/1000, ylim=c(0, maxfreq+0.2)/1000, xlab="Length  (mm)", ylab="",
      las=1)
    abline(h=0)
    mtext("Estimated no. larvae in river  (thousands)", side=2, line=4, cex=1.2)
    }
  figu("Length frequency distribution of the post-treatment ", YEAR,
    " larval sea lamprey population.",
    "  Each rectangle represents a single sea lamprey larva captured during",
    " the survey, expanded to the whole St. Marys River population.",
    FIG=fig, h=3, w=4)
  rm(lens.plus, sel, maxfreq, am2, am, all5)


  # map of plot-specific estimates
  # assign symbol colors based on larval density
  dens.cat <- rep(0, dim(pid3)[1])
  dens.cat[pid3$meannperha>0] <- cut(pid3$meannperha[pid3$meannperha>0],
    breaks=9, labels=FALSE)





  colz <- ifelse(dens.cat==0, "white", brewcol(dens.cat))
  sel.trt <- pid3$new.numb %in% treated.plots
  sel.z <- dens.cat==0
  symsize <- 4*plotrix::rescale(sqrt(pid3$meannperha), 0:1)
  fig <- function() {
    par(yaxs="i", xaxs="i")
    maps::map(type="n", xlim=range(pid3$meanlong, na.rm=TRUE)+c(-0.002, 0.002),
      ylim=range(pid3$meanlat, na.rm=TRUE), mar=c(0, 0, 0, 0))
    # river boundary
    addsmr(col="lightgray")
    points(pid3$meanlong[!sel.z], pid3$meanlat[!sel.z], pch=21,
      cex=symsize[!sel.z], bg=colz[!sel.z])
    points(pid3$meanlong[sel.z], pid3$meanlat[sel.z], pch=4)
    }
  figu("Estimated ", YEAR, " post-treatment larval sea lamprey density in",
    " St. Marys River hotspots.",
    "  Larger and darker circles indicate higher larval densities,",
    " Xs represents hotspots with zero catches.", FIG=fig)
  rm(post.in, post.in.dens, pid2, EOGS, ADG, b0, b1, lens2, probs, dens.cat,
    colz, sel.trt)


  # save data to csv files

  # prepare catch and length data for inclusion in CATCHALL data base
  allmiss <- rep("", dim(smr3)[1])
  smr3$transamp <- allmiss
  smr3$transect <- allmiss
  smr3$site <- allmiss
  smr3$plot.num <- allmiss
  outcolumns <- c("year", "mm", "dd", "stime", "period", "sampid", "transamp",
    "transect", "site", "boat", "latitude", "longitude", "region", "label",
    "inbplot", "plot.num", "new.numb", "sample", "cluster", "depth", "hab.type",
    "sub.major", "sub.minor1", "sub.minor2", "ab.total", "i.total", "sl.total",
    "sl.adjctch", "sl.larv.n", "sl.larv.adj", "sl.meta.n", "sl.meta.adj",
    "comment")
  outcolumns <- c("year", "mm", "dd", "stime", "period", "sampid", "transamp",
    "transect", "site", "boat", "latitude", "longitude", "region", "label",
    "inbplot", "plot.num", "new.numb", "sample", "cluster", "depth", "hab.type",
    "sub.major", "sub.minor1", "sub.minor2", "ab.total", "i.total", "sl.larv.n",
    "sl.larv.adj", "comment")
  write.csv(smr3[, outcolumns], paste(Dir, Outfiles[1], sep="/"),
    row.names=FALSE, na="")

  LengthsClean$year <- rep(YEAR, dim(LengthsClean)[1])
  write.csv(LengthsClean[LengthsClean$class=="A",
    c("year", "sampid", "length", "sl.larv.adj")],
    paste(Dir, Outfiles[2], sep="/"),
    row.names=FALSE, na="")

  # save plot-specific estimates
  write.csv(pid3[, c("year", "period", "new.numb", "meanlat", "meanlong",
    "area.ha", "n.samp", "catch", "meannperha", "sd.dens", "larvpe", "ptran",
    "tranpe", "pbig", "bigpe")],
    paste(Dir, Outfiles[3], sep="/"), row.names=FALSE)

  rm(allmiss, outcolumns)

  endrtf()
}
