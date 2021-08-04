#' Great Lakes Fishery Commission
#'
#' \pkg{GLFC} contains functions developed for the Great Lakes Fishery
#' Commission's sea lamprey control program, including estimation of the index
#' of adult sea lamprey abundance (functions starting with "AI") and estimation
#' of larval sea lamprey abundance from deepwater electofishing surveys
#' (functions starting with "DWEF").
#'
#' An example of how to use the functions in **GLFC** to estimate indices of
#' adult sea lamprey abundance in the Great Lakes is given in this
#' [vignette](https://rawgit.com/JVAdams/GLFC/master/vignettes/Adult-Index.html).
#' Use \code{\link{AIprep}} to prepare the Adult Index data,
#' \code{\link{AIcheck}} to error check the data,
#' \code{\link{AIestimate}} to estimate the Adult Index,
#' \code{\link{AItarget}} to calculate targets for the Adult Index, and
#' \code{\link{AIreport}} to generate a draft Adult Index report.
#'
#' Or use \code{\link{AIpresto}} to do it all in one fell swoop.
#'
#' \emph{U.S. Geological Survey} (USGS) Computer Program \pkg{GLFC}
#'   version 1.5.3.9000
#' Written by Jean V. Adams, USGS - Great Lakes Science Center
#' \href{http://www.glsc.usgs.gov/}{glsc.usgs.gov}, Ann Arbor, Michigan, USA.
#' Written in programming language R (R Core Team, 2021, www.R-project.org),
#' version 4.1.0 (2021-05-18).
#' Run on a PC with Intel(R) Core(TM) i7-7600U CPU, 2.80 GHz processor,
#' 16.0 GB RAM, and Microsoft Windows 10 Enterprise operating system.
#' Source code is available from Jean V. Adams on GitHub,
#' \href{https://github.com/JVAdams/GLFC}{github.com/JVAdams/GLFC},
#' \emph{jvadams (at) usgs (dot) gov}.
#'
#' \emph{Disclaimer:}  Although this program has been used by the USGS,
#' no warranty, expressed or implied, is made by the USGS or the United States
#' Government as to the accuracy and functioning of the program and related
#' program material nor shall the fact of distribution constitute any such
#' warranty, and no responsibility is assumed by the USGS in connection
#' therewith.
#'
#' @name GLFC
#' @docType package
NULL
