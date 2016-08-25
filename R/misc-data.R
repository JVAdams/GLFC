#' @name
#'   mapL
#' @title
#'   Great Lakes Shorelines
#' @description
#'   List of five data frames, one for each Great Lake
#'   (Superior, Michigan, Huron, Erie, Ontario)
#'   with longitude and latitude coordinates of the shorelines.
#' @format
#'   One row for each point, consecutive rows with non-missing values for
#'   each line.
#' @source
#'   NOAA National Geophysical Data Center, (the now defunct) Coastline
#'   Extractor,
#'   \href{http://www.ngdc.noaa.gov/mgg/coast/}{ngdc.noaa.gov/mgg/coast}.
#' @author
#'   Rich Signell, USGS.
NULL
#' @name
#'   lsIndex
#' @title
#'   Great Lakes Streams used in the Index of Adult Sea Lampreys
#' @description
#'   IDs identifying streams to used to generate the Adult Index.
#' @format
#'   A list of 5 numeric vectors of lake-stream IDs for the 5
#'   Great Lakes, e.g., 1.064 = (lake ID) + (stream ID)/1000.
#' @source
#'   Great Lakes Fishery Commission (\href{http://www.glfc.org}{GLFC})
#'   Sea Lamprey Control Board Meeting 14-02,
#'   15-17 Oct 2014, Briefing Item 5 - Attachment 2,
#'   Transitioning to the New Adult Index in 2015.
#' @author
#'   GLFC Trapping Task Force.
NULL
#' @name
#'   Lakenames
#' @title
#'   Great Lakes Names
#' @description
#'   A vector with the names of the five Great Lakes.
#' @format
#'   A character vector, length 5.
NULL
#' @name
#'   Lakeabbs
#' @title
#'   Great Lakes Abbreviations
#' @description
#'   A vector with the first initials of the five Great Lakes.
#' @format
#'   A character vector, length 5.
NULL
#' @name
#'   blindcolz
#' @title
#'   Color-blind Friendly Colors
#' @description
#'   A vector with nine color-blind friendly colors in hex code.
#' @format
#'   A character vector, length 9.
#' @source
#'   How to make figures and presentations that are friendly to color blind
#'   people,
#'   20 November 2002,
#'   \href{http://jfly.iam.u-tokyo.ac.jp/html/color_blind/}{[link]}.
#' @author
#'   Masataka Okabe and Kei Ito.
NULL
#' @name
#'   lsKeep
#' @title
#'   Great Lakes Streams with Commitment to Adult Sea Lamprey Trapping
#' @description
#'   IDs identifying streams which will continue to have ongoing trapping
#'   even if not part of the Adult Index.
#' @format
#'   A list of 5 numeric vectors of lake-stream IDs for the 5
#'   Great Lakes, e.g., 1.064 = (lake ID) + (stream ID)/1000.
#' @source
#'   Great Lakes Fishery Commission (\href{http://www.glfc.org}{GLFC})
#'   Sea Lamprey Control Board Meeting 14-02,
#'   15-17 Oct 2014, Briefing Item 5 - Attachment 2,
#'   Transitioning to the New Adult Index in 2015.
#' @author
#'   GLFC Trapping Task Force.
NULL
#' @name
#'   index2pe
#' @title
#'   Factors to Scale Up the Adult Index to a Lake-Wide Population
#' @description
#'   Lake-specific conversion factors to scale up indices of adult sea
#'   lamprey abundance in the Great Lakes to lake-wide population estimates.
#' @format
#'   A named vector of length 5 (for the 5 Great Lakes)
#'   with factors rounded to the nearest hundredth.
#' @source
#'   Great Lakes Fishery Commission (\href{http://www.glfc.org}{GLFC})
#'   Sea Lamprey Control Board Meeting 14-02,
#'   15-17 Oct 2014, Briefing Item 5 - Attachment 2,
#'   Transitioning to the New Adult Index in 2015.
#' @author
#'   GLFC Trapping Task Force.
NULL
#' @name
#'   SMRStratArea
#' @title
#'   St. Marys River Strata Areas
#' @description
#'   Data frame with three variables: \code{inbplot} indicating whether the 
#'   stratum is in (=1) a high larval density area or not (=0), \code{region} 
#'   indicating the general location in the river (1 = North Channel, 
#'   2 = turning basin, 3 = widening part, 4 = Neebish channels, and 
#'   5 = most upstream part), and \code{haStrat} area of the stratum in 
#'   hectares.  Strata of the St. Marys River larval sea lamprey survey are 
#'   defined by \code{region} and \code{inbplot}.
#' @format
#'   A data frame with 9 rows and 3 columns.  Areas are rounded to the 
#'   nearest hundredth hectare.
#' @source
#'   Great Lakes Fishery Commission (\href{http://www.glfc.org}{GLFC})
#'   Sea Lamprey Control Board Meeting 14-02,
#'   15-17 Oct 2014, Briefing Item 6 - Attachment 2,
#'   Larval Assessment Task Force Minutes, item (3c) Follow up on changes to St.
#'   Marys River area used for estimation calculations (page 6-15).
#' @author
#'   GLFC Larval Assessment Task Force.
NULL
#' @name
#'   trappedStreams
#' @title
#'   General Information on Great Lakes Streams Trapped for Adult Sea Lampreys
#' @description
#'   Location information on trapped streams (past and present).
#' @format
#'   A data frame with 8 elements: \code{lake} (lake ID),
#'   \code{lscode} (lake-stream ID, lake + strcode/1000), \code{country}, 
#'   \code{strcode} (stream ID),
#'   \code{estr} (stream ID for Empiric Stream Treatment Ranking),
#'   \code{strname} (stream name), \code{lat} (latitude), 
#'   \code{long} (longitude).
#' @source
#'   Great Lakes Fishery Commission (\href{http://www.glfc.org}{GLFC})
#'   spawner model data base, last updated 12 May 2015.
#' @author
#'   GLFC Trapping Task Force.
NULL
#' @name
#'   GLFCenv
#' @title
#'   GLFC Package Local Environment
#' @description
#'   An environment local to the GLFC package, used to hold objects
#'   outside of the individual package functions
#' @format
#'   An environment.
#' @source
#'   Post from Hadley Wickham to r-help on 2 Dec 2014
#'  \href{https://stat.ethz.ch/pipermail/r-help/2014-December/423847.html}{[link]}.
NULL
