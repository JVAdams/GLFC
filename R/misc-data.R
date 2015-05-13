#' @name
#'   lsIndex
#' @title
#'   Great Lakes Streams used in the Index of Adult Sea Lampreys
#' @description
#'   IDs identifying streams to used to generate the Adult Index.
#' @format
#'   A list of 5 numeric vectors of stream IDs for the 5
#'   Great Lakes, e.g., 1.064 = (lake code) + (stream code)/1000.
#' @source
#'   Great Lakes Fishery Commission (\href{http://www.glfc.org}{GLFC})
#'   Sea Lamprey Control Board Meeting 14-02,
#'   15-17 Oct 2014, Briefing Item 5 - Attachment 2,
#'   Transitioning to the New Adult Index in 2015.
#' @author
#'   GLFC Trapping Task Force.
NULL
#' @name
#'   lsKeep
#' @title
#'   Great Lakes Streams with Commitment to Adult Sea Lamprey Trapping
#' @description
#'   IDs identifying streams which will continue to have ongoing trapping
#'   even if not part of the Adult Index.
#' @format
#'   A list of 5 numeric vectors of stream IDs for the 5
#'   Great Lakes, e.g., 1.064 = (lake code) + (stream code)/1000.
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
#'   Factors to Scale Up the Adult Index to a Lakewide Population
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
# information on trapped streams (past and present)
# including: lake, lscode, country, strcode, estr, strname, lat, long
# last updated 12 May 2015

#' @name
#'   trappedStreams
#' @title
#'   General Information on Great Lakes Streams Trapped for Adult Sea Lampreys
#' @description
#'   Location information on trapped streams (past and present).
#' @format
#'   A data frame with 8 elements: lake (lake code),
#'   lscode (lake + strcode/1000), country, strcode (stream code),
#'   estr (stream ID for Empiric Stream Treatment Ranking),
#'   strname (stream name), lat (latitude), long (longitude).
#' @source
#'   Great Lakes Fishery Commission (\href{http://www.glfc.org}{GLFC})
#'   spawner model data base, last updated 12 May 2015.
#' @author
#'   GLFC Trapping Task Force.
NULL
