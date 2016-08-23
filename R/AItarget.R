#' Calculate Targets for the Adult Index
#'
#' Calculate lake-wide targets for the Adult Index of sea lamprey
#' populations in the Great Lakes from the mean of specified years.
#' @param lakeIndex
#'   A data frame of annual lake-wide Adult Indices with at least these
#'   3 columns: \code{lake}, \code{year}, and the Adult Index \code{index}.
#' @param years
#'   A list of length 5 (for each Great Lake respectively: Superior, Michigan,
#'   Huron, Erie, and Ontario), each element an integer vector of length 5
#'   specifying the 5 years during which there were acceptable sea lamprey
#'   wounding rates on lake trout, default
#'   list(1994:1998, 1995:1999, 1989:1993, 1991:1995, 1993:1997).
#'   These will be used to generate Adult Index targets.
#' @param adjust
#'   A numeric vector of length 5 (for each Great Lake),
#'   specifying adjustments to be made to the calculated means to
#'   generate Adult Index targets, default c(1, 5/8.9, 0.25, 1, 1).  Since Lake
#'   Huron did not have a time period with acceptable sea lamprey wounding
#'   rates, the target was set to 25\% of the mean for 1989-1993.
#' @details
#'   The Lake Michigan Committee accepted the Sea Lamprey Control Board's 
#'   recommendation for an Adult Index target at its Executive Session, 
#'   22 March 2016.  Specifically, the years of acceptable wounding used to 
#'   define the Adult Index target for Lake Michigan sea lampreys was 
#'   changed to a time period with available Adult Index data, 1995-1999, and 
#'   the index target was defined as the mean of the Adult Index during these 
#'   years, adjusted by 5/8.9 to compensate for the highest wounding rate 
#'   observed during this period.
#' @return
#'   A data frame with the calculated targets for the Adult Index of each
#'   Great Lake, with 2 columns: \code{lake} and \code{targInd}.
#' @export

AItarget <- function(lakeIndex,
  years=list(1994:1998, 1995:1999, 1989:1993, 1991:1995, 1993:1997),
  adjust=c(1, 5/8.9, 0.25, 1, 1)) {

  targ <- data.frame(lake=1:5, targInd=rep(NA, 5))

  for(i in 1:5) {
    pick5 <- lakeIndex$lake==i & is.element(lakeIndex$year, years[[i]])
    targ$targInd[i] <- mean(lakeIndex$index[pick5])*adjust[i]
    }

  targ
}
