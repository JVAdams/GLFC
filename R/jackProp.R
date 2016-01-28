#' Proportion of Contributions to Jackknifed Range that Meet Target
#'
#' Given a collection of raw contributions to a jackknifed range,
#' and a corresponding target value, this function calculates the proportion
#' of those contributions that are less than the target.
#' @param jackRaw
#'   A data frame of raw (scaled) contributions to the index.  Rows are
#'   observations (e.g., years).  Columns include some identifiers (e.g.,
#'   year) as well as all the individuals (e.g., locations) that were left out
#'   when calculating the jackknifed range.
#' @param target
#'   A numeric scalar giving the target value with which the contributions
#'   will be compared.
#' @param idCols
#'   An integer vector giving the column numbers of the identification
#'   variables, default 1:2.  If there are no identification variables, use
#'   \code{idCols=NULL}.
#' @return
#'   A data frame with the same number of rows as \code{jackRaw} and with the
#'   identification variables plus a new variable, \code{pMeet} representing
#'   the proportion of the contributions that met the target.
#' @export
#' @details
#'   The index is simply the sum of the columns in \code{m} for
#'   each row.  The jackknifed range is produced by recalculating the index,
#'   leaving out one column at a time, then scaling up the result to the same
#'   scale as the index based on all columns.
#' @examples
#' # 3 years of population estimates from four streams
#' streampe <- matrix(c(1L, 2L, 7L, 4L, 11L, 9L, 5L, 3L, 10L, 8L, 6L, 12L), nrow=3, dimnames=list(1996:1998, letters[1:4]))
#' jI <- jackIndex(streampe, simple=FALSE)
#' jraw <- jI$jack.raw
#' jackProp(jackRaw=jraw, target=17, idCols=NULL)

jackProp <- function(jackRaw, target, idCols=1:2) {
  if(is.null(idCols)) {
    jrnoid <- jackRaw
    jrid <- NULL
    jp <- apply(jrnoid < target, 1, mean)
    jrid <- data.frame(pMeet=jp)
    row.names(jrid) <- row.names(jackRaw)
  } else {
    if(!is.numeric(idCols)) stop("idCols must be a vector of integers")
    if(identical(idCols, 0)) stop("Use idCols=NULL not idCols=0",
      " if there are no identification variables")
    jrnoid <- jackRaw[, -idCols]
    jrid <- jackRaw[, idCols]
    jp <- apply(jrnoid < target, 1, mean)
    jrid <- cbind(jrid, pMeet=jp)
  }
  return(jrid)
}
