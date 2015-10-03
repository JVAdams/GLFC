#' Index with Jackknifed Range
#'
#' Given a collection of estimates contributing to an index, this function
#' provides the estimated index (sum) of observations (across a row) and
#' the jackknifed range of the index, the result of leaving out
#' an entire column (e.g., a location) one at a time.
#' @param m
#'   A numeric matrix of estimates contributing to the index.  Rows are
#'   observations (e.g., years).  Columns are individuals (e.g., locations).
#' @return
#'   A numeric matrix with 3 columns, the index, and the lower and
#'   upper jackknifed range of the index.
#' @export
#' @details
#'   The index is simply the sum of the columns in \code{m} for
#'   each row.  The jackknifed range is produced by recalculating the index,
#'   leaving out one column at a time, then scaling up the result to the same
#'   scale as the index based on all columns.
#' @examples
#' # 3 years of population estimates from four streams
#' streampe <- matrix(1:12, nrow=3, dimnames=list(1996:1998, letters[1:4]))
#' jackIndex(streampe)

jackIndex <- function(m) {
  if (any(is.na(m))) stop("The input matrix may not have any missing values.")
  # calculate the index as the sum of the columns for each row
  rowsum <- apply(m, 1, sum)
  # calculate the mean of the index
  avgind <- mean(rowsum)
  # recalculate the index, leaving out one individual at a time
  loo <- apply(m, 2, function(column) (rowsum - column))
  # rescale the loo index, relative to mean
  looscaled <- apply(loo, 2, function(x) x/mean(x))
  # convert to original scale
  looscaled2 <- looscaled * avgind
  # calculate range
  ranges <- t(apply(looscaled2, 1, range))
  cbind(index=rowsum, jlo=ranges[, 1], jhi=ranges[, 2])
  }
