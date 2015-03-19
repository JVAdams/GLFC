#' Index of Sea Lamprey Adults with Jackknifed Range
#'
#' Estimated Adult Index of sea lamprey with the observed range
#' in the index when one stream
#' at a time is excluded from the estimation.
#' @param m
#'   A numeric matrix of stream run size estimates with
#'   observation years as rows and individual streams as columns.
#' @return
#'   A numeric matrix with three columns, the Adult Index, and the lower and
#'   upper jackknifed range.
#' @export
#' @details
#'   The annual Adult Index is simply the sum of the columns in \code{m} for
#'   each row.  The jackknifed range is produced by recalculating the index,
#'   leaving out one stream at a time, then scaling up the result to the same
#'   scale as the Adult Index based on all streams.
#' @examples
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
