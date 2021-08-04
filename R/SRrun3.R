#' Three-Year Running Mean
#'
#' Calculate the three-year moving average of a time series of data,
#' by the specified grouping variables.
#'
#' @param df
#'   A data frame containing the data to be averaged.
#' @param x
#'   A character scalar, naming the element of \code{df} with the
#'   numeric estimates to be averaged.
#' @param group
#'   A character scalar, naming the element of \code{df} with the
#'   grouping variable.
#' @param time
#'   A character scalar, naming the element of \code{df} with the
#'   numeric variable denoting time.
#' @param minfreq
#'   A numeric scalar, the minimum number of non-missing observations
#'   required to calculate an average, default 2.
#' @return
#'   A data frame with one row per group and time, giving the raw observations
#'   (Y) and their corresponding three-year running mean (Y.3mn).
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' rawdat <- data.frame(
#'   Year = 1990 + c(0:5, 8:9, 2:3, 5:8),
#'   Type = rep(1:2, c(8, 6)),
#'   Y = c(1:12, 9, 7)*10
#' )
#' SRrun3(rawdat, "Y", "Type", "Year", minfreq=1)
#' SRrun3(rawdat, "Y", "Type", "Year")
#' SRrun3(rawdat, "Y", "Type", "Year", minfreq=3)

SRrun3 <- function(df, x, group, time, minfreq=2) {
  df2 <- df
  df2$Fx <- df[[x]]
  df2$Fgroup <- df[[group]]
  df2$Ftime <- df[[time]]
  look <- tidyr::complete(df2, Fgroup, Ftime=full_seq(Ftime, 1)) %>%
    group_by(Fgroup) %>%
    arrange(Fgroup, Ftime) %>%
    mutate(
      lag1=lag(Fx, 1),
      lag2=lag(Fx, 2),
      nrun= (!is.na(Fx)) + (!is.na(lag1)) + (!is.na(lag2))
    ) %>%
    ungroup()
  look[[time]] <- look$Ftime
  look[[group]] <- look$Fgroup
  fullrun3 <- rowMeans(look[, c("Fx", "lag1", "lag2")], na.rm=TRUE)
  look[[paste0(x, ".3mn")]] <- ifelse(look$nrun<minfreq, NA, fullrun3)
  select(look, -Fx, -Fgroup, -Ftime, -lag1, -lag2, -nrun)
}
