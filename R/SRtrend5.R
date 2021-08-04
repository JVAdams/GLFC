#' Five-Year Linear Trend
#'
#' Calculate the five-year linear trend of a time series of data,
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
#' @param lasttime
#'   A numeric scalar, the last value of time that should be used to calculate
#'   the trend. The default, NULL, results in \code{lasttime} being set to the
#'   maximum time in \code{df}.
#' @return
#'   A data frame with one row per group and last time, giving the intercept
#'   (Y.5nt), slope (Y.5sl), and P value (Y.5pv) for the five-year linear trend.
#' @importFrom broom tidy
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' rawdat <- data.frame(
#'   Year = 1990 + c(0:5, 8:9, 2:3, 5:8),
#'   Type = rep(1:2, c(8, 6)),
#'   Y = c(1:12, 9, 7)*10
#' )
#' SRtrend5(rawdat, "Y", "Type", "Year")
#' SRtrend5(rawdat, "Y", "Type", "Year", lasttime=1995)

SRtrend5 <- function(df, x, group, time, lasttime=NULL) {
  df2 <- df
  df2$Fx <- df[[x]]
  df2$Fgroup <- df[[group]]
  df2$Ftime <- df[[time]]
  look <- tidyr::complete(df2, Fgroup, Ftime=full_seq(Ftime, 1))
  if(is.null(lasttime)) {
    lasttime <- max(look$Ftime)
  }
  sub <- look %>%
    filter(
      Ftime %in% (lasttime - (4:0)),
      !is.na(Fx),
      !is.na(Ftime)
    ) %>%
    nest(data = -Fgroup) %>%
    mutate(
      fit = purrr::map(data, ~ lm(Fx ~ Ftime, data=.x)),
      tidied = purrr::map(fit, broom::tidy)
    ) %>%
    unnest(tidied)
  part1 <- sub %>%
    filter(term=="Ftime") %>%
    select(Fgroup, slope=estimate, p.value)
  part2 <- sub %>%
    filter(term=="(Intercept)") %>%
    select(Fgroup, int=estimate)
  both <- full_join(part2, part1, by="Fgroup")

  both[[group]] <- both$Fgroup
  both[[time]] <- lasttime
  both[[paste0(x, ".5nt")]] <- both$int
  both[[paste0(x, ".5sl")]] <- both$slope
  both[[paste0(x, ".5pv")]] <- both$p.value
  select(both, -Fgroup, -int, -slope, -p.value)
}
