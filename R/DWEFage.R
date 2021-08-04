#' Estimate ages of sea lamprey larvae from lengths
#'
#' Generate Estimates of larval sea lamprey ages from the measured lengths of
#' yearling and older larvae.
#' @param JustLens
#'   A numeric vector with measured lengths of sea lamprey larvae for which
#'   age estimates are desired. It is assumed that no young of the year (YOY),
#'   age-0 larvae are included in the sample.
#' @param LenFreq
#'   A numeric vector with the number of larvae corresponding to each
#'   measured length in \code{JustLens}. The default value will result in each
#'   length in \code{JustLens} being counted as a single observation.
#' @param AgeLenKey
#'   A data frame with numeric variables \code{Length} and \code{Age} giving
#'   measured lengths and ages of sea lamprey larvae. The default value (NULL)
#'   will result in the use of a built in data base from 504 larvae collected
#'   during 1993-2001.
#' @return
#'   A list of length two is returned.
#'   The first element is a numeric matrix with the number of larvae in each
#'   age (row) by length (column) category. There are 7 age categories from 1
#'   to 7 and 31 length categories:
#'   <=20, >20 to <=25, ..., >160 to <=165, > 165.
#'   The second element is a numeric vector with the number of larvae in each
#'   age category.
#' @importFrom tibble tibble
#' @export
#' @references
#'
#' Haeseker, SL, ML Jones, and JR Bence. 2003.
#'  Estimating uncertainty in the stock-recruitment relationship for St. Marys
#'  River sea lampreys.
#'  Journal of Great Lakes Research 29(Suppl. 1):728-741.
#'  \href{https://doi.org/10.1016/S0380-1330(03)70527-9}{[link]}
#'
#' Hoenig, JM and DM Heisey. 1987.
#'  Use of a log-linear model with the EM algorithm to correct estimates of
#'  stock composition and to convert length to age.
#'  Transactions of the American Fisheries Society 116(2):232-243.
#'
#' @examples
#' DWEFage(c(120, 76, 39))

DWEFage <- function(JustLens, LenFreq=rep(1, length(JustLens)), AgeLenKey=NULL) {

  #### 0 - definition of variables ####

  # n - number of individuals in a sample
  # i - age index (1:7)
  # j - length bin index (<20, 20-25, ... 160-165, >165)
  # k - sample ID: 1 - aged fish; 2 - lengths only
  # s - iteration index
  # P - conditional probability given age
  # a - proportion at age


  #### 1 - initial values ####

  old_likely <- 0  # small likelihood to start
  likely_diff <- 10  # large difference to start
  step <- 0
  nero <- 1e-4


  #### 2 - set up arrays, etc ####

  # 7 ages, 31 length bins, 2 samples
  n <- array(0, dim=c(7, 31, 2))
  new_n <- array(0, dim=c(7, 31))


  #### 3 - create aged sample from data ####

  # provide default data if no new data is given
  if(is.null(AgeLenKey)) {
    AgeLenKey <- tibble(
      Year = rep(1993:2001, c(57, 55, 15, 35, 0, 35, 77, 122, 108)),
      Specimen.State = rep(c("Frozen", "Fresh"), c(162, 342)),
      Length = c(27, 31, 31, 32, 32, 36, 39, 39, 42, 44, 45, 45, 45, 47, 50,
        51, 51, 53, 53, 56, 60, 60, 62, 63, 64, 65, 65, 66, 66, 68, 70,
        71, 73, 78, 79, 80, 83, 84, 87, 88, 89, 91, 95, 96, 115, 118,
        118, 121, 122, 125, 125, 126, 127, 129, 132, 146, 153, 21, 28,
        28, 31, 53, 58, 64, 67, 68, 71, 72, 73, 76, 76, 77, 77, 81, 82,
        83, 85, 87, 88, 89, 89, 89, 89, 92, 95, 96, 97, 99, 100, 104,
        106, 106, 107, 110, 117, 117, 117, 122, 122, 123, 123, 124, 124,
        132, 132, 133, 134, 134, 137, 137, 144, 145, 94, 96, 112, 121,
        123, 124, 125, 126, 127, 128, 130, 130, 132, 135, 137, 52, 57,
        62, 63, 74, 80, 83, 83, 85, 94, 99, 100, 101, 104, 110, 110,
        110, 112, 114, 115, 117, 117, 119, 120, 121, 125, 126, 127, 128,
        132, 136, 138, 140, 146, 147, 92, 95, 99, 102, 105, 105, 110,
        112, 112, 113, 113, 115, 116, 119, 119, 120, 120, 122, 123, 125,
        126, 127, 128, 128, 128, 129, 132, 133, 135, 136, 136, 141, 145,
        146, 157, 66, 85, 87, 88, 97, 100, 103, 105, 105, 110, 110, 110,
        112, 113, 113, 114, 115, 115, 116, 120, 120, 122, 123, 126, 128,
        128, 130, 130, 130, 130, 130, 131, 132, 132, 132, 133, 134, 136,
        140, 142, 142, 142, 142, 142, 142, 144, 145, 145, 146, 146, 146,
        148, 148, 148, 150, 150, 150, 151, 152, 154, 155, 155, 157, 160,
        160, 160, 161, 163, 164, 166, 166, 166, 169, 170, 173, 175, 180,
        26, 27, 27, 28, 29, 29, 30, 30, 30, 31, 31, 31, 32, 32, 32, 33,
        33, 33, 33, 34, 35, 35, 35, 36, 36, 40, 44, 45, 47, 49, 50, 50,
        51, 51, 51, 51, 52, 53, 53, 54, 54, 54, 54, 54, 55, 55, 55, 55,
        55, 56, 56, 56, 57, 57, 57, 57, 57, 57, 58, 58, 58, 59, 60, 60,
        60, 61, 61, 61, 61, 61, 62, 62, 63, 63, 63, 64, 64, 64, 64, 65,
        65, 65, 65, 66, 66, 66, 67, 67, 67, 68, 68, 70, 70, 71, 71, 72,
        73, 73, 73, 76, 76, 76, 77, 78, 79, 79, 79, 81, 81, 81, 82, 88,
        90, 91, 95, 106, 117, 119, 120, 127, 132, 143, 16, 17, 19, 19,
        19, 21, 22, 22, 22, 22, 22, 23, 24, 24, 25, 25, 26, 27, 27, 28,
        29, 30, 30, 32, 33, 34, 35, 35, 36, 36, 38, 39, 39, 40, 41, 42,
        42, 43, 45, 45, 45, 46, 46, 46, 46, 46, 47, 47, 47, 47, 48, 49,
        51, 51, 51, 52, 52, 52, 52, 52, 54, 54, 55, 55, 58, 58, 59, 61,
        62, 64, 65, 66, 66, 67, 67, 71, 72, 73, 73, 74, 75, 76, 76, 77,
        81, 83, 85, 85, 88, 90, 94, 97, 100, 101, 102, 103, 103, 105,
        107, 110, 111, 112, 114, 117, 131, 134, 137, 139),
      Age = c(1, 1, 1, 1, 1, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2,
        2, 3, 1, 2, 2, 2, 2, 1, 2, 2, 2, 3, 2, 3, 4, 3, 4, 2, 2, 2, 3,
        2, 4, 4, 4, 2, 4, 4, 3, 3, 4, 4, 4, 5, 5, 3, 4, 2, 1, 2, 2, 3,
        2, 2, 2, 3, 1, 4, 3, 3, 4, 2, 3, 2, 4, 4, 3, 3, 4, 2, 3, 4, 4,
        4, 2, 4, 4, 3, 4, 4, 3, 4, 3, 3, 3, 4, 5, 3, 3, 3, 4, 3, 4, 4,
        4, 5, 4, 6, 4, 7, 4, 4, 2, 3, 4, 3, 4, 4, 5, 4, 3, 5, 4, 7, 4,
        4, 6, 2, 2, 2, 2, 2, 2, 3, 3, 2, 2, 4, 3, 3, 4, 2, 2, 4, 3, 4,
        4, 4, 4, 6, 3, 5, 4, 3, 5, 4, 3, 3, 3, 4, 5, 5, 3, 4, 3, 3, 3,
        4, 3, 3, 3, 3, 3, 4, 3, 3, 4, 4, 6, 3, 3, 4, 4, 4, 3, 3, 4, 4,
        6, 4, 4, 4, 5, 4, 4, 5, 4, 3, 2, 2, 2, 3, 3, 3, 3, 4, 3, 4, 4,
        3, 3, 4, 4, 3, 5, 4, 3, 4, 4, 4, 4, 3, 3, 3, 4, 4, 4, 5, 6, 3,
        4, 4, 4, 4, 4, 3, 4, 4, 4, 4, 5, 6, 4, 4, 5, 4, 4, 4, 3, 3, 3,
        4, 5, 5, 5, 5, 5, 3, 4, 6, 4, 4, 4, 4, 5, 4, 4, 5, 5, 6, 4, 6,
        5, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1, 1,
        2, 1, 2, 2, 1, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        4, 2, 2, 2, 2, 2, 3, 2, 3, 3, 2, 2, 3, 3, 3, 3, 4, 4, 4, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1,
        2, 2, 1, 2, 2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 2, 2, 1, 2, 1, 2, 1,
        2, 2, 2, 3, 5, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 3, 3,
        3, 4, 3, 3, 3, 4, 4, 4, 3, 4, 4, 3, 4, 3, 4, 3, 4, 4, 4, 4, 5,
        5)
    )
  }

  # length bins
  bin_levels <- c(0, seq(20, 165, 5), 200)

  AgeLenKey$Lenbins <- cut(AgeLenKey$Length, breaks=bin_levels, labels=FALSE)

  # cross-tab of Ages and Length Bins
  lenage_key <- table(AgeLenKey$Age, AgeLenKey$Lenbins)

  # add actual data to n[age, lenbin, 1] - sample 1
  n[ , , 1] <- lenage_key[, 1:31]


  #### 4 - read in data for second sample ####

  # length frequency w/out ages, sample 2
  Lens.long <- rep(JustLens, LenFreq)
  Lens.bins <- cut(Lens.long, breaks=bin_levels)
  curr_lengths <- as.numeric(table(Lens.bins))
  curr_lengths[curr_lengths < nero] <- nero


  #### 5 - start E-M loop with initial guess ####

  for (i in 1:7) {
    for (j in 1:31) {
      n[i, j, 2] <- curr_lengths[j] * n[i, j, 1]/colSums(n[ , , 1])[j]
    }}


  #### 6 - enter iterative loop ####

  while (likely_diff > 1e-4) {
    # M step
    P <- (n[ , , 1] + n[ , , 2]) /
      (rowSums(n[ , , 1]) + rowSums(n[ , , 2],) + 1e-6)
    a <- rowSums(n[ , , 2]) / (sum(n[ , , 2]) + 1e-6)

    # E step
    for (i in 1:7) {
      for (j in 1:31) {
        new_n[i, j] <- P[i, j] * rowSums(n[ , , 2])[i] * colSums(n[, , 2])[j] /
          (colSums(P * rowSums(n[ , , 2]))[j] + 1e-6)
      }}
    n[ , , 2] <- new_n

    # Likelihood step
    x1 <- sum((n[ , , 1] + n[ , , 2]) * log(P + 1e-6))
    x2 <- sum(rowSums(n[ , , 2]) * log(a + 1e-6))
    likely <- x1 + x2

    # get difference in likelihood from last time
    likely_diff <- abs(likely - old_likely)
    old_likely <- likely
    step <- step + 1
  }

  adjust <- sum(LenFreq)/sum(new_n)

  out.m <- adjust*new_n
  dimnames(out.m) <- list(1:7, names(table(Lens.bins)))

  out.v <- adjust*rowSums(new_n)

  return(list(AgeLenFreq=out.m, AgeFreq=out.v))

}
