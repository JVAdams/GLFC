#' Latest GitHub Version
#'
#' Compare the version of the currently downloaded R package to the
#' latest version available on GitHub.
#'
#' @param name
#'   A character scalar identifying the name of the GitHub user where the R
#'   package repository is located online, default \code{"JVAdams"}.
#' @param pkg
#'   A character scalar identifying the name of the R package,
#'   default \code{"GLFC"}.
#' @return
#'   A message is returned, reporting the version status.
#' @importFrom curl has_internet
#' @export
#' @examples
#' \dontrun{
#'  library(GLFC)
#'  githubver()
#' }

githubver <- function(name="JVAdams", pkg="GLFC") {

  if(curl::has_internet()) {

    current.version <- suppressWarnings(packageDescription(pkg, lib.loc=NULL,
      fields="Version"))
    if(is.na(current.version)) current.version <- "X"

    prefix <- "https://raw.githubusercontent.com/"
    name.pkg <- paste(name, pkg, sep="/")
    suffix <- "/master/DESCRIPTION"
    url <- paste0(prefix, name.pkg, suffix)

    # read the second line of the description file and save the version number
    latest.version <- strsplit(readLines(url)[2], " ")[[1]][2]

    if(current.version!=latest.version) {
      stop("\nYou do not have the latest version of ", pkg,
        " installed.\n",
        "Please install the latest version using this code:\n\n",
        "install.packages('remotes')\n",
        "remotes::install_github('", name.pkg, "')\n\n",
        call.=FALSE)
      }
  } else {
    warning("\nNo internet connection.\n",
     "Can't check to see if you have the latest version of the ",
      pkg, " package.\n\n",
      call.=FALSE)
  }
}

# ############# testing ###############
# # no package installed
# githubver("hrbrmstr", "dtupdate")
# # old package installed
# githubver("tysonstanley", "dissertateUSU")
# # latest package installed
# githubver()
# # no internet connection
# githubver()
