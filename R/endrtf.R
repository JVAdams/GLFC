#' Write and Close an RTF Document
#'
#' Write and close an rtf (rich text format) document.
#' @param rtf
#'   An rtf object, default \code{doc}.
#' @param details
#'   Logical scalar indicating if session details should be added to
#'   the end of the document, default FALSE.
#' @param ...
#'   Additional parameters to \code{\link[rtf]{addPageBreak}}.
#' @seealso
#'   \code{\link{startrtf}} for an example, \code{\link{heading}},
#'   \code{\link{para}}, \code{\link{tabl}},
#'   \code{\link{figu}}, \code{\link{figbig}},
#'   \code{\link[rtf]{RTF}}, \code{\link[rtf]{addPageBreak}}.
#' @import
#'   rtf
#' @export

endrtf <- function(rtf=doc, details=FALSE, ...) {
	if (details==TRUE) {
		addPageBreak(rtf, ...)
		addSessionInfo(rtf)
		}
	done(rtf)
	}
