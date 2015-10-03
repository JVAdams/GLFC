#' Add a Paragraph to an RTF Document
#'
#' Add a paragraph to an rtf (rich text format) document.
#' @param ...
#'   One or more character scalars (separated by commas) of text to add to
#'   document as a single paragraph.
#' @param rtf
#'   An rtf object, default \code{doc}.
#' @param bold
#'   Logical scalar indicating if paragraph should use bold font, default FALSE.
#' @param italic
#'   Logical scalar indicating if paragraph should use italic font,
#'   default FALSE.
#' @details
#'   The specified heading is written to the rtf file.
#' @seealso
#'   \code{\link{startrtf}} for an example, \code{\link{heading}},
#'   \code{\link{tabl}}, \code{\link{figu}},
#'   \code{\link{figbig}}, \code{\link{endrtf}},
#'   \code{\link[rtf]{RTF}}.
#' @import
#'   rtf
#' @export

para <- function(..., rtf=doc, bold=FALSE, italic=FALSE) {
  startParagraph(this=rtf)
  addText(this=rtf, ..., bold=bold, italic=italic)
  endParagraph(this=rtf)
  addNewLine(this=rtf)
  }
