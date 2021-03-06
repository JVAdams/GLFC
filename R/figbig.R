#' Add a Big Figure to an RTF Document
#'
#' Add a big figure to an rtf (rich text format) document.
#' @param ...
#'   One or more character scalars (separated by commas) of text to use for
#'   the figure caption.
#' @param FIG
#'   A function to create a figure which will be added to the document,
#'   default \code{fig}.
#' @param rtf
#'   An rtf object, default \code{doc}.
#' @param figc
#'   Numeric scalar figure number to use in caption, default
#'   \code{GLFCenv$figcount}.
#' @param boldt
#'   Logical scalar indicating if figure number should use bold font,
#'   default TRUE.
#' @param w
#'   Numeric scalar width of figure in inches, default 6.5.
#' @param h
#'   Numeric scalar height of figure in inches, default 8.
#' @param rf
#'   Numeric scalar resolution of figure, default 300.
#' @param newpage
#'   Character scalar indicating if the figure should start on a new page in
#'   the document "port" for a new portrait page,
#'   "land" for a new landscape page, and "none" for no new page (the default).
#' @param omi
#'   Numeric vector, length 4, width of document page margins in inches
#'   (bottom, left, top, right), default c(1, 1, 1, 1).
#' @return
#'   A 1 is added to the numeric vector of length 1, \code{GLFCenv$figcount},
#'   stored in the working directory to keep track of the number of
#'   figures written to the rtf document, and label the captions accordingly.
#' @details
#'   The figure and caption are written to the rtf file.
#'   The size of a new page is assumed to be 11 by 17 inches.
#' @seealso
#'   \code{\link{startrtf}} for an example, \code{\link{heading}},
#'   \code{\link{para}}, \code{\link{tabl}},
#'   \code{\link{endrtf}}, \code{\link[rtf]{RTF}}.
#' @import
#'   rtf
#' @export

figbig <- function(..., FIG=fig, rtf=doc, figc=GLFCenv$figcount, boldt=TRUE,
    w=NULL, h=NULL, rf=300, newpage="none", omi=c(1, 1, 1, 1)) {
  wf <- if (is.null(w)) {
    9
  } else {
    w
  }
  hf <- if (is.null(h)) {
    14
  } else {
    h
  }
  if (newpage=="none") {
    addNewLine(this=rtf)
  }
  if (newpage=="port") {
    addPageBreak(this=rtf, width=11, height=17, omi=omi)
  }
  if (newpage=="land") {
    wf <- if (is.null(w)) {
      15
    } else {
      w
    }
    hf <- if (is.null(h)) {
      8
    } else {
      h
    }
    addPageBreak(this=rtf, width=17, height=11, omi=omi)
  }
  addPlot(this=rtf, plot.fun=FIG, width=wf, height=hf, res=rf)
  addNewLine(this=rtf)
  addNewLine(this=rtf)
  startParagraph(this=rtf)
  addText(this=rtf, paste0("Figure ", figc, ". "), bold=boldt)
  addText(this=rtf, ...)
  endParagraph(this=rtf)
  addNewLine(this=rtf)
  addNewLine(this=rtf)
  GLFCenv$figcount <- figc + 1
}
