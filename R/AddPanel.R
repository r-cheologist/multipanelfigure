#' @title AddPanel
#' @description A convenience function adding graphical objects to a
#' \code{\link[gtable]{gtable}} constructed by \code{\link{MultiPanelFigure}}.
#' @details Currently supported as panel-representing objects (\code{panel}) are
#' \enumerate{
#'   \item{\code{\link[ggplot2]{ggplot}} objects.}
#'   \item{Single \code{\link{character}} objects representing paths to readable
#'     portable network graphics (\code{*.png}), tagged image file format
#'     (\code{*.tiff}/\code{*.tif}) or joint photographic experts group
#'     (\code{*.jpg}/\code{*.jpeg}) files, which will be read and placed into
#'     panels as requested.}}
#' Note that \code{*.jpg}/\code{*.jpeg} files must be produced
#' using the dimensions of the panle(s) they are to be placed in for sensible
#' results. \code{\link[ggplot2]{ggplot}} objects obviously auto-scale and
#' \code{*.tiff}/\code{*.tif}, as well as \code{*.png} files have their native
#' sizes read out of the file (which isn't working for
#' \code{*.jpg}/\code{*.jpeg}).
#' @param panel Single \code{\link{character}} object representing path to a
#' bitmap image (\code{*.png}, \code{*.tiff}/\code{*.tif},
#' \code{*.jpg}/\code{*.jpeg}) or a \code{\link[ggplot2]{ggplot}} object to be
#' placed in a multipanel figure.
#' See 'Details'.
#' @param figure \code{\link[gtable]{gtable}} object as produced by
#' \code{\link{MultiPanelFigure}} and representing the figure the panel is to be
#' placed in.
#' @param topPanel Single \code{\link{numeric}} indicating the row index of
#' the panel that is to be placed in the figure.
#' @param bottomPanel Single \code{\link{numeric}} indicating the lower row
#' index of the panel that is to be placed in the figure. Important for
#' definition of panel spanning (see examples).
#' @param leftPanel Single \code{\link{numeric}} indicating the column index
#' of the panel that is to be placed in the figure.
#' @param rightPanel Single \code{\link{numeric}} indicating the right column
#' index of the panel that is to be placed in the figure. Important for
#' definition of panel spanning (see examples).
#' @param panelLabel Single \code{\link{character}} object defining the panel
#' label used for automated annotation.
#' @return Returns the \code{\link[gtable]{gtable}} object fed to it
#' (\code{figure}) with the addition of the \code{panel}.
#' @author Johannes Graumann
#' @export
#' @seealso \code{\link[gtable]{gtable}}, \code{\link{MultiPanelFigure}},
#' \code{\link[tiff]{readTIFF}}, \code{\link[png]{readPNG}},
#' \code{\link[jpeg]{readJPEG}}
#' @importFrom assertive assert_has_all_attributes
#' @importFrom assertive assert_is_inherited_from
#' @importFrom assertive assert_is_a_string
#' @importFrom assertive assert_all_are_readable_files
#' @importFrom assertive assert_is_a_number
#' @importFrom assertive assert_all_are_whole_numbers
#' @importFrom assertive assert_all_are_in_range
#' @importFrom assertive assert_all_are_true
#' @importFrom png readPNG
#' @importFrom tiff readTIFF
#' @importFrom jpeg readJPEG
#' @importFrom grid unit
#' @importFrom grid convertUnit
#' @importFrom grid rasterGrob
#' @importFrom grid grid.text
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom ggplot2 ggplotGrob
#' @importFrom gtable gtable_add_grob
#' @examples
#' # Create the figure layout
#' require(gtable)
#' Figure <- MultiPanelFigure(
#'   widths = c(20,30,30),
#'   heights = c(40,60,60),
#'   figureName = "Figure")
#' gtable_show_layout(Figure)
#' # All panels are free
#' attr(Figure,"MultiPanelFigure.panelsFree")
#'
#' # Make a simple ggplot object to fill panels
#' require(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p <- p + geom_point()
#'
#' # Fill a first panel using the ggplot object directly
#' Figure <- AddPanel(p, Figure, topPanel = 1, leftPanel = 1)
#' grid.draw(Figure)
#' # One panel is occupied
#' attr(Figure,"MultiPanelFigure.panelsFree")
#'
#' # Write the ggplot object to a temporary *.jpg, re-read and use it
#' # horizontally spanning 2 panels
#' require(jpeg)
#' tmpFile <- tempfile(fileext = ".jpg")
#' ggsave(
#'   filename = tmpFile,
#'   plot = p,
#'   width = 30,
#'   height = 40,
#'   units = "mm",
#'   dpi = 300)
#' Figure <- AddPanel(
#'     tmpFile,
#'     Figure,
#'     topPanel = 1,
#'     leftPanel = 2)
#' grid.draw(Figure)
#' # Two panels are occupied
#' attr(Figure,"MultiPanelFigure.panelsFree")
#'
#' # Write the ggplot object to a temporary *.png, re-read and use it
#' # horizontally spanning 2 panels
#' require(png)
#' tmpFile <- tempfile(fileext = ".png")
#' ggsave(
#'   filename = tmpFile,
#'   plot = p,
#'   width = 55,
#'   height = 60,
#'   units = "mm",
#'   dpi = 300)
#' Figure <- AddPanel(
#'     tmpFile,
#'     Figure,
#'     topPanel = 2,
#'     leftPanel = 1,
#'     rightPanel = 2)
#' grid.draw(Figure)
#' # Four panels are occupied
#' attr(Figure,"MultiPanelFigure.panelsFree")
#'
#' # Write the ggplot object to a temporary *.tif, re-read and use it
#' # vertically spanning 2 panels
#' require(tiff)
#' tmpFile <- tempfile(fileext = ".tiff")
#' ggsave(
#'   filename = tmpFile,
#'   plot = p,
#'   width = 30,
#'   height = 125,
#'   units = "mm",
#'   dpi = 300)
#' Figure <- AddPanel(
#'   tmpFile,
#'   Figure,
#'   topPanel = 2,
#'   bottomPanel = 3,
#'   leftPanel = 3)
#' grid.draw(Figure)
#' # Six panels are occupied
#' attr(Figure,"MultiPanelFigure.panelsFree")
AddPanel <- function(
  panel,
  figure,
  topPanel = 1,
  bottomPanel = topPanel,
  leftPanel = 1,
  rightPanel = leftPanel,
  panelLabel = head(attr(figure, "MultiPanelFigure.panelLabelsFree"),1))
{
  ####################################################
  # Check prerequisites & transform objects to grobs #
  ####################################################

  assert_has_all_attributes(
    figure,
    attrs = c(
      "MultiPanelFigure.panelsFree",
      "MultiPanelFigure.panelLabelsFree",
      "MultiPanelFigure.unit"))
  assert_is_inherited_from(figure, classes = "gtable")

  if(is.character(panel)){
    assert_is_a_string(panel)
    assert_all_are_readable_files(panel)
    if(grepl(pattern = "\\.png$", ignore.case = TRUE, x = panel)){
      panel <- readPNG(panel, info = TRUE)
      panelDim <- attr(panel, "info")[["dim"]]
      panelDpi <- attr(panel, "info")[["dpi"]]
      panelSize <- unit(x = panelDim/panelDpi,units = "inches")
      panelSize <- convertUnit(panelSize,unitTo = attr(figure , "MultiPanelFigure.unit"))
      panel <- rasterGrob(
        panel,
        x = 0, y = 1,
        width = panelSize[1],
        height = panelSize[2],
        just = c("left","top"))
    } else if(grepl(pattern = "\\.ti[f]{1,2}$", ignore.case = TRUE, x = panel)){
      panel <- readTIFF(panel, info = TRUE)
      panelDim <- c(ncol(panel), nrow(panel))
      if(!identical(
        attr(panel, "x.resolution"),
        attr(panel, "y.resolution")))
      {
        warning("Non-identical x/y resolutions.")
      }
      panelDpi <- attr(panel, "x.resolution")
      panelSize <- unit(x = panelDim/panelDpi, units = "inches")
      panelSize <- convertUnit(panelSize,unitTo = attr(figure , "MultiPanelFigure.unit"))
      panel <- rasterGrob(
        panel,
        x = 0, y = 1,
        width = panelSize[1],
        height = panelSize[2],
        just = c("left","top"))
    } else if(grepl(pattern = "\\.jp[e]*g$", ignore.case = TRUE, x = panel)){
      panel <- readJPEG(panel)
      panel <- rasterGrob(
        panel,
        x = 0, y = 1,
        width = unit(1,"npc"),
        height = unit(1, "npc"),
        just = c("left", "top"))
    } else {
      stop("unsupported file format.")
    }
  } else if(inherits(x = panel, what = "ggplot")){
    panel <- ggplotGrob(panel)
  } else {
    stop("\'panel\' is nt supported.")
  }

  rows <- nrow(attr(figure,which = "MultiPanelFigure.panelsFree"))
  columns <- ncol(attr(figure,which = "MultiPanelFigure.panelsFree"))

  assert_is_a_number(topPanel)
  assert_all_are_whole_numbers(topPanel)
  assert_all_are_in_range(x = topPanel, lower = 1, upper = rows)
  assert_all_are_true(topPanel <= bottomPanel)

  assert_is_a_number(bottomPanel)
  assert_all_are_whole_numbers(bottomPanel)
  assert_all_are_in_range(x = topPanel, lower = 1, upper = rows)
  assert_all_are_true(bottomPanel >= topPanel)

  assert_is_a_number(leftPanel)
  assert_all_are_whole_numbers(leftPanel)
  assert_all_are_in_range(x = leftPanel, lower = 1, upper = columns)
  assert_all_are_true(leftPanel <= rightPanel)

  assert_is_a_number(rightPanel)
  assert_all_are_whole_numbers(rightPanel)
  assert_all_are_in_range(x = rightPanel, lower = 1, upper = columns)
  assert_all_are_true(rightPanel >= leftPanel)

  # Are the targeted panels free?
  tmpMatrix <- matrix(TRUE, nrow = rows, ncol = columns)
  tmpMatrix[
    seq(from=topPanel, to= bottomPanel),
    seq(from=leftPanel,to=rightPanel)] <- FALSE
  tmpMatrix <- attr(figure,which = "MultiPanelFigure.panelsFree") + tmpMatrix
  if(all(tmpMatrix[
    seq(from=topPanel, to= bottomPanel),
    seq(from=leftPanel,to=rightPanel)] == 1))
  {
    attr(figure,which = "MultiPanelFigure.panelsFree")[
      seq(from=topPanel, to= bottomPanel),
      seq(from=leftPanel,to=rightPanel)] <- FALSE
  } else {
    stop("Attempt to use already filled panel. Check \'attr(figure,which = \"MultiPanelFigure.panelsFree\")\'.")
  }

  assert_is_a_string(panelLabel)

  ##############
  # Processing #
  ##############
  # Get the "real" spans (including inter-panel spaces)
  placing <- c(topPanel, bottomPanel, leftPanel, rightPanel)
  names(placing) <- c("topPanel", "bottomPanel", "leftPanel", "rightPanel")
  placing <- sapply(
    placing,
    function(pl){
      if(pl == 1){
        return(1)
      } else {
        return(pl + pl - 1)
      }
    })
  # Add panel label
  panelLabel <- grid.text(
    label = panelLabel,
    x = 0, y = 1,
    hjust = unit(0, "mm"),
    vjust = unit(1, "mm"),
    draw = FALSE)
  panel <- gTree(children=gList(panel, panelLabel))
  # Add grob to gtable
  figure <- gtable_add_grob(
    figure,
    grobs = panel,
    t = placing[["topPanel"]],
    b = placing[["bottomPanel"]],
    l = placing[["leftPanel"]],
    r = placing[["rightPanel"]])
  # Fix attributes
  attr(figure , "MultiPanelFigure.panelLabelsFree") <- tail(
    x = attr(figure , "MultiPanelFigure.panelLabelsFree"),
    n = -1)
  attr(figure , "MultiPanelFigure.panelLabelsFree") <- head(
    x = attr(figure , "MultiPanelFigure.panelLabelsFree"),
    n = sum(attr(figure , "MultiPanelFigure.panelsFree")))
  # Return
  return(figure)
}
