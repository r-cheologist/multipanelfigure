#' @title add_panel
#' @aliases addPanel
#' @description A convenience function adding graphical objects to a
#' \code{\link[gtable]{gtable}} constructed by \code{\link{multi_panel_figure}}.
#' @details Currently supported as panel-representing objects (\code{panel}) are
#' \enumerate{
#'   \item{\code{\link[ggplot2]{ggplot}} objects.}
#'   \item{\code{\link[grid]{grob}} objects.}
#'   \item{\code{\link[grid]{gList}} objects.}
#'   \item{\code{\link[lattice]{trellis.object}} objects.}
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
#' \pkg{lattice}-generated \code{\link[lattice]{trellis.object}}s are converted to
#' \code{grob}s using \code{grid.grabExpr(print(x))}, the side effects of which
#' with respect to plot formatting are not well studied.
#' @param figure Object of classes \code{multipanelfigure}/\code{\link[gtable]{gtable}}
#' as produced by \code{\link{multi_panel_figure}} and representing the figure the
#' panel is to be placed in.
#' @param panel Single \code{\link{character}} object representing path to a
#' bitmap image (\code{*.png}, \code{*.tiff}/\code{*.tif},
#' \code{*.jpg}/\code{*.jpeg}), a \code{\link[ggplot2]{ggplot}} object , a
#' \code{\link[lattice]{trellis.object}}, a \code{\link[grid]{gList}} object or
#' a \code{\link[grid]{grob}} object to be placed in a multipanel figure. See
#' 'Details'.
#' @param top_panel Single \code{\link{numeric}} indicating the row index of
#' the panel that is to be placed in the figure.
#' @param bottom_panel Single \code{\link{numeric}} indicating the lower row
#' index of the panel that is to be placed in the figure. Important for
#' definition of panel spanning (see examples).
#' @param left_panel Single \code{\link{numeric}} indicating the column index
#' of the panel that is to be placed in the figure.
#' @param right_panel Single \code{\link{numeric}} indicating the right column
#' index of the panel that is to be placed in the figure. Important for
#' definition of panel spanning (see examples).
#' @param panel_label Single \code{\link{character}} object defining the panel
#' label used for automated annotation.
#' @return Returns the \code{\link[gtable]{gtable}} object fed to it
#' (\code{figure}) with the addition of the \code{panel}.
#' @author Johannes Graumann
#' @export
#' @seealso \code{\link[gtable]{gtable}}, \code{\link{multi_panel_figure}},
#' \code{\link[tiff]{readTIFF}}, \code{\link[png]{readPNG}},
#' \code{\link[jpeg]{readJPEG}}
#' @importFrom assertive.base assert_all_are_true
#' @importFrom assertive.files assert_all_are_readable_files
#' @importFrom assertive.numbers assert_all_are_whole_numbers
#' @importFrom assertive.numbers assert_all_are_in_closed_range
#' @importFrom assertive.properties assert_has_all_attributes
#' @importFrom assertive.types assert_is_inherited_from
#' @importFrom assertive.types assert_is_a_string
#' @importFrom assertive.types assert_is_a_number
#' @importFrom png readPNG
#' @importFrom tiff readTIFF
#' @importFrom jpeg readJPEG
#' @importFrom grid unit
#' @importFrom grid convertUnit
#' @importFrom grid rasterGrob
#' @importFrom grid grid.text
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom grid grid.grabExpr
#' @importFrom ggplot2 ggplotGrob
#' @importFrom gtable gtable_add_grob
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @importFrom utils head
#' @importFrom utils tail
#' @examples
#' # Create the figure layout
#' require(grid)
#' require(gtable)
#' Figure <- multi_panel_figure(
#'   widths = c(20,30,30,30),
#'   heights = c(40,60,60,60),
#'   figure_name = "Figure")
#' gtable_show_layout(Figure)
#' # All panels are free
#' attr(Figure,"multipanelfigure.panelsFree")
#' # Clear canvas
#' grid.newpage()
#'
#' # Make a simple ggplot object to fill panels
#' require(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p <- p + geom_point()
#'
#' # Fill a first panel using the ggplot object directly
#' Figure <- add_panel(Figure, p, top_panel = 1, left_panel = 1)
#' grid.draw(Figure)
#' # One panel is occupied
#' attr(Figure,"multipanelfigure.panelsFree")
#' # Clear canvas
#' grid.newpage()
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
#'   unit = "mm",
#'   dpi = 300)
#' Figure <- add_panel(
#'     Figure,
#'     tmpFile,
#'     top_panel = 1,
#'     left_panel = 2)
#' grid.draw(Figure)
#' # Two panels are occupied
#' attr(Figure,"multipanelfigure.panelsFree")
#' # Clear canvas
#' grid.newpage()
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
#'   unit = "mm",
#'   dpi = 300)
#' Figure <- add_panel(
#'     Figure,
#'     tmpFile,
#'     top_panel = 2,
#'     left_panel = 1,
#'     right_panel = 2)
#' grid.draw(Figure)
#' # Four panels are occupied
#' attr(Figure,"multipanelfigure.panelsFree")
#' # Clear canvas
#' grid.newpage()
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
#'   unit = "mm",
#'   dpi = 300)
#' Figure <- add_panel(
#'   Figure,
#'   tmpFile,
#'   top_panel = 2,
#'   bottom_panel = 3,
#'   left_panel = 3)
#' grid.draw(Figure)
#' # Six panels are occupied
#' attr(Figure,"multipanelfigure.panelsFree")
#' # Clear canvas
#' grid.newpage()
#'
#'\dontrun{
#' # Incorporate a lattice/trellis object
#' require(lattice)
#' Depth <- equal.count(quakes$depth, number=8, overlap=.1)
#' latticePlot_trellis <- xyplot(lat ~ long | Depth, data = quakes)
#' Figure <- add_panel(
#'   Figure,
#'   latticePlot_trellis,
#'   top_panel = 3,
#'   left_panel = 1,
#'   right_panel = 2)
#' grid.draw(Figure)
#' # Eight panels are occupied
#' attr(Figure,"multipanelfigure.panelsFree")
#' # Clear canvas
#' grid.newpage()
#'
#' # Incorporate a gList object (such as produced by VennDigram)
#' require(VennDiagram)
#' venn_plot <- venn.diagram(
#'   x = list(A = 1:150, B = 121:170),
#'   filename = NULL)
#' Figure <- add_panel(
#'   Figure,
#'   venn_plot,
#'   top_panel = 4,
#'   left_panel = 1,
#'   right_panel = 2)
#' grid.draw(Figure)
#' # Ten panels are occupied
#' attr(Figure,"multipanelfigure.panelsFree")
#'
#' # Incorporate a base plot figure (produces minor margin issues)
#' require(gridGraphics)
#' plot(seq(10))
#' grid.echo()
#' grid.grab() -> base_plot
#' Figure <- add_panel(
#'   Figure,
#'   base_plot,
#'   top_panel = 4,
#'   left_panel = 3,
#'   right_panel = 4)
#' grid.draw(Figure)
#' # Twelve panels are occupied
#' attr(Figure,"multipanelfigure.panelsFree")
#'}
add_panel <- function(
  figure,
  panel,
  top_panel = 1,
  bottom_panel = top_panel,
  left_panel = 1,
  right_panel = left_panel,
  panel_label = head(attr(figure, "multipanelfigure.panellabelsfree"),1))
{
  ####################################################
  # Check prerequisites & transform objects to grobs #
  ####################################################

  figure %>%
    assert_is_multipanelfigure

  panel <- makeGrob(panel, unit_to = attr(figure , "multipanelfigure.unit"))

  rows <- nrow(attr(figure,which = "multipanelfigure.panelsFree"))
  columns <- ncol(attr(figure,which = "multipanelfigure.panelsFree"))

  top_panel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_closed_range(lower = 1, upper = rows)

  bottom_panel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_closed_range(lower = 1, upper = rows)

  top_panel %>%
    assert_all_are_in_range(lower = 1, upper = bottom_panel)

  bottom_panel %>%
    assert_all_are_in_range(lower = top_panel, upper = rows)

  left_panel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_range(lower = 1, upper = columns)

  right_panel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_range(lower = 1, upper = columns)
    assert_all_are_true(left_panel <= right_panel)

  left_panel %>%
    assert_all_are_in_closed_range(lower = 1, upper = right_panel)

  right_panel %>%
    assert_all_are_in_closed_range(lower = left_panel, upper = columns)

  # Are the targeted panels free?
  tmpMatrix <- matrix(TRUE, nrow = rows, ncol = columns)
  tmpMatrix[
    seq(from=top_panel, to= bottom_panel),
    seq(from=left_panel,to=right_panel)] <- FALSE
  tmpMatrix <- attr(figure,which = "multipanelfigure.panelsFree") + tmpMatrix
  if(all(tmpMatrix[
    seq(from=top_panel, to= bottom_panel),
    seq(from=left_panel,to=right_panel)] == 1))
  {
    attr(figure,which = "multipanelfigure.panelsFree")[
      seq(from=top_panel, to= bottom_panel),
      seq(from=left_panel,to=right_panel)] <- FALSE
  } else {
    stop("Attempt to use already filled panel. Check \'attr(figure,which = \"multipanelfigure.panelsFree\")\'.")
  }

  assert_is_a_string(panel_label)

  ##############
  # Processing #
  ##############
  # Get the "real" spans (including inter-panel spaces)
  placing <-
    c(top_panel, bottom_panel, left_panel, right_panel) %>%
    setNames(c("top_panel", "bottom_panel", "left_panel", "right_panel")) %>%
    sapply(
      function(pl){
        if(pl == 1){
          return(1)
        } else {
          return(pl + pl - 1)
        }
      })
  # Add panel label
  panel_label <- grid.text(
    label = panel_label,
    x = 0, y = 1,
    hjust = unit(0, "mm"),
    vjust = unit(1, "mm"),
    draw = FALSE)
  panel <- gTree(children=gList(panel, panel_label))
  # Add grob to gtable
  figure <- gtable_add_grob(
    figure,
    grobs = panel,
    t = placing[["top_panel"]],
    b = placing[["bottom_panel"]],
    l = placing[["left_panel"]],
    r = placing[["right_panel"]])
  # Fix attributes
  attr(figure , "multipanelfigure.panellabelsfree") <- tail(
    x = attr(figure , "multipanelfigure.panellabelsfree"),
    n = -1)
  attr(figure , "multipanelfigure.panellabelsfree") <- head(
    x = attr(figure , "multipanelfigure.panellabelsfree"),
    n = sum(attr(figure , "multipanelfigure.panelsFree")))
  # Return
  return(figure)
}

makeGrob <- function(x, unit_to){
  if(is.character(x)){
    x %>%
      assert_is_a_string() %>%
      assert_all_are_readable_files(warn_about_windows = FALSE, severity = "warning")
    if(grepl(pattern = "\\.png$", ignore.case = TRUE, x = x)){
      panel <- readPNG(x, info = TRUE)
      panelDim <- attr(panel, "info")[["dim"]]
      panelDpi <- attr(panel, "info")[["dpi"]]
      panelSize <-
        (panelDim/panelDpi) %>%
        unit(units = "inches") %>%
        convertUnit(unitTo = unit_to)
      panel <- rasterGrob(
        panel,
        x = 0, y = 1,
        width = panelSize[1],
        height = panelSize[2],
        just = c("left","top"))
    } else if(grepl(pattern = "\\.ti[f]{1,2}$", ignore.case = TRUE, x = x)){
      panel <- readTIFF(x, info = TRUE)
      panelDim <- c(ncol(panel), nrow(panel))
      if(!identical(
        attr(panel, "x.resolution"),
        attr(panel, "y.resolution")))
      {
        warning("Non-identical x/y resolutions.")
      }
      panelDpi <- attr(panel, "x.resolution")
      panelSize <-
        (panelDim/panelDpi) %>%
        unit(units = "inches") %>%
        convertUnit(unitTo = unit_to)
      panel <- rasterGrob(
        panel,
        x = 0, y = 1,
        width = panelSize[1],
        height = panelSize[2],
        just = c("left","top"))
    } else if(grepl(pattern = "\\.jp[e]*g$", ignore.case = TRUE, x = x)){
      panel <- readJPEG(x)
      panel <- rasterGrob(
        panel,
        x = 0, y = 1,
        width = unit(1,"npc"),
        height = unit(1, "npc"),
        just = c("left", "top"))
    } else {
      stop("unsupported file format.")
    }
  } else if(inherits(x = x, what = "ggplot")){
    panel <- ggplotGrob(x)
  } else if(inherits(x = x, what = "gList")){
    panel <- x
  } else if(inherits(x = x, what = "grob")){
    panel <- x
  } else if (inherits(x = x, what = "trellis")){
    panel <- grid.grabExpr(print(x))
  } else {
    stop("Class of \'panel\' is not supported.")
  }
  return(panel)
}

#' @export
addPanel <- function( figure, ... ){
  .Deprecated(
    new = "add_panel",
    package = "multipanelfigure")
  paramList <- list ( ... )
  if("topPanel" %in% names(paramList)){
    top_panel = paramList[["topPanel"]]
  } else {
    top_panel = 1
  }
  if("bottomPanel" %in% names(paramList)){
    bottom_panel = paramList[["bottomPanel"]]
  } else {
    bottom_panel = top_panel
  }
  if("leftPanel" %in% names(paramList)){
    left_panel = paramList[["leftPanel"]]
  } else {
    left_panel = 1
  }
  if("rightPanel" %in% names(paramList)){
    right_panel = paramList[["rightPanel"]]
  } else {
    rightPanel = left_panel
  }
  if("panelLabel" %in% names(paramList)){
    panel_label = paramList[["panelPanel"]]
  } else {
    panel_label = head(
      attr(
        figure,
        "multipanelfigure.panellabelsfree"),
      1)
  }
  add_panel(
    figure = figure,
    top_panel = top_panel,
    bottom_panel = bottom_panel,
    left_panel = left_panel,
    right_panel = right_panel,
    panel_label = panel_label,
    ... )
}