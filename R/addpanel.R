#' @title addpanel
#' @aliases addpanel
#' @description A convenience function adding graphical objects to a
#' \code{\link[gtable]{gtable}} constructed by \code{\link{multipanelfigure}}.
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
#' as produced by \code{\link{multipanelfigure}} and representing the figure the
#' panel is to be placed in.
#' @param panel Single \code{\link{character}} object representing path to a
#' bitmap image (\code{*.png}, \code{*.tiff}/\code{*.tif},
#' \code{*.jpg}/\code{*.jpeg}), a \code{\link[ggplot2]{ggplot}} object , a
#' \code{\link[lattice]{trellis.object}}, a \code{\link[grid]{gList}} object or
#' a \code{\link[grid]{grob}} object to be placed in a multipanel figure. See
#' 'Details'.
#' @param topPanel Single \code{\link[base]{numeric}} indicating the row index of
#' the panel that is to be placed in the figure.
#' @param bottomPanel Single \code{\link[base]{numeric}} indicating the lower row
#' index of the panel that is to be placed in the figure. Important for
#' definition of panel spanning (see examples).
#' @param leftPanel Single \code{\link[base]{numeric}} indicating the column index
#' of the panel that is to be placed in the figure.
#' @param rightPanel Single \code{\link[base]{numeric}} indicating the right column
#' index of the panel that is to be placed in the figure. Important for
#' definition of panel spanning (see examples).
#' @param Single \code{\link[base]{character}} used for the panel label, or
#' \code{NULL} to automatically generate the label.
#' @return Returns the \code{\link[gtable]{gtable}} object fed to it
#' (\code{figure}) with the addition of the \code{panel}.
#' @author Johannes Graumann
#' @export
#' @seealso \code{\link[gtable]{gtable}}, \code{\link{multipanelfigure}},
#' \code{\link[tiff]{readTIFF}}, \code{\link[png]{readPNG}},
#' \code{\link[jpeg]{readJPEG}}
#' @importFrom assertive.base assert_all_are_true
#' @importFrom assertive.base use_first
#' @importFrom assertive.base coerce_to
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
#' # First, some setup; see below for the use of addpanel
#'
#' # Make a grid grob
#' a_grob <- grid::linesGrob(arrow = grid::arrow())
#' # Make a simple ggplot object to fill panels
#' ggp <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
#'   ggplot2::geom_point()
#'
#' # Save the plot to JPEG, PNG, and TIFF file for later
#' tmpFileJpeg <- tempfile(fileext = ".jpg")
#' ggplot2::ggsave(
#'   filename = tmpFileJpeg,
#'   plot = ggp + ggplot2::ggtitle("a jpeg"),
#'   width = 60, height = 40,
#'   units = "mm", dpi = 300)
#' tmpFilePng <- tempfile(fileext = ".png")
#' ggplot2::ggsave(
#'   filename = tmpFilePng,
#'   plot = ggp + ggplot2::ggtitle("a png"),
#'   width = 55, height = 60,
#'   units = "mm", dpi = 300)
#' tmpFileTiff <- tempfile(fileext = ".tiff")
#' ggplot2::ggsave(
#'   filename = tmpFileTiff,
#'   plot = ggp + ggplot2::ggtitle("a tiff"),
#'   width = 60, height = 125,
#'   units = "mm", dpi = 300)
#'
#' # addpanel works best with pipes
#' `%<>%` <- magrittr::`%<>%`
#'
#' # ------------------------------------------------------------------
#'
#' # Now, the actual example!
#' # Create the figure layout
#' (Figure <- multipanelfigure(
#'   widths = c(20,30,60),
#'   heights = c(40,60,60,60)))
#'
#' # Fill the top-left panel using the grob object directly
#' (Figure %<>% addpanel(a_grob))
#'
#' # Add the ggplot object directly to the top row, second column.
#' (Figure %<>% addpanel(ggp, leftPanel = 2))
#'
#' # JPEG, PNG, and TIFF images are added by passing the path to their file.
#' # Add the JPEG to the top row, third column
#' (Figure %<>% addpanel(tmpFileJpeg, leftPanel = 3))
#'
#' # Add the PNG to the second row, first and second column
#' (Figure %<>% addpanel(tmpFilePng,
#'     topPanel = 2, leftPanel = 1, rightPanel = 2))
#'
#' # Add the TIFF to the second and third rows, third column
#' (Figure %<>% addpanel(tmpFileTiff,
#'     topPanel = 2, bottomPanel = 3, leftPanel = 3))
#'\donttest{
#' # lattice/trellis plot objects are also added directly
#' Depth <- lattice::equal.count(quakes$depth, number=8, overlap=0.1)
#' latticePlot_trellis <- lattice::xyplot(lat ~ long | Depth, data = quakes)
#' # Add the lattice plot to the third row, first and second column
#' (Figure %<>% addpanel(latticePlot_trellis,
#'   topPanel = 3, leftPanel = 1, rightPanel = 2))
#'
#' # Incorporate a gList object (such as produced by VennDigram)
#' require(VennDiagram)
#' venn_plot <- VennDiagram::venn.diagram(
#'   x = list(A = 1:150, B = 121:170), filename = NULL)
#' # Add the venn diagram to the fourth row, first and second columns
#' (Figure %<>% addpanel(
#'   venn_plot,
#'   topPanel = 4, leftPanel = 1, rightPanel = 2))
#'
#' # Incorporate a base plot figure (produces minor margin issues)
#' base_plot <- capturebaseplot(
#'  heatmap(
#'    cor(USJudgeRatings), Rowv = FALSE, symm = TRUE, col = topo.colors(16),
#'    distfun = function(c) as.dist(1 - c), keep.dendro = TRUE))
#' # Add the heatmap to the fourth row, third column
#' (Figure %<>% addpanel(
#'   base_plot,
#'   topPanel = 4, leftPanel = 3))
#'}
addpanel <- function(
  figure,
  panel,
  topPanel = 1,
  bottomPanel = topPanel,
  leftPanel = 1,
  rightPanel = leftPanel,
  label = NULL)
{
  ####################################################
  # Check prerequisites & transform objects to grobs #
  ####################################################

  figure %>%
    assert_is_multipanelfigure

  panel <- makeGrob(panel, unitTo = attr(figure , "multipanelfigure.units"))

  rows <- nrow(attr(figure,which = "multipanelfigure.panelsFree"))
  columns <- ncol(attr(figure,which = "multipanelfigure.panelsFree"))

  topPanel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_closed_range(lower = 1, upper = rows)

  bottomPanel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_closed_range(lower = 1, upper = rows)

  topPanel %>%
    assert_all_are_in_range(lower = 1, upper = bottomPanel)

  bottomPanel %>%
    assert_all_are_in_range(lower = topPanel, upper = rows)

  leftPanel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_range(lower = 1, upper = columns)

  rightPanel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_range(lower = 1, upper = columns)
    assert_all_are_true(leftPanel <= rightPanel)

  leftPanel %>%
    assert_all_are_in_closed_range(lower = 1, upper = rightPanel)

  rightPanel %>%
    assert_all_are_in_closed_range(lower = leftPanel, upper = columns)

  # Are the targeted panels free?
  tmpMatrix <- matrix(TRUE, nrow = rows, ncol = columns)
  tmpMatrix[
    seq(from=topPanel, to= bottomPanel),
    seq(from=leftPanel,to=rightPanel)] <- FALSE
  tmpMatrix <- attr(figure,which = "multipanelfigure.panelsFree") + tmpMatrix
  if(all(tmpMatrix[
    seq(from=topPanel, to= bottomPanel),
    seq(from=leftPanel,to=rightPanel)] == 1))
  {
    attr(figure,which = "multipanelfigure.panelsFree")[
      seq(from=topPanel, to= bottomPanel),
      seq(from=leftPanel,to=rightPanel)] <- FALSE
  } else {
    stop("Attempt to use already filled panel. Check \'attr(figure,which = \"multipanelfigure.panelsFree\")\'.")
  }

  # Check/fix panel label
  label <- if(is.null(label))
  {
    nextLabel(figure)
  } else
  {
    use_first(coerce_to(label, "character"))
  }

  ##############
  # Processing #
  ##############
  # Get the "real" spans (including inter-panel spaces)
  placing <-
    c(topPanel, bottomPanel, leftPanel, rightPanel) %>%
    setNames(c("topPanel", "bottomPanel", "leftPanel", "rightPanel")) %>%
    sapply(
      function(pl){
        if(pl == 1){
          return(1)
        } else {
          return(pl + pl - 1)
        }
      })
  # Add panel label
  panelLabel <- grid.text(
    label = label,
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
  # Return
  return(figure)
}

makeGrob <- function(x, unitTo){
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
        convertUnit(unitTo = unitTo)
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
        convertUnit(unitTo = unitTo)
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