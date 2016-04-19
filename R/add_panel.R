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
#' @param label Single \code{\link{character}} object defining the panel
#' label used for automated annotation.
#' @return Returns the \code{\link[gtable]{gtable}} object fed to it
#' (\code{figure}) with the addition of the \code{panel}.
#' @author Johannes Graumann
#' @export
#' @seealso \code{\link[gtable]{gtable}}, \code{\link{multi_panel_figure}},
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
#' # First, some setup; see below for the use of add_panel
#'
#' # Make a grid grob
#' a_grob <- grid::linesGrob(arrow = grid::arrow())
#' # Make a simple ggplot object to fill panels
#' a_ggplot <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
#'   ggplot2::geom_point()
#'
#' # Save the plot to JPEG, PNG, and TIFF file for later
#' tmp_file_jpeg <- tempfile(fileext = ".jpg")
#' ggplot2::ggsave(
#'   filename = tmp_file_jpeg,
#'   plot = a_ggplot + ggplot2::ggtitle("a jpeg"),
#'   width = 60, height = 40,
#'   units = "mm", dpi = 300)
#' tmp_file_png <- tempfile(fileext = ".png")
#' ggplot2::ggsave(
#'   filename = tmp_file_png,
#'   plot = a_ggplot + ggplot2::ggtitle("a png"),
#'   width = 55, height = 60,
#'   units = "mm", dpi = 300)
#' tmp_file_tiff <- tempfile(fileext = ".tiff")
#' ggplot2::ggsave(
#'   filename = tmp_file_tiff,
#'   plot = a_ggplot + ggplot2::ggtitle("a tiff"),
#'   width = 60, height = 125,
#'   units = "mm", dpi = 300)
#'
#' # add_panel works best with pipes
#' `%<>%` <- magrittr::`%<>%`
#'
#' # ------------------------------------------------------------------
#'
#' # Now, the actual example!
#' # Create the figure layout
#' (figure <- multi_panel_figure(
#'   widths = c(20,30,60),
#'   heights = c(40,60,60,60)))
#'
#' # Fill the top-left panel using the grob object directly
#' (figure %<>% add_panel(a_grob))
#'
#' # Add the ggplot object directly to the top row, second column.
#' (figure %<>% add_panel(a_ggplot, left_panel = 2))
#'
#' # JPEG, PNG, and TIFF images are added by passing the path to their file.
#' # Add the JPEG to the top row, third column
#' (figure %<>% add_panel(tmp_file_jpeg, left_panel = 3))
#'
#' # Add the PNG to the second row, first and second column
#' (figure %<>% add_panel(
#'   tmp_file_png,
#'   top_panel = 2, left_panel = 1, right_panel = 2))
#'
#' # Add the TIFF to the second and third rows, third column
#' (figure %<>% add_panel(
#'   tmp_file_tiff,
#'   top_panel = 2, bottom_panel = 3, left_panel = 3))
#'
#' # lattice/trellis plot objects are also added directly
#' Depth <- lattice::equal.count(quakes$depth, number=8, overlap=0.1)
#' a_lattice_plot <- lattice::xyplot(lat ~ long | Depth, data = quakes)
#' # Add the lattice plot to the third row, first and second column
#' (figure %<>% add_panel(
#'   a_lattice_plot,
#'   top_panel = 3, left_panel = 1, right_panel = 2))
#'
#' # Incorporate a gList object (such as produced by VennDigram)
#' if(requireNamespace("VennDiagram"))
#' {
#'   a_venn_plot <- VennDiagram::venn.diagram(
#'   x = list(A = 1:150, B = 121:170), filename = NULL)
#' # Add the Venn diagram to the fourth row, first and second columns
#' (figure %<>% add_panel(
#'   a_venn_plot,
#'   top_panel = 4, left_panel = 1, right_panel = 2))
#' }
#'
#' # Incorporate a base plot figure (produces minor margin issues)
#' a_base_plot <- capture_base_plot(
#'  heatmap(
#'    cor(USJudgeRatings), Rowv = FALSE, symm = TRUE, col = topo.colors(16),
#'    distfun = function(c) as.dist(1 - c), keep.dendro = TRUE))
#' # Add the heatmap to the fourth row, third column
#' (figure %<>% add_panel(
#'   a_base_plot,
#'   top_panel = 4, left_panel = 3))
add_panel <- function(
  figure,
  panel,
  top_panel = 1,
  bottom_panel = top_panel,
  left_panel = 1,
  right_panel = left_panel,
  label = NULL)
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
    label = label,
    x = 0, y = 1,
    hjust = unit(0, "mm"),
    vjust = unit(1, "mm"),
    draw = FALSE)
  panel <- gTree(children = gList(panel, panel_label))
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
    x = attr(figure, "multipanelfigure.panellabelsfree"),
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
    right_panel = left_panel
  }
  add_panel(
    figure = figure,
    top_panel = top_panel,
    bottom_panel = bottom_panel,
    left_panel = left_panel,
    right_panel = right_panel,
    label = NULL, # for ease of maintenance, only support auto-labelling in deprecated case
    ... )
}
