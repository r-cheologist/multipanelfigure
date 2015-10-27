#' Save a multi-panel figure frame
#'
#' Saves a multi-panel figure frame to a file, using sensible defaults.
#' @param filename A string specifying the filename, or a path to the file.
#' @param figureFrame An object created by \code{multipanelfigure}.
#' @param device Either a device function, or a string naming the device.
#' Automatically determined from the \code{filename}.
#' @param path A string naming the directoy to store the output file (for use
#' when \code{filename} is just the name of the file.)
#' @param dpi The resolution of the output, in dots per inch.  (Only used by
#' bitmap devices.)
#' @param ... Passed to the device function.
#' @return The filename is invisibly returned, but the function is mostly
#' invoked for the side-effect of writing a plot to file.
#' @references This function is derived from \code{\link[ggplot2]{ggsave}}.
#' @importFrom assertive.properties assert_has_all_attributes
#' @importFrom assertive.types assert_is_inherited_from
#' @importFrom digest digest
#' @importFrom grDevices dev.off
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom magrittr %>%
#' @importFrom pathological get_extension
#' @importFrom utils capture.output
#' @export
mpfsave <- function(filename = default_name(figureFrame), figureFrame,
  device = pathological::get_extension(filename), path = NULL, dpi = 300, ...)
{
  figureFrame %>%
    assert_is_multipanelfigure

  # This code from ggplot2::ggsave
  eps <- ps <- function(..., width, height) grDevices::postscript(...,
      width = width, height = height, onefile = FALSE, horizontal = FALSE,
      paper = "special")
  tex <- function(..., width, height) grDevices::pictex(...,
      width = width, height = height)
  pdf <- function(..., version = "1.4") grDevices::pdf(...,
      version = version)
  svg <- function(...) grDevices::svg(...)
  wmf <- function(..., width, height) grDevices::win.metafile(...,
      width = width, height = height)
  emf <- function(..., width, height) grDevices::win.metafile(...,
      width = width, height = height)
  png <- function(..., width, height) grDevices::png(..., width = width,
      height = height, res = dpi, units = "in")
  jpg <- jpeg <- function(..., width, height) grDevices::jpeg(...,
      width = width, height = height, res = dpi, units = "in")
  bmp <- function(..., width, height) grDevices::bmp(..., width = width,
      height = height, res = dpi, units = "in")
  tiff <- function(..., width, height) grDevices::tiff(...,
      width = width, height = height, res = dpi, units = "in")
  if(is.character(device))
  {
    device <- get(device)
  }
  default_name <- function(figureFrame) {
      paste(digest::digest(figureFrame), ".pdf", sep = "")
  }
  # End ggplot2::ggsave code

  # Need to convert to inches because some devices (e.g. win.metafile) only
  # allow you to specify dimensions in that unit
  width <- simplegrobwidth(figureFrame, "inches")
  height <- simplegrobheight(figureFrame, "inches")

  # This code from ggplot2::ggsave
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }


  message(
    "Saving a figure to ", filename,
    " with dimensions width = ", format(width, digits = 3),
    " inches; height = ", format(height, digits = 3), " inches"
  )


  device(file = filename, width = width, height = height, ...)
  on.exit(capture.output(dev.off()))
  grid::grid.newpage()
  grid::grid.draw(figureFrame)
  invisible(filename)
  # End ggplot2::ggsave code
}
