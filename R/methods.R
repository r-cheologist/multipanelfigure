#' Print a multi-panel figure
#'
#' Prints and object of class \code{multipanelfigure}.
#' @param x An object of class \code{multipanelfigure}.
#' @param newpage Logical. If \code{TRUE}, a new device page is opened before
#' drawing.
#' @param ... Passed from other print methods.
#' @return The input \code{x} is invisibly returned, but the method is mostly
#' invoked for the side effect of printing the plot to the current device.
#' @examples
#' p <- lattice::xyplot(dist ~ speed, cars)
#' fig <- multipanelfigure(
#'   width = 100, height = 100,
#'   rows = 1, columns = 1
#' )
#' # With no panels, printing shows the layout
#' print(fig)
#' fig <- addpanel(fig, p)
#' # After a panel is added, printing shows the plot.
#' print(fig) # shows plot
#' @importFrom assertive.properties is_empty
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @importFrom gtable gtable_show_layout
#' @export
print.multipanelfigure <- function(x, newpage = TRUE, ...)
{
  if(is_empty(x$grobs))
  {
    gtable_show_layout(x)
  } else
  {
    if(newpage)
    {
      grid.newpage()
    }
    grid.draw(x)
  }
  invisible(x) # For consistency with other print methods
}
