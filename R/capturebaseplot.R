#' Capture a base plot
#'
#' Capture a plot drawn using base graphics as a grid \code{grob}.
#' @param expr A expression that draws a plot using base graphics.
#' @return An object of class \code{gTree}.
#' @note A side effect of this function is that plots get drawn twice: once as
#' a base plot, and secondly as a grid plot.
#' @examples
#' p <- capturebaseplot(hist(rnorm(1000), seq(-4, 4, 0.2)))
#' grid::grid.draw(p)
#' # If the plot takes multiple lines to draw, then wrap the code in braces.
#' p2 <- capturebaseplot({
#'   par(las = 1)
#'   plot(1:5)
#'   title("One to five")
#' })
#' grid::grid.draw(p2)
#' @importFrom gridGraphics grid.echo
#' @importFrom grid grid.grab
#' @export
capturebaseplot <- function(expr)
{
  force(expr)
  gridGraphics::grid.echo()
  grid::grid.grab()
}
