#' @title Convenient Access to \code{grob} Dimensions
#' @aliases simpleGrobHeight
#' @aliases simpleGrobWidth
#' @usage simpleGrobWidth(grob, unitTo = "mm")
#' simpleGrobHeight(grob, unitTo = "mm")
#' @description Convenience functions extracting dimensions from
#' \code{\link{grob}} objects.
#' @param unitTo A single \code{\link{character}} string representing a valid
#' \pkg{grid}-\code{\link[grid]{unit}}.
#' @param grob A \code{\link[grid]{grob}} object for which dimensions are to be
#' retrieved.
#' @return Single \code{\link{numeric}} objects are returned.
#' @author Johannes Graumann
#' @seealso \code{\link{MultiPanelFigure}}
#' @importFrom assertive assert_is_inherited_from
#' @importFrom assertive assert_is_a_string
#' @importFrom grid convertUnit
#' @importFrom grid heightDetails
#' @importFrom grid widthDetails
#' @examples
#' library(grid)
#' testCircle <- circleGrob(x = 15, y = 30, r = 15, default.unit = "mm")
#' grid.newpage()
#' grid.draw(testCircle)
#' simpleGrobHeight(testCircle)
#' simpleGrobWidth(testCircle)
#'
#' simpleGrobHeight(testCircle, unitTo = "in")
#' simpleGrobWidth(testCircle, unitTo = "cm")
#' @export
simpleGrobWidth <- function(grob, unitTo = "mm"){
  # Check prerequisites
  assert_is_inherited_from(x = grob, classes = "grob")
  assert_is_a_string(unitTo)
  grid:::valid.units(units = unitTo)

  # Process
  convertUnit(
    widthDetails(grob),
    unitTo = unitTo,
    valueOnly = TRUE)
}

#' @export
simpleGrobHeight <- function(grob, unitTo = "mm"){
  # Check prerequisites
  assert_is_inherited_from(x = grob, classes = "grob")
  assert_is_a_string(unitTo)
  grid:::valid.units(units = unitTo)

  # Process
  convertUnit(
    heightDetails(grob),
    unitTo = unitTo,
    valueOnly = TRUE)

}