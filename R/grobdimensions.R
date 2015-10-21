#' @title Convenient Access to \code{grob} Dimensions
#' @aliases simplegrobheight
#' @aliases simplegrobwidth
#' @usage simplegrobwidth(grob, unitTo = "mm")
#' simplegrobheight(grob, unitTo = "mm")
#' @description Convenience functions extracting dimensions from
#' \code{\link{grob}} objects.
#' @param unitTo A single \code{\link{character}} string representing a valid
#' \pkg{grid}-\code{\link[grid]{unit}}.
#' @param grob A \code{\link[grid]{grob}} object for which dimensions are to be
#' retrieved.
#' @return Single \code{\link{numeric}} objects are returned.
#' @author Johannes Graumann
#' @seealso \code{\link{multipanelfigure}}
#' @importFrom assertive.types assert_is_inherited_from
#' @importFrom assertive.types assert_is_a_string
#' @importFrom grid convertUnit
#' @importFrom grid heightDetails
#' @importFrom grid widthDetails
#' @importFrom magrittr %>%
#' @importFrom magrittr %T>%
#' @examples
#' library(grid)
#' testCircle <- circleGrob(x = 15, y = 30, r = 15, default.unit = "mm")
#' grid.newpage()
#' grid.draw(testCircle)
#' simplegrobheight(testCircle)
#' simplegrobwidth(testCircle)
#'
#' simplegrobheight(testCircle, unitTo = "in")
#' simplegrobwidth(testCircle, unitTo = "cm")
#' @export
simplegrobwidth <- function(grob, unitTo = "mm"){
  # Check prerequisites
  grob %T>%
    assert_is_inherited_from(classes = "grob")
  unitTo %T>%
    assert_is_a_string() %T>%
    grid:::valid.units()

  # Process
  convertUnit(
    widthDetails(grob),
    unitTo = unitTo,
    valueOnly = TRUE)
}

#' @export
simplegrobheight <- function(grob, unitTo = "mm"){
  # Check prerequisites
  grob %T>%
    assert_is_inherited_from(classes = "grob")
  unitTo %T>%
    assert_is_a_string() %T>%
    grid:::valid.units()

  # Process
  convertUnit(
    heightDetails(grob),
    unitTo = unitTo,
    valueOnly = TRUE)

}
