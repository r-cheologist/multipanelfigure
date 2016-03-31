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
#' @examples
#' # Get dimensions of a grid grob
#' testCircle <- grid::circleGrob(x = 15, y = 30, r = 15, default.unit = "mm")
#' simplegrobheight(testCircle)
#' simplegrobwidth(testCircle)
#'
#' # Use the unitTo arg to convert units
#' simplegrobheight(testCircle, unitTo = "in")
#' simplegrobwidth(testCircle, unitTo = "cm")
#'
#' # Get dimensions of a multi-panel figure
#' Figure <- multipanelfigure(width = 55, height = 55, rows = 2, columns = 2)
#' simplegrobheight(Figure)
#' simplegrobwidth(Figure)
#'
#' # ggsave defaults to measuring dimensions in inches
#' usedUnits <- "in"
#' width <- simplegrobwidth(Figure, unitTo = usedUnits)
#' height <- simplegrobheight(Figure, unitTo = usedUnits)
#' tmpFile <- tempfile(fileext = ".png")
#' ggplot2::ggsave(tmpFile, Figure, width = width, height = height)
#' \donttest{ # Not testing due to use of external software
#' utils::browseURL(tmpFile)
#' }
#' @export
simplegrobwidth <- function(grob, unitTo = "mm"){
  # Check prerequisites
  grob %>%
    assert_is_inherited_from(classes = "grob")
  unitTo %>%
    assert_is_a_string() %>%
    assert_is_a_valid_unit_type()

  # Process
  convertUnit(
    widthDetails(grob),
    unitTo = unitTo,
    valueOnly = TRUE)
}

#' @export
simplegrobheight <- function(grob, unitTo = "mm"){
  # Check prerequisites
  grob %>%
    assert_is_inherited_from(classes = "grob")
  unitTo %>%
    assert_is_a_string() %>%
    assert_is_a_valid_unit_type()

  # Process
  convertUnit(
    heightDetails(grob),
    unitTo = unitTo,
    valueOnly = TRUE)

}
