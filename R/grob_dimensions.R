#' @title Convenient Access to \code{grob} Dimensions
#' @name grob_dimensions
#' @aliases simplegrobheight simplegrobwidth simple_grob_height simple_grob_width
#' @usage simple_grob_width(grob, unitTo = "mm")
#' simple_grob_height(grob, unitTo = "mm")
#' @description Convenience functions extracting dimensions from
#' \code{\link{grob}} objects.
#' @param unitTo A single \code{\link{character}} string representing a valid
#' \pkg{grid}-\code{\link[grid]{unit}}.
#' @param grob A \code{\link[grid]{grob}} object for which dimensions are to be
#' retrieved.
#' @return Single \code{\link{numeric}} objects are returned.
#' @author Johannes Graumann
#' @seealso \code{\link{multi_panel_figure}}
#' @importFrom assertive.types assert_is_inherited_from
#' @importFrom assertive.types assert_is_a_string
#' @importFrom grid convertUnit
#' @importFrom grid heightDetails
#' @importFrom grid widthDetails
#' @importFrom magrittr %>%
#' @examples
#' library(grid)
#' testCircle <- circleGrob(x = 15, y = 30, r = 15, default.unit = "mm")
#' grid.newpage()
#' grid.draw(testCircle)
#' simple_grob_height(testCircle)
#' simple_grob_width(testCircle)
#'
#' simple_grob_height(testCircle, unitTo = "in")
#' simple_grob_width(testCircle, unitTo = "cm")
#' @export
simple_grob_width <- function(grob, unitTo = "mm"){
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
simple_grob_height <- function(grob, unitTo = "mm"){
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

#' @export
simplegrobheight <- function( ... ){
  .Deprecated(
    new = "simple_grob_height",
    package = "multipanelfigure")
  simple_grob_height( ... )
}

#' @export
simplegrobwidth <- function( ... ){
  .Deprecated(
    new = "simple_grob_width",
    package = "multipanelfigure")
  simple_grob_width( ... )
}