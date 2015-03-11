#' @title MultiPanelFigure
#' @description A convenience function building \code{\link[gtable]{gtable}}-based
#' infrastructure for the assembly of multipanel figures.
#' @details The \code{\link[gtable]{gtable}} may be constructed in two ways:
#' \enumerate{
#'   \item{Based on explicit width/heigth definitions for individual panels
#'     handed to it via \code{widths} and \code{heights}.}
#'   \item{Based on total figure/\code{\link[gtable]{gtable}} dimensions given by
#'     \code{width} and \code{height} together with the number of \code{columns}
#'     and \code{rows} requested.}}
#' The function automatically inserts whitespace of width
#' \code{interPanelSpacing} between panels, which has to be considered for the
#' total dimesnions of the resulting \code{\link[gtable]{gtable}}. Width of the
#' \code{\link[gtable]{gtable}} in the former case, for example may be calculated
#' \deqn{W[total] = sum(widths) + (length(widths) - 1) * interPanelSpacing}
#' while width of resulting panels in the latter table construction approach may
#' be calculated
#' \deqn{W[panel] = (width - (columns - 1) * interPanelSpacing)/columns}
#'
#' The two approaches to \code{\link[gtable]{gtable}} construction require mutually
#' exclusive parameter sets:
#' \describe{
#'   \item{Individual definition of panel dimensions:}{Requires \code{widths} and
#'     \code{heights}. Excludes the use of \code{width}, \code{columns},
#'     \code{heights} and \code{rows}.}
#'   \item{Definition of global \code{\link[gtable]{gtable}}/figure dimensions:}{
#'     Requires\code{width}, \code{columns}, \code{heights} and \code{rows}.
#'     Excludes the use of \code{widths} and \code{heights}.}}
#' @param width Single \code{link{numeric}} defining the width of the resulting
#' \code{\link[gtable]{gtable}} (units depending on \code{units}). See 'Details' for
#' dependend and interfering parameters.
#' @param widths \code{\link{vector}} of \code{\link{numeric}}s defining the
#' actual widths of panels/columns in the resulting \code{\link[gtable]{gtable}} (units
#' depending on \code{units}). See 'Details' for dependend and
#' interfering parameters.
#' @param columns Single \code{\link{numeric}} defining the number of columns in
#' the resulting \code{\link[gtable]{gtable}}. See 'Details' for dependend and
#' interfering parameters.
#' @param height Single \code{link{numeric}} defining the height of the resulting
#' \code{\link[gtable]{gtable}} (units depending on \code{units}). See 'Details' for
#' dependend and interfering parameters.
#' @param heights \code{\link{vector}} of \code{\link{numeric}}s defining the
#' actual heights of panels/rows in the resulting \code{\link[gtable]{gtable}} (units
#' depending on \code{units}). See 'Details' for dependend and
#' interfering parameters.
#' @param rows Single \code{\link{numeric}} defining the number of rows in
#' the resulting \code{\link[gtable]{gtable}}. See 'Details' for dependend and
#' interfering parameters.
#' @param interPanelSpacing The amount of white space automatically inserted
#' between panels. Defaults to \code{5 mm} unless explicitly given, in which
#' case the value depents on the \code{units} parameter.
#' @param units Single \code{\link{character}} object defining the units of all
#' dimensions defined. Must satisfy \code{grid:::valid.units}.
#' @param figureName Single \code{\link{character}} object defining the name of
#' the resulting \code{\link[gtable]{gtable}}.
#' @param panelLabels A \code{\link{character}} \code{\link{vector}} defining
#' the panel labels used for automated annotation. \code{\link{length}} must be
#' larger or equal to the number of panels defined. Will be used sequentially.
#' @return Returns a \code{\link[gtable]{gtable}} object with the following additional
#' attribute:
#' \describe{
#'   \item{\code{MultiPanelFigure.panelsFree}:}{A \code{\link{logical}}
#'     \code{\link{matrix}} with the dimensions of the \code{\link[gtable]{gtable}}
#'     indicating occupancy of the panels in the table.}
#'   \item{\code{MultiPanelFigure.panelLabelsFree}:}{A \code{\link{character}}
#'     \code{\link{vector}} indicative of the \code{panelLabels} still available.}
#'   \item{\code{MultiPanelFigure.units}:}{A single \code{\link{character}}
#'     object storing the corresponding value given during object creation.}}
#' @author Johannes Graumann
#' @importFrom assertive assert_is_a_bool
#' @importFrom assertive assert_is_numeric
#' @export
#' @seealso \code{\link[gtable]{gtable}}, \code{\link{AddPanel}}
#' @keywords hplot utilities
#' @importFrom assertive assert_is_a_number
#' @importFrom assertive assert_all_are_positive
#' @importFrom assertive assert_is_a_string
#' @importFrom assertive assert_is_null
#' @importFrom assertive assert_is_not_null
#' @importFrom assertive assert_all_are_whole_numbers
#' @importFrom assertive assert_all_are_in_range
#' @importFrom assertive assert_is_numeric
#' @importFrom assertive assert_is_character
#' @importFrom assertive assert_all_are_true
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_col_space
#' @importFrom gtable gtable_add_row_space
#' @examples
#' library(gtable)
#'
#' Figure1 <- MultiPanelFigure(
#'    width = 100,
#'    columns = 4,
#'    height = 100,
#'    rows = 6,
#'    figureName = "Figure1")
#' gtable_show_layout(Figure1)
#'
#' Figure2 <- MultiPanelFigure(
#'    widths = c(20,30),
#'    heights = c(40,60),
#'    figureName = "Figure2")
#' gtable_show_layout(Figure2)
MultiPanelFigure <- function(
  width = NULL,
  widths = NULL,
  columns = NULL,
  height = NULL,
  heights = NULL,
  rows = NULL,
  interPanelSpacing = NaN,
  units = "mm",
  figureName = "FigureX",
  panelLabels = LETTERS)
{
  #######################
  # Check Prerequisites #
  #######################
  assert_is_a_number(interPanelSpacing)
  if(is.nan(interPanelSpacing)){
    interPanelSpacing <- unit(x = 5, units = "mm")
    interPanelSpacing <- convertUnit(interPanelSpacing,unitTo = units)
    interPanelSpacing <- as.numeric(interPanelSpacing)
  }
  assert_all_are_positive(interPanelSpacing)

  assert_is_a_string(figureName)

  if(!is.null(width)){
    assert_is_null(widths)
    assert_is_not_null(columns)
    assert_is_a_number(width)
    assert_all_are_positive(width)
    assert_is_a_number(columns)
    assert_all_are_whole_numbers(columns)
    assert_all_are_in_range(x = columns, lower = 1, upper = Inf)
    widths <- rep(
      x = (width - interPanelSpacing * (columns - 1))/columns,
      times = columns)
  } else {
    assert_is_not_null(widths)
    assert_is_null(columns)
    assert_is_numeric(widths)
    assert_all_are_positive(widths)
    columns <- length(widths)
    width <- sum(widths) + interPanelSpacing * (columns - 1)
  }

  if(!is.null(height)){
    assert_is_null(heights)
    assert_is_not_null(rows)
    assert_is_a_number(height)
    assert_all_are_positive(height)
    assert_is_a_number(rows)
    assert_all_are_whole_numbers(rows)
    assert_all_are_in_range(x = rows, lower = 1, upper = Inf)
    heights <- rep(
      x = (height - interPanelSpacing * (rows - 1))/rows,
      times = rows)
  } else {
    assert_is_not_null(heights)
    assert_is_null(rows)
    assert_is_numeric(heights)
    assert_all_are_positive(heights)
    rows <- length(heights)
    height <- sum(heights) + interPanelSpacing * (rows - 1)
  }

  assert_is_a_string(units)
  grid:::valid.units(units = units)

  assert_is_character(panelLabels)
  assert_all_are_true(length(columns) * length(rows) <= length(panelLabels))

  ####################
  # Construct gtable #
  ####################
  # Basic layout
  tmpGTable <- gtable(
    widths = unit(x = widths, units = units),
    heights = unit(x = heights, units = units),
    name = figureName)
  # add interpanel space
  tmpGTable <- gtable_add_col_space(
    x = tmpGTable,
    width = unit(
      x = interPanelSpacing,
      units = units))
  tmpGTable <- gtable_add_row_space(
    x = tmpGTable,
    height = unit(
      x = interPanelSpacing,
      units = units))
  ##########################
  # Prep and return output #
  ##########################
  MultiPanelFigure <- list(
    panelsFree = matrix(
      data = TRUE,
      ncol = columns,
      nrow = rows),
    panelLabelsFree = panelLabels[seq(columns * rows)],
    units = units)
  attributes(tmpGTable) <- c(attributes(tmpGTable),"MultiPanelFigure"=MultiPanelFigure)
  return(tmpGTable)
}
