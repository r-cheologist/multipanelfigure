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
  interPanelSpacing = 5,
  unit = "mm",
  figureName = "FigureX",
  panelNames = LETTERS)
{
  #######################
  # Check Prerequisites #
  #######################
  assert_is_a_number(interPanelSpacing)
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

  assert_is_a_string(unit)
  grid:::valid.units(units = unit)

  assert_is_character(panelNames)
  assert_all_are_true(length(columns) * length(rows) <= length(panelNames))

  ####################
  # Construct gtable #
  ####################
  # Basic layout
  tmpGTable <- gtable(
    widths = unit(x = widths, units = unit),
    heights = unit(x = heights, units = unit),
    name = figureName)
  # add interpanel space
  tmpGTable <- gtable_add_col_space(
    x = tmpGTable,
    width = unit(
      x = interPanelSpacing,
      units = unit))
  tmpGTable <- gtable_add_row_space(
    x = tmpGTable,
    height = unit(
      x = interPanelSpacing,
      units = unit))
  ##########################
  # Prep and return output #
  ##########################
  MultiPanelFigure <- list(
    panelsFree = matrix(
      data = TRUE,
      ncol = columns,
      nrow = rows),
    panelNamesFree = panelNames[seq(columns * rows)],
    unit = unit)
  attributes(tmpGTable) <- c(attributes(tmpGTable),"MultiPanelFigure"=MultiPanelFigure)
  return(tmpGTable)
}
