#' @title multipanelfigure
#' @aliases multipanelfigure
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
#'   \item{\code{multipanelfigure.panelsFree}:}{A \code{\link{logical}}
#'     \code{\link{matrix}} with the dimensions of the \code{\link[gtable]{gtable}}
#'     indicating occupancy of the panels in the table.}
#'   \item{\code{multipanelfigure.panelLabelsFree}:}{A \code{\link{character}}
#'     \code{\link{vector}} indicative of the \code{panelLabels} still available.}
#'   \item{\code{multipanelfigure.units}:}{A single \code{\link{character}}
#'     object storing the corresponding value given during object creation.}}
#' @author Johannes Graumann
#' @export
#' @seealso \code{\link[gtable]{gtable}}, \code{\link{addpanel}},
#' \code{\link{simplegrobwidth}}, \code{\link{simplegrobheight}}
#' @keywords hplot utilities
#' @importFrom assertive.base assert_all_are_true
#' @importFrom assertive.properties assert_is_null
#' @importFrom assertive.properties assert_is_not_null
#' @importFrom assertive.numbers assert_all_are_non_negative
#' @importFrom assertive.numbers assert_all_are_positive
#' @importFrom assertive.numbers assert_all_are_whole_numbers
#' @importFrom assertive.numbers assert_all_are_in_range
#' @importFrom assertive.types assert_is_a_bool
#' @importFrom assertive.types assert_is_numeric
#' @importFrom assertive.types assert_is_a_string
#' @importFrom assertive.types assert_is_a_number
#' @importFrom assertive.types assert_is_numeric
#' @importFrom assertive.types assert_is_character
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_col_space
#' @importFrom gtable gtable_add_row_space
#' @importFrom magrittr %>%
#' @importFrom magrittr %T>%
#' @examples
#' library(gtable)
#' # Figure construction based on overall dimensions
#' Figure1 <- multipanelfigure(
#'    width = 100,
#'    columns = 4,
#'    height = 100,
#'    rows = 6,
#'    figureName = "Figure1")
#' gtable_show_layout(Figure1)
#'
#' # Figure construction based on individual panel dimensions
#' Figure2 <- multipanelfigure(
#'    widths = c(20,30),
#'    heights = c(40,60),
#'    figureName = "Figure2")
#' gtable_show_layout(Figure2)
#'
#' # A more involved example including filling and printing to device ...
#' ## Make a simple ggplot object to fill panels
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p <- p + geom_point()
#' ## Fill panels
#' Figure2 <- addpanel(p, Figure2, topPanel = 1, leftPanel = 2)
#' Figure2 <- addpanel(p, Figure2, topPanel = 2, leftPanel = 1, rightPanel = 2)
#' ## Plot to appropriately sized png device
#' tmpFile <- tempfile(fileext = ".png")
#' usedUnits <- "in"
#' png(
#'   filename = tmpFile,
#'   width = simplegrobwidth(Figure2, unitTo = usedUnits),
#'   height = simplegrobheight(Figure2, unitTo = usedUnits),
#'   units = usedUnits,
#'   res = 300)
#' grid.draw(Figure2)
#' dev.off()
#' message(
#'   paste0("Now have a look at '",tmpFile,"' - nicely sized PNG output."))
multipanelfigure <- function(
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
  units %T>%
    assert_is_a_string() %T>%
    grid:::valid.units()

  assert_is_a_number(interPanelSpacing)
  if(is.nan(interPanelSpacing)){
    interPanelSpacing <-
      5 %>%
      unit(units = "mm") %>%
      convertUnit(unitTo = units) %>%
      as.numeric()
  }
  assert_all_are_non_negative(interPanelSpacing)

  assert_is_a_string(figureName)

  if(!is.null(width)){
    assert_is_null(widths)
    width %T>%
      assert_is_a_number() %T>%
      assert_all_are_positive()
    columns %T>%
      assert_is_not_null() %T>%
      assert_is_a_number() %T>%
      assert_all_are_whole_numbers() %T>%
      assert_all_are_in_range(lower = 1, upper = Inf)
    widths <- rep(
      x = (width - interPanelSpacing * (columns - 1))/columns,
      times = columns)
  } else {
    assert_is_null(columns)
    widths %T>%
      assert_is_not_null() %T>%
      assert_is_numeric() %T>%
      assert_all_are_positive()
    columns <- length(widths)
    width <- sum(widths) + interPanelSpacing * (columns - 1)
  }

  if(!is.null(height)){
    assert_is_null(heights)
    height %T>%
      assert_is_a_number() %T>%
      assert_all_are_positive()
    rows %T>%
      assert_is_not_null() %T>%
      assert_is_a_number() %T>%
      assert_all_are_whole_numbers() %T>%
      assert_all_are_in_range(lower = 1, upper = Inf)
    heights <- rep(
      x = (height - interPanelSpacing * (rows - 1))/rows,
      times = rows)
  } else {
    assert_is_null(rows)
    heights %T>%
      assert_is_not_null() %T>%
      assert_is_numeric() %T>%
      assert_all_are_positive()
    rows <- length(heights)
    height <- sum(heights) + interPanelSpacing * (rows - 1)
  }

  assert_is_character(panelLabels)
  assert_all_are_true(length(columns) * length(rows) <= length(panelLabels))

  ####################
  # Construct gtable #
  ####################
  # Basic layout
  tmpGTable <-
    gtable(
      widths = unit(x = widths, units = units),
      heights = unit(x = heights, units = units),
      name = figureName) %>%
    # add interpanel space
    gtable_add_col_space(
      width = unit(
        x = interPanelSpacing,
        units = units)) %>%
    gtable_add_row_space(
      height = unit(
        x = interPanelSpacing,
        units = units))
  ##########################
  # Prep and return output #
  ##########################
  multipanelfigure <- list(
    panelsFree = matrix(
      data = TRUE,
      ncol = columns,
      nrow = rows),
    panelLabelsFree = panelLabels[seq(columns * rows)],
    units = units)
  attributes(tmpGTable) <- c(attributes(tmpGTable),"multipanelfigure"=multipanelfigure)
  return(tmpGTable)
}
