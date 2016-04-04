#' A photo of a rhino
#'
#' A photograph of a greater one-horned rhinoceros (Rhinoceros unicornis) taken
#' in Kaziranga National Park, Assam, India.
#' CC-BY Janette Cotton, 2016.
#' @docType data
#' @name rhino
#' @format A JPEG file.
#' @examples
#' if(requireNamespace("magrittr"))
#' {
#'   `%>%` <- magrittr::`%>%`
#'   fig <- multi_panel_figure(
#'     width = 150, height = 150,
#'     rows = 3, columns = 3,
#'     interPanelSpacing = 0
#'   )
#'   rhinoFile <- system.file("extdata/rhino.jpg", package = "multipanelfigure")
#'   fig %>%
#'     add_panel(rhinoFile) %>%
#'     add_panel(rhinoFile, leftPanel = 2, rightPanel = 3) %>%
#'     add_panel(rhinoFile, topPanel = 2, bottomPanel = 3) %>%
#'     add_panel(rhinoFile, leftPanel = 2, rightPanel = 3, topPanel = 2, bottomPanel = 3)
#' }
NULL
