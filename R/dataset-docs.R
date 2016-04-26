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
#'   figure <- multi_panel_figure(
#'     width = 150, height = 150,
#'     rows = 3, columns = 3,
#'     inter_row_spacing = 0,
#'     inter_column_spacing = 0
#'   )
#'   rhinoFile <- system.file("extdata/rhino.jpg", package = "multipanelfigure")
#'   figure %>%
#'     add_panel(rhinoFile) %>%
#'     add_panel(rhinoFile, left_panel = 2, right_panel = 3) %>%
#'     add_panel(rhinoFile, top_panel = 2, bottom_panel = 3) %>%
#'     add_panel(rhinoFile, left_panel = 2, right_panel = 3, top_panel = 2, bottom_panel = 3)
#' }
NULL

#' Mass spectrometry intensities by stem cell type and organelle
#'
#' This data was used to create Supplementary Figure 4e of Billing 2016 (see
#' references).
#'
#' A data frame of genes corresponding to protein intensities as measured by
#' mass spectrometry proteomics experiments on embryonic and mesenchymal stem
#' cells.
#' @docType data
#' @name billing2016_suppfig4e
#' @format A data frame with 81 rows and the following columns:
#' \describe{
#' \item{GeneName}{A factor with three levels naming genes that have interesting
#' properties.}
#' \item{Intensity}{A numeric vector of positive intensities of proteins
#' corresponding to the genes as determined by mass spectrometry.}
#' \item{StemCellType}{A factor with three levels indicating the type of stem
#' cell experimented on. "ESC" means embryonic stem cell; "ESC-MSC" means
#' mesenchymal stem cell derived from an embryonic stem cell; "BM-MSC" means
#' mesenchymal stem cell derived from bone marrow.}
#' \item{Organelle}{The region of the cell experimented on. "CH" means
#' chromatin, "Cyt" means cytosol, "Nuc" means nucleus.}
#' \item{Replicate}{An integer specifying the experimental replicate.}
#' \item{Experiment}{The interaction of StemCellType, Organelle and Replicate.}
#' }
#' @references
#' Billing AM, Ben Hamidane H, Dib SS, et al. Comprehensive transcriptomic and
#' proteomic characterization of human mesenchymal stem cells reveals source
#' specific cellular markers. Scientific Reports. 2016;6:21507.
#' doi:10.1038/srep21507.
#'
#' Article text available at:
#' \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4746666}
#'
#' Supplementary figures available at:
#' \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4746666/bin/srep21507-s1.pdf}
#' @seealso \code{\link{billing2016_suppfig4g}}
#' @examples
#' ggplot2::ggplot(billing2016_suppfig4e, ggplot2::aes(Experiment, Intensity)) +
#'   ggplot2::geom_bar(stat = "identity") +
#'   ggplot2::geom_vline(xintercept = seq(3.5, 24.5, 3), linetype = "dotted") +
#'   ggplot2::facet_wrap(~ GeneName) +
#'   ggplot2::xlab(NULL) +
#'   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 4))
NULL

#' Mass spectrometry intensities by stem cell type
#'
#' This data was used to create Supplementary Figure 4g of Billing 2016 (see
#' references).
#'
#' A matrix of log base 10 protein intensities as measured by mass spectrometry
#' proteomics experiments on embryonic and mesenchymal stem cells.
#' @docType data
#' @name billing2016_suppfig4g
#' @format A matrix with 13 rows and 9 columns.  Rows represent genes, columns
#' represent experiments and are split by:
#' \enumerate{
#' \item{The type of stem cell experimented on. "ESC" means embryonic stem cell;
#' "ESC-MSC" means mesenchymal stem cell derived from an embryonic stem cell;
#' "BM-MSC" means mesenchymal stem cell derived from bone marrow.}
#' \item{The experimental replicate.}
#' }
#' Values in the matrix are intensities of proteins coresponding to the genes,
#' as measured by mass spectrometry.
#' @references
#' Billing AM, Ben Hamidane H, Dib SS, et al. Comprehensive transcriptomic and
#' proteomic characterization of human mesenchymal stem cells reveals source
#' specific cellular markers. Scientific Reports. 2016;6:21507.
#' doi:10.1038/srep21507.
#'
#' Article text available at:
#' \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4746666}
#'
#' Supplementary figures available at:
#' \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4746666/bin/srep21507-s1.pdf}
#' @seealso \code{\link{billing2016_suppfig4e}}
#' @examples
#' color_scale <- grDevices::cm.colors(25)
#' heatmap(
#'   billing2016_suppfig4g,
#'   margins = c(12, 5), col = color_scale,
#'   cexRow = 0.5, cexCol = 0.4)
NULL
