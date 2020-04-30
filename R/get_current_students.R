#' Get Current BIDs from CCLE Participant HTML File
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom rvest html_table html_nodes
#' @importFrom xml2 read_html
get_current_students <- function(filename) {
  p <- rvest::html_table(rvest::html_nodes(xml2::read_html(filename), xpath = "//*/table"), fill = TRUE)[[1]]
  bid <- p$`ID numberSort by ID number Ascending`
  na_idx <- is.na(bid)
  bid <- bid[!na_idx]
  to_pad <- 9 - nchar(bid)
  sort(paste0(sapply(to_pad, strrep, x = "0"), bid))
}
