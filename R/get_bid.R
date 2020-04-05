#' Title
#'
#' @param r
#'
#' @return
#' @export
#'
#' @examples
get_bid <- function(r){
  suppressWarnings(r %>%
                     str_extract_all("\\d{9}(?=\\/)") %>%
                     unlist() %>%
                     str_trim())
}
