#' Title
#'
#' @param r
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom stringr str_extract_all str_trim
get_bid <- function(r){
  suppressWarnings(r %>%
                     stringr::str_extract_all("\\d{9}(?=\\/)") %>%
                     unlist() %>%
                     stringr::str_trim())
}
