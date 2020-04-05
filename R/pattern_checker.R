#' Title
#'
#' @param pattern
#' @param bids
#' @param ext
#'
#' @return
#' @export
#'
#' @examples
pattern_checker <- function(pattern, bids = character(0), ext = "Rmd") {
  writeLines(pattern)
  cmd <- paste0("grep -Przil --include=*.", ext, " '", pattern, "'")
  a <- system(cmd, intern = TRUE)
  found <- unique(sort(get_bid(a)))
  list(found = found, not_found = setdiff(bids, found))
}
