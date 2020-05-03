#' Title
#'
#' @param file
#' @param local
#' @param to
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom purrr quietly
#' @importFrom R.utils withTimeout
robust_source <- function(file, local = parent.frame(), to = 10) {
  qeval <- purrr::quietly(eval)
  tryCatch({
    ll <- parse(file)
    ll <- ll[!grepl("\\bsetwd\\(", ll)]
    for (i in seq_along(ll)) {
      tryCatch({
        R.utils::withTimeout(qeval(ll[[i]], envir = local)[["results"]],
                             timeout = to)
               }, error = function(e) {
                 -1
               }
      )
    }
    TRUE
  },
  error = function(e) {
    FALSE
  }
  )
}
