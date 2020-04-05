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
robust_source <- function(file, local = parent.frame(), to = 10) {
  qeval <- purrr::quietly(eval)
  tryCatch({
    ll <- parse(file)
    for (i in seq_along(ll)) {
      tryCatch(R.utils::withTimeout(qeval(ll[[i]], envir = local)[["results"]], timeout = to),
               error = function(e){-1}
      )
    }
    TRUE
  },
  error = function(e) {
    FALSE
  }
  )
}
