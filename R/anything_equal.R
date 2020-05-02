any_which <- function(x) {
  out <- any(x)
  if (out) {
    attributes(out) <- list(objects = names(x[x]))
  }
  out
}

check_val <- function(a, b, tol = 1e-6) {
  identical(mode(a), mode(b)) &&
    (close_enough(a, b, tol) ||
       isTRUE(identical(b, a)) ||
       isTRUE(identical(as(a, class(b)), b)) ||
       (identical(length(b), length(a)) &&
          (!is.recursive(b) && ! is.recursive(a)) &&
          isTRUE(all(b == a))))
}

#' Title
#'
#' @param x 
#' @param envir 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
anything_equal <- function(x, envir, tol = 1e-6) {
  obj_names <- ls(envir = envir)
  any_which(vapply(obj_names, function(b){
    check_val(x, get(b, envir = envir), tol)
  }, logical(1)))
}
