any_which <- function(x) {
  out <- any(x)
  if (out) {
    attributes(out) <- list(objects = names(x[x]))
  }
  out
}


get_highest_mode <- function(a, b) {
  modes <- c("logical", "numeric", "complex", "character")
  modes[max(match(c(mode(a), mode(b)), modes))]
}

check_val <- function(a, b, fix_mode = FALSE, tol = 1e-6) {
  h_mode <- get_highest_mode(a, b)
  if (!is.na(h_mode) && fix_mode) {
    mode(a) <- mode(b) <- h_mode
  }
  tryCatch({
    identical(mode(a), mode(b)) &&
      close_enough(a, b, tol) ||
      isTRUE(identical(b, a)) ||
      isTRUE(identical(as(a, class(b)), b)) ||
      (identical(length(b), length(a)) &&
         (!is.recursive(b) && ! is.recursive(a)) &&
         isTRUE(all(b == a)))
  }, error = function(e) {
    FALSE
  })
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
anything_equal <- function(x, envir, fix_mode = FALSE, tol = 1e-6) {
  obj_names <- ls(envir = envir)
  any_which(vapply(obj_names, function(b) {
    check_val(x, get(b, envir = envir), fix_mode, tol)
  }, logical(1)))
}
