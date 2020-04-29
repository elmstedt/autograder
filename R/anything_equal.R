# anything_equal <- function(x, envir) {
#   obj_names <- ls(envir = envir)
#   any(vapply(obj_names, function(b){
#     check_pass(x, get(b, envir = envir))
#   }, logical(1)))
# }

anything_equal <- function(x) {
  pf <- parent.frame()
  obj_names <- ls(envir = pf)
  any(vapply(obj_names, function(b){
    check_pass(x, get(b, envir = pf))
  }, logical(1)))
}

check_pass <- function(a, b, tol = 1e-6) {
  identical(mode(a), mode(b)) &&
  (close_enough(a, b, tol) ||
    isTRUE(identical(b, a)) ||
    isTRUE(identical(as(a, class(b)), b)) ||
    (identical(length(b), length(a)) &&
       (!is.recursive(b) && ! is.recursive(a)) &&
       isTRUE(all(b == a))))
}

which_equal <- function(x, envir) {
  obj_names <- ls(envir = envir)
  names(vapply(obj_names, function(b){
    check_pass(x, get(b, envir = envir))
  }, logical(1)))
}