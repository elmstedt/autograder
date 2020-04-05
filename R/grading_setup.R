#' Grading Setup
#'
#' @return
#' @export
#'
#' @examples
grading_setup <- function() {
  from <- dir(system.file("rubric_files", package = "autograder"), full.names = TRUE)
  file.copy(from, ".")
}

#' Grading Demo
#'
#' @return
#' @export
#'
#' @examples
grading_demo <- function() {
  from <- dir(system.file("rubric_files", package = "autograder"), full.names = TRUE)
  file.copy(from, ".")
  from <- system.file("subs", package = "autograder")
  file.copy(from, ".", recursive = TRUE)
}
