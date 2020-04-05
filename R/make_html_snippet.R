#' Make an HTML Snippet of a Student File
#'
#' TODO: Planned feature used in shiny grader interface.
#'
#' @param html_file
#' @param start
#' @param end
#' @param html_open
#' @param html_close
#'
#' @return
#' @export
#'
#' @examples
make_html_snippet <- function(html_file, start, end, html_open, html_close) {
  this_html <- suppressWarnings(readLines(html_file))
  open <- suppressWarnings(readLines(html_open))
  close <- suppressWarnings(readLines(html_close))
  tempDir <- tempfile()
  dir.create(tempDir)
  html <- file.path(tempDir, "student_snippet.html")
  writeLines(c(open, this_html[start:end], close), html)
  html
}
