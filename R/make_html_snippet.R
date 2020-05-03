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
  temp_dir <- tempfile()
  dir.create(temp_dir)
  html <- file.path(temp_dir, "student_snippet.html")
  writeLines(c(open, this_html[start:end], close), html)
  html
}
