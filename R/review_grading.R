#' Review Grades
#'
#' @param grades
#'
#' @return
#' @export
#'
#' @examples
review_grading <- function(grades) {
  grade_lines <- apply(grades, 1, paste, collapse = "<br/>")
  tempDir <- tempfile()
  dir.create(tempDir)
  html_file <- file.path(tempDir, "sample_feedback.html")
  writeLines(grade_lines,
             con = html_file,
             sep = "<br/><br/><hr><br/><br/>")
  browseURL(html_file)
}
