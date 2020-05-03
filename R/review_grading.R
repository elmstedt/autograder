#' Review Grades
#'
#' @param grades
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom utils browseURL
review_grading <- function(grades) {
  grade_lines <- apply(grades, 1, paste, collapse = "<br/>")
  temp_dir <- tempfile()
  dir.create(temp_dir)
  html_file <- file.path(temp_dir, "sample_feedback.html")
  writeLines(grade_lines,
             con = html_file,
             sep = "<br/><br/><hr><br/><br/>")
  utils::browseURL(html_file)
}
