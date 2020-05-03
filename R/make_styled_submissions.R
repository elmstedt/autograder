#' Make Properly Styled SUbmissions
#'
#' Creates a new directory of submission files where (almost) all of the style
#' issue have been fixed. (PLANNED FEATURE: Use this to provide the source for
#' a shiny powered manual grader.)
#'
#' @param sub_dir
#' @param new_sub_dir
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom stringr str_trim
make_styled_submissions <- function(sub_dir,
                                    new_sub_dir = file.path(sub_dir,
                                                            "../",
                                                            "styled_subs")) {
    if (!dir.exists(new_sub_dir)) {
      dir.create(new_sub_dir)
    }
  for (bid in dir(sub_dir)) {
    file.copy(from = file.path(sub_dir, bid), to = new_sub_dir, recursive = TRUE)
    rmd <- dir(file.path(new_sub_dir, bid),
               recursive = TRUE,
               full.names = TRUE,
               pattern = "Rmd",
               ignore.case = TRUE)
    lines <- stringr::str_trim(readLines(rmd)) %>% unlist()

    lines <- gsub("``` ?\\{r", "```\\{r", lines)
    lines <- gsub("``(`[\\w ])", "\\1", lines)

    ### fix abs path
    writeLines(lines, rmd)
  }
  style_dir(new_sub_dir, recursive = TRUE, filetype = "Rmd")
  files <- dir(new_sub_dir, full.names = TRUE, recursive = TRUE)
  non_rmd <- files[!grepl("Rmd", files)]
  unlink(non_rmd)
  new_sub_dir
}
