#' Title
#'
#' @param rmd
#' @param support_dir
#'
#' @return
#' @export
#'
#' @examples
fix_abs_path <- function(rmd, support_dir) {
  lines <- read_lines(rmd)
  load_lines <- grepl(pattern = "load\\(.*\\)", lines)
  to_load <- lines[load_lines]
  for (i in seq_along(to_load)) {
    if (str_detect(to_load[i], "/")) {
      str_split(str_extract_all(to_load[i], "(?<=load\\((\"|')).*(?=(\"|'))"), "/") %>%
        unlist() %>%
        tail(1) ->
        this_load
      to_load[i] <- paste0("load(\"", this_load, "\")")
    }
  }
  lines[load_lines] <- to_load
  writeLines(lines, rmd)
}

#' Title
#'
#' @param sub_dir
#' @param support_dir
#'
#' @return
#' @export
#'
#' @examples
fix_abs_paths <- function(sub_dir, support_dir) {
  rmds <- dir(sub_dir, recursive = TRUE, pattern = "Rmd", ignore.case = TRUE, full.names = TRUE)
  for (rmd in rmds) {
    fix_abs_path(rmd, support_dir)
  }
}
