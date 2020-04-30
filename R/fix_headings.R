#' Fix Malformed Section Heading
#'
#' Make section headings conform to the standard.
#' TODO: generalize to take custom regex patterns and replacements.
#'
#' @param sub_dir
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom readr read_lines
#' @importFrom stringr str_extract_all
fix_headings <- function(sub_dir) {
  rmds <- dir(file.path(sub_dir), full.names = TRUE, pattern = "Rmd", ignore.case = TRUE, recursive = TRUE)
  for (rmd in rmds){
    lines <- readLines(rmd[1])
    qpat <- "[Qq]\\w+[ #]? ?(\\d)"
    q <- grep(pattern = qpat, lines)

    actual_part_locs <- grep("Q\\w+.*\\d\\w", lines)
    questions <- stringr::str_extract_all(lines[actual_part_locs], "[Qq]\\w+ ?\\d") %>% unlist
    new_questions <- paste0("\n", unique(questions), "\n")
    new_question_locs <- sort(actual_part_locs[!duplicated(questions)], decreasing = TRUE)
    for(i in seq_along(new_question_locs)) {
      append(lines, new_questions[i], new_question_locs[i])
    }
    actual_part_locs <- grep("Q\\w+.*\\d\\w", lines)
    parts <- stringr::str_extract_all(lines[actual_part_locs], "(?<=(\\w ?\\d))\\w") %>% unlist
    for(i in seq_along(actual_part_locs)) {
      lines[actual_part_locs[i]] <- paste0("### (", parts[i], ")")
    }

    q <- grep(pattern = qpat, lines)
    lines[q] <- gsub("^#* ?[Qq][a-zA-Z]+[ #]? ?(\\d)", "\n## Question \\1\n", lines[q])

    bad_pat <- "^(#* ?)?\\((\\w)\\)|^(\\w)\\)|.*Part (\\w)"
    p <- grep(pattern = bad_pat, lines)
    lines[p] <- gsub(bad_pat, "### \\(\\3\\2\\4\\)", lines[p])

    headings <- grep("^#+ ", lines)
    before <- headings - 1
    after <- headings + 1
    need_blank <- sort(c(before[lines[before] != ""], headings[lines[after] != ""]), decreasing = TRUE)
    for (idx in need_blank) {
      lines <- append(lines, "", idx)
    }

    writeLines(lines, rmd)
    lines <- readr::read_lines(rmd)
    for (i in rev(seq_along(lines))) {
      if (lines[i] == "" && lines[i - 1] == "") {
        lines <- lines[-i]
      }
    }
    writeLines(lines, rmd)
  }
  NULL
}
