#' Get Start and End Lines for Questions
#'
#' @param this_file
#' @param rubric
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr tibble
#' @importFrom stringr str_trim str_replace_all
#' @importFrom utils head
get_question_locations <- function(this_file, rubric) {
  if (any(grepl("<!DOCTYPE html>", this_file))) {
    type <- "html"
  } else {
    type <- "rmd"
  }
  loc_start <- function(key, rubric, low = 1, high = length(this_file)) {
    a <- which(grepl(key,
                     this_file,
                     ignore.case = TRUE) &
                 seq_along(this_file) >= low &
                 seq_along(this_file) <= high)
    max(a[as.integer(length(a) > 0)], low)
  }
  questions <- unique(rubric$question)

  if (type == "html") {
    question_pat <- paste0("<h(\\d)> *Que?stion ?#?", questions, ".*</h\\1>")
  } else {
    this_file <- stringr::str_trim(
      stringr::str_replace_all(this_file,
                               pattern = "## ?(Question ?#? ?\\d|\\(?[a-zA-Z]\\)?).*",
                               replacement = "## \\1"))
    this_file <- stringr::str_trim(
      stringr::str_replace_all(this_file,
                               pattern = "^(\\([a-zA-Z]\\).*)",
                               replacement = "### \\1"))

    question_pat <- paste0("#+ *Question ?#? ?", questions)
  }
  q_starts <- cummax(sapply(question_pat, loc_start))

  if (type == "html" && all(q_starts == 1)) {
    question_pat <- paste0("^<p>##[\\w ]*", questions)
    (q_starts <- sapply(question_pat, loc_start))
  }
  if (type == "html" && all(q_starts == 1)) {
    question_pat <- paste0("^<p>Question ", questions)
    (q_starts <- sapply(question_pat, loc_start))
  }
  if (type == "html" && all(q_starts == 1)) {
    question_pat <- paste0("<h(\\d)> ?# ?", questions, " ?</h\\1>")
    (q_starts <- sapply(question_pat, loc_start))
  }


  q_ends <- c(q_starts[-1] - 1, length(this_file))
  names(q_ends) <- names(q_starts)
  locs <- dplyr::tibble(q = integer(0),
                        p = character(0),
                        start = integer(0),
                        end = integer(0))
  i <- 1
  for (i in seq_along(questions)) {
    q_parts <- unique(rubric[rubric$question == questions[i], ]$part)
    if (type == "html") {
      part_pat <- paste0("<h(\\d)> *\\(?", q_parts, "\\)?.*</h\\1>")
    } else {
      part_pat <- paste0("^##+ *\\(?", q_parts, "\\)?$")
    }
    p_starts <- unlist(sapply(part_pat,
                              loc_start,
                              low = q_starts[i],
                              high = q_ends[i]))

    opts <- c(unique(p_starts), q_ends[i][length(p_starts) > 0]) - 1
    p_ends <- opts[apply(outer(p_starts - 1, opts, `<`), 1, which.max)]

    idx <- which(p_ends == 0)
    if (length(idx) > 0) {
      p_ends[idx] <- p_ends[idx + 1]
    }

    names(p_ends) <- names(p_starts) <- q_parts[match(names(p_starts), part_pat)]
    missing <- setdiff(q_parts, names(p_starts))
    p_starts[missing] <- 1
    p_ends[missing] <- length(this_file)
    p_starts <- p_starts[order(names(p_starts))]
    p_ends <- p_ends[order(names(p_ends))]
    p_starts <- cummax(pmax(p_starts, q_starts[i]))

    p_ends <- utils::head(cummax(c(p_ends, q_ends[i])), -1)

    locs <- rbind(locs, dplyr::tibble(q = questions[i],
                                      p = c("", names(p_starts)),
                                      start = c(q_starts[i], p_starts),
                                      end = c(q_ends[i], p_ends)))
  }

  locs
}
