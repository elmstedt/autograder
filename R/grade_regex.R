# auto_rub <- rubric[rubric$type == "auto_regex", ]

#' Title
#'
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
process_student_regex <- function(pattern, auto_regex) {
  # message(pattern)
  ext <- auto_regex[auto_regex$regex == pattern, ]$ext
  cmd <- paste0("grep -Przil --include=*.", ext, " \"", pattern, "\"")
  a <- suppressWarnings(system(cmd, intern = TRUE))
  sort(get_bid(a))
}

did_pass <- function(l, bids) {
  bids %in% l
}

#' Title
#'
#' @param regex_rubric
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr tibble group_by select filter summarise ungroup as_tibble inner_join
#' @importFrom pbapply pblapply
#' @importFrom readr read_csv cols col_character
grade_regex <- function(regex_rubric, bids){
# get rubric
  auto_regex <- suppressMessages(readr::read_csv(regex_rubric,
                                          col_types = readr::cols(subpart = readr::col_character())))
  # message(1)

  if (any(is.na(auto_regex$match))) {
    auto_regex[is.na(auto_regex$match), ]$match <- ""
  }
  # message(1)
  if (any(is.na(auto_regex$nomatch))) {
    auto_regex[is.na(auto_regex$nomatch), ]$nomatch <- ""
  }
  # message(1)
  w <- pbapply::pblapply(auto_regex$regex, process_student_regex, auto_regex)
  # message(1)
  passed <- sapply(w, did_pass, bids)
  if (is.null(dim(passed))) {
    passed <- t(as.matrix(passed))
  }
  # message(1)
  lapply(seq_along(bids), function(i){
    ww <- cbind(dplyr::tibble(bid = bids[i], passed = passed[i, ]), auto_regex)
    names(ww) <- c("bid", "passed", names(auto_regex))
    ww
  }) ->
    auto_regex_raw
  auto_regex_raw <- Reduce("rbind", auto_regex_raw)
  # message(1)
  auto_regex_raw %>%
    dplyr::group_by(bid, question, part, subpart) %>%
    dplyr::select(bid, question, part, subpart, score, match, nomatch, passed) %>%
    dplyr::filter(score == max(score * passed) | !passed) %>%
    dplyr::summarise(score = max(score * passed), fb = paste0(ifelse(passed, match, nomatch), collapse = "<br/>")) %>%
    dplyr::ungroup() %>%
    dplyr::as_tibble() ->
    auto_regex_res

  auto_regex %>%
    dplyr::group_by(question, part, subpart) %>%
    dplyr::summarise(possible = max(score)) ->
    regex_possible

  auto_regex_res <- suppressMessages(dplyr::inner_join(auto_regex_res, regex_possible))
  # must properly set names
  auto_regex_res
}
