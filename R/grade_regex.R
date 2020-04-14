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
  cmd <- paste0("grep -Przil --include=*.", ext, " '", pattern, "'")
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
grade_regex <- function(regex_rubric, bids){
# get rubric
  auto_regex <- suppressMessages(read_csv(regex_rubric,
                                          col_types = cols(subpart = col_character())))
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
    ww <- cbind(tibble(bid = bids[i], passed = passed[i, ]), auto_regex)
    names(ww) <- c("bid", "passed", names(auto_regex))
    ww
  }) ->
    auto_regex_raw
  auto_regex_raw <- Reduce("rbind", auto_regex_raw)
  # message(1)
  auto_regex_raw %>%
    group_by(bid, question, part, subpart) %>%
    select(bid, question, part, subpart, score, match, nomatch, passed) %>%
    filter(score == max(score * passed) | !passed) %>%
    summarise(score = max(score * passed), fb = paste0(ifelse(passed, match, nomatch), collapse = "<br/>")) %>%
    ungroup() %>%
    as_tibble() ->
    auto_regex_res

  auto_regex %>%
    group_by(question, part, subpart) %>%
    summarise(possible = max(score)) ->
    regex_possible

  auto_regex_res <- suppressMessages(inner_join(auto_regex_res, regex_possible))
  # must properly set names
  auto_regex_res
}
