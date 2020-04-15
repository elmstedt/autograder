#' Title
#'
#' @param reader_file
#' @param results_file
#' @param rubric_file
#' @param regex_rubric_file
#' @param function_rubric_file
#' @param model_solution_file
#' @param support_dir
#' @param allowed_functions_file
#'
#' @return
#' @export
#'
#' @examples
compile_grades <- function(reader_file = "grader.csv",
                           results_file = "grades.csv",
                           rubric_file = "rubric.csv",
                           regex_rubric_file = "rubric_auto_regex.csv",
                           function_rubric_file = "rubric_auto_fun.csv",
                           model_solution_file = "model_solutions.Rmd",
                           support_dir = "support_files",
                           allowed_functions_file = "allowed_functions.csv",
                           grade_style = TRUE,
                           check_formals = FALSE,
                           debug = FALSE
) {
  message("Reading in rubric files...")
  rubric <- suppressMessages(read_csv(rubric_file,
                                      col_types = cols(.default = "c")))
  auto_fun <- suppressMessages(read_csv(function_rubric_file,
                                        col_types = cols(.default = "c")))

  # fix auto_fun with automatic feedback.
  allowed_fun <- suppressMessages(read_csv(allowed_functions_file,
                                           col_types = cols(.default = "c")))
  grader <- suppressMessages(read_csv(reader_file,
                                      col_types = cols(.default = "c")))

  auto_rub_fun <- rubric[rubric$type == "auto_fun", ]
  message("Copying support files...")
  suppressWarnings(file.copy(support_dir, getwd(), recursive = TRUE))
  rmds <- dir("subs", pattern = "Rmd$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
  bids <- get_bid(rmds)

  # only grade currently enrolled students
  # message("Getting current students from roster...")
  # students <- get_current_students(roster_file)
  # bids <- intersect(bids, students)

  # grade functions
  message("Grading student functions...")

  graded_fun <- grade_functions(function_rubric = function_rubric_file,
                                rubric_file = rubric_file,
                                sol_file = model_solution_file,
                                bids = bids,
                                rmds = rmds,
                                allowed_fun = allowed_fun,
                                check_formals = check_formals,
                                debug = debug)

  # message("Average homework grade", mean(graded_fun$score))
  # grade regex

  # auto_regex <- read_csv("rubric_auto_regex.csv")
  message("Grading student answers...")
  regex_rub <- suppressMessages(read_csv(regex_rubric_file,
                                         col_types = cols(.default = "c")))
  if (nrow(regex_rub) > 0) {
    graded_regex <- grade_regex(regex_rubric_file, bids)
  } else {
    graded_regex <- graded_fun[0,]
  }

  # grade style
  if (!isFALSE(grade_style)) {
    message("Grading student style...")
    style_grades <- pblapply(bids, grade_all_style, rmds, make_my_lintrs(), make_stats_20_lintrs())
    style_grades <- Reduce("rbind", style_grades)
  } else {
    style_grades <- tibble(bid = bids, style_score = 0, style_table = "")
  }

  message("Compiling grade file...")
  # combine function and regex grades
  auto_raw <- rbind(graded_fun, graded_regex)
  auto_raw %>%
    group_by(bid, question, part, subpart) %>%
    summarise(possible = sum(possible), score = sum(score), feedback = paste0(fb, collapse = "<br/>")) %>%
    ungroup() ->
    auto_res
  auto_res$score <- round(auto_res$score, 1)

  # a <- auto_res[auto_res$bid == bid,]

  # load manual grading file

  # 0-pad bids
  if (nrow(grader) > 0) {
  grader <- with(grader,{
    grader$bid <- ifelse(nchar(bid) < 9, paste0("00", bid), bid)
    grader})
  # add grader possible points (should be programatic)
  # grader$possible <- 1
  # grader[grader$question == 3, ]$possible <- 2
  grader <- grader[grader$bid %in% bids,]
  #grader <- grader[grader$bid %in% students,]
  # add subpart column
  grader$part <- as.character(grader$part)
  if (is.null(grader[["subpart"]])) {
    grader$subpart <- NA_character_
  }

  grader <- suppressMessages(inner_join(grader,
                       rubric[rubric$type == "grader",
                              c("question", "part", "subpart", "points")]))
  grader$possible <- grader$points
  # reorder grader columns to match other files
  grader <- grader[names(auto_res)]
  grader <- grader[!is.na(grader$bid),]

  # bind grader results to auto results and reorder rows
  res <- rbind(grader, auto_res)
  } else {
    res <- auto_res
  }
  ord <- with(res, order(bid, question, part, subpart))
  res <- res[ord,]

  # remove question 0 if one exists (legacy)
  if (any(res$question == 0, na.rm = TRUE)){
    res[res$question == 0, ]$question <- NA
  }

  # set any NA scores to 0
  if (any(is.na(res$score))) {
    res[is.na(res$score),]$score <- 0
  }

  # make grade data frame containing bid and raw score
  res %>%
    group_by(bid) %>%
    summarise(grade = sum(as.numeric(score))) ->
    df_grade

  # get total possible points
  res %>%
    group_by(bid) %>%
    summarise(pos = sum(as.numeric(possible))) %>%
    `[[`(., 2) %>%
    max ->
    tot_pos
  # res[, "possible"] <- as.character(res[, "possible"])
  # make feedback body
  res %>%
    group_by(bid) %>%
    do(make_data_row(.)) ->
    feedback_body


  # make feedback csv file
  fbb <- feedback_body
  full_feedback <- make_report(feedback_body$feedback, rep(tot_pos, nrow(feedback_body)), df_grade$grade, style_grades$style_score, scoring_expr)
  ffb <- gsub("\\n", "", full_feedback)
  df_grade$grading_table <- ffb
  style_grades$style_table <- gsub("\\n", "<br/>", style_grades$style_table)

  # todo: include late penalty calculation
  grading_results <- inner_join(df_grade, style_grades, by = "bid")
  grading_results <- grading_results %>%
    group_by(bid) %>%
    summarise(grade = scoring_function(grade) + ifelse(identical(grade_style, 0),
                                                       0,
                                                       style_score),
              feedback = paste(grading_table, style_table))
  grading_results$feedback <- gsub(pattern = "(<br/>)+", replacement = "<br/>", grading_results$feedback)
  write_csv(grading_results, results_file)
  message("Finished grading. The grading results are in the file:\n", normalizePath(file.path(".", results_file)))
  grading_results
}
