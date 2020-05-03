#' Title
#'
#' @param bid
#' @param rmds
#' @param auto_fun
#' @param auto_rub_fun
#' @param allowed_fun
#'
#' @return
#' @export
#'
#' @examples
process_student_auto <- function(bid,
                                 rmds,
                                 auto_fun,
                                 auto_rub_fun,
                                 allowed_fun,
                                 auto_val,
                                 my,
                                 check_formals = FALSE,
                                 chapter_level = 0,
                                 max_runtime = 3,
                                 debug = FALSE) {
  # TODO run correct code only once, not for every student, append
  # to rubric the correct responses

  # TODO break into multiple submodules. Pattern should be:
  # FOR each student
  #   source student file
  #   find all student functions
  #   FOR each function in the rubric
  #     IF student has same function
  #       IF student function violates whitelist
  #         award 0 points
  #         next
  #       ELSE
  #         check args (0/1)?
  #         IF loops and/or conditionals are used
  #           score multiple applied
  #         check presence of stop commands
  #         FOR each test case
  #           IF valid case
  #             capture evaluation of student function
  #             IF error
  #               create feedback message
  #           else
  #             compare student result to expected result
  #             IF student result is as expected
  #               give full points
  #             ELSE
  #               give partial points
  #               create feedback message
  #   RETURN a data frame with:
  #                 uid
  #                 function name
  #                 inputs
  #                 input type: base, edge, error
  #                 student output
  #                 correct or not

  dbg("UID: ", bid)
  this_student <- suppressMessages(dplyr::inner_join(auto_fun, auto_rub_fun))
  rmd <- rmds[grepl(bid, rmds)]
  stdr <- suppressMessages(knitr::purl(rmd, quiet = TRUE, documentation = 0))
  # deprint student functions
  writeLines(gsub(pattern = "print\\(", replacement = "invisible\\(", readLines(stdr)), stdr)
  student <- new.env()

  # check if file can source
  did_source_student <- (suppressWarnings(suppressMessages(robust_source(stdr, student, 0.1))))
  if (!did_source_student) {
    message("student file did not source.", bid)
    return(NULL)
  }

  fcalls <- auto_fun$fun
  fargs <- auto_fun$arguments

  all_functions <- unique(fcalls)
  fcalls <- gsub("\\w+::+", "", fcalls)
  to_copy <- which(stringr::str_detect(all_functions, "::+"))
  all_functions <- gsub("\\w+::+", "", all_functions)
  for (tc in to_copy) {
    assign(all_functions[tc], get(all_functions[tc]), envir = student)
    assign(all_functions[tc], get(all_functions[tc]), envir = my)
  }
  student_objs <- sapply(ls(envir = student), get, envir = student)
  student_obj_classes <- sapply(student_objs, class, USE.NAMES = TRUE)
  student_functions <- student_objs[student_obj_classes == "function"]
  fix_mode <- allowed_fun$fix_mode
  tol <- allowed_fun$tol

  #check_whitelist
  whitelist <- unique(c(whitelist[whitelist[, "ch"] <= chapter_level, "fun"],
                     names(student_functions),
                     all_functions))
  for (i in 1:3) {
    # check for recursive violations 3 deep should be sufficient for Stats 20.
    whitelist_violations <- lapply(student_functions,
                                   check_whitelist,
                                   whitelist,
                                   student)
    zeros <- sapply(whitelist_violations, function(x)length(x) == 0)
    names(zeros) <- names(student_functions)
    whitelist <- setdiff(whitelist, names(zeros[!zeros]))
  }

  zeros <- unlist(zeros)
  illegal_functions <- mapply(get_illegal_functions,
         allowed_fun$fun,
         allowed_fun$bad_fun,
         MoreArgs = list(envir = student)
  )

  bad_funs <- ifelse(sapply(illegal_functions, length) == 0, 1, allowed_fun$bad_max)
  zeros[setdiff(names(bad_funs), names(zeros))] <- 1
  bad_funs <- bad_funs * zeros[names(bad_funs)]
  illegal_loops <- mapply(get_illegal_loops,
                          all_functions,
                          allowed_fun[match(all_functions, allowed_fun$fun), ]$loops_allowed,
                          MoreArgs = list(envir = student)
  )
  bad_loops <- rep(1, length.out = length(illegal_loops))
  names(bad_loops) <- names(illegal_loops)
  baddies <- names(which(sapply(illegal_loops, length) > 0))
  bad_loops[baddies] <- allowed_fun[allowed_fun$fun %in% baddies, ]$loop_max

  illegal_conditionals <- mapply(get_illegal_conditionals,
                          all_functions,
                          allowed_fun[match(all_functions, allowed_fun$fun), ]$ifs_allowed,
                          MoreArgs = list(envir = student)
  )
  bad_ifs <- rep(1, length.out = length(illegal_conditionals))
  names(bad_ifs) <- names(illegal_conditionals)
  baddies <- names(which(sapply(illegal_conditionals, length) > 0))
  bad_ifs[baddies] <- allowed_fun[allowed_fun$fun %in% baddies, ]$if_max

  do_call <- purrr::quietly(do.call)

  check_function <- function(f, a, my, student, fix_mode, tol) {
    assign(".Traceback", NULL, "package:base")
    on.exit({
      msg <- utils::capture.output(traceback())
      if (!identical(msg, "No traceback available ")) {
        dbg(paste(msg, "\n"))
      }
    })
    expected_out <- ""
    if (f %in% c("identity")) {
      b <- deparse(get(a, envir = my))
      a <- deparse(get(a, envir = student))
    } else {
      b <- a
    }
    sarglist <- paste0("list(", a, ")")
    marglist <- paste0("list(", b, ")")
    mres <- do_call(f,
                     eval(rlang::parse_expr(marglist), envir = my),
                     envir = my)[["result"]]
    expected_out <- paste("<code>",
                          paste(utils::capture.output(mres),
                                collapse = "<br/>"),
                          "</code>")
    expected_out <- gsub(" ", "&nbsp;", expected_out)
    feedback <- paste("<code>", f, "(", a, ")</code> must return:<br/>",
                      expected_out,
                      "<br/>&nbsp;<br/>", sep = "")
    sres <- tryCatch({
      R.utils::withTimeout(
        do_call(f,
                 eval(rlang::parse_expr(sarglist), envir = student),
                 envir = student)[["result"]], timeout = max_runtime)
    }, error = function(e) {
      e
    })
    if (is.numeric(sres)) {
      sres <- round(sres, 6)
    }
    if (is.numeric(mres)) {
      mres <- round(mres, 6)
    }

    pass <- check_val(mres, sres, fix_mode, tol)

    list(pass, ifelse(pass, "", feedback))
  }

  function_results <- t(mapply(function(f, a, fix_mode, tol) {
                                 check_function(f, a, my, student, fix_mode, tol)},
                               f = fcalls,
                               a = fargs,
                               fix_mode = fix_mode,
                               tol = tol))

  dimnames(function_results) <- NULL
  function_results <- dplyr::as_tibble(plyr::alply(function_results, .margins = 2, unlist))
  this_student[, c("res", "feedback")] <- function_results

  flist <- unique(auto_fun[, 1:4])
  flist <- all_functions

  make_argstring <- function(f) {
    frml <- formals(f, my)
    fnames <- names(frml)
    fvals <- sapply(frml, format)
    joiner <- ifelse(nchar(fvals) == 0, "", " = ")
    paste0("function(", paste0(fnames, joiner, fvals, collapse = ", "), ")")
  }


  if (check_formals) {
    w <- lapply(flist, function(f) {
      score <- tryCatch({
        1 - mean(c(
          identical(formals(f, student), formals(f, my)),
          identical(names(formals(f, student)), names(formals(f, my)))))
        }, error = function(e) {
          1
      })
      data.frame(score = 1 - 0.1 * score,
                 fb = ifelse(score > 0,
                             paste0(
                               "Function arguments should be the following, ",
                               "in the following order, ",
                               "with the following default values: ",
                               make_argstring(f)),
                             ""),
                 stringsAsFactors = FALSE)
      })
    args_score <- Reduce("rbind", w)
    } else {
      args_score <- data.frame(score = rep(1, length(flist)),
                               fb = "",
                               stringsAsFactors = FALSE)
    }

this_student %>%
    dplyr::group_by(question, part, subpart) %>%
    dplyr::summarise(bid = bid,
                     score = mean(as.numeric(res)) * mean(as.numeric(points)),
                     possible = mean(as.numeric(points)),
                     fb = paste(ifelse(res, "", feedback),
                                sep = "<br/>", collapse = "<br/>")) ->
    student_res
  student_res$fb <- gsub("^(&nbsp;)?( ?<br/>)*|(<br/>&nbsp;)?(<br/>)*?$", "", student_res$fb)

  # TODO: change score to be a proportion of
  # available points from rubric (need to add points to both auto and regex rubrics)
  student_res$score <- ceiling(student_res$score *
                                 args_score$score *
                                 bad_funs *
                                 pmin(bad_ifs, bad_loops)
                               * 10) / 10
  student_res$fb <- ifelse(bad_ifs == 1,
                           student_res$fb,
                           paste("Function should be written without conditionals.<br/>",
                                 student_res$fb,
                                 sep = ifelse(student_res$fb == "", "", "<br/>")))

  student_res$fb <- ifelse(bad_loops == 1,
                           student_res$fb,
                           paste("Function should be written without loops.<br/>",
                                 student_res$fb,
                                 sep = ifelse(student_res$fb == "", "", "<br/>")))
  student_res$fb <- ifelse(bad_funs == 1,
                           student_res$fb,
                           paste("Function used disallowed functions.<br/>",
                                 student_res$fb,
                                 sep = ifelse(student_res$fb == "", "", "<br/>")))
  student_res$fb <- ifelse(args_score$fb == "",
                           student_res$fb,
                           paste(args_score$fb, student_res$fb,
                                 sep = ifelse(student_res$fb == "", "", "<br/>")))
    student_res$fb <- gsub("^(<br/>)+", "", student_res$fb)

  # Revert to data.frame and do with with() and by().
  auto_val %>%
    dplyr::group_by(object_name) %>%
    dplyr::group_map(grade_value, my = my, student = student, keep = TRUE) ->
    val_results

  val_results <- dplyr::bind_rows(
    val_results
  )

  dplyr::bind_cols(
    bid = rep(bid, nrow(auto_val)),
    val_results) -> graded_values
  student_res <- student_res[c("bid",
                               "question",
                               "part",
                               "subpart",
                               "possible",
                               "score",
                               "fb")]

  out <- dplyr::bind_rows(
    student_res,
    graded_values)
  file.remove(stdr)
  rm(student)
  out
}

#' Title
#'
#' @param function_rubric
#' @param rubric_file
#' @param sol_file
#' @param bids
#' @param rmds
#' @param allowed_fun
#'
#' @return
#' @export
#'
#' @examples
grade_functions <- function(function_rubric,
                            rubric_file,
                            values_rubric,
                            sol_file,
                            bids,
                            rmds,
                            allowed_fun,
                            check_formals,
                            chapter_level = 0,
                            max_runtime = 3,
                            debug = FALSE) {
  dbg("In grade functions")
  me <- knitr::purl(sol_file)
  my <- new.env()
  did_source_teacher <- robust_source(me, my)
  if (!isTRUE(did_source_teacher)) {
    stop("Failed to source solutions.")
  }
  dbg("did_source_teacher: ", did_source_teacher)

  rubric <- suppressMessages(readr::read_csv(rubric_file,
                                      col_types = readr::cols(.default = "c")))
  auto_rub_fun <- rubric[rubric$type == "auto_fun", ]
  auto_fun <- suppressMessages(readr::read_csv(function_rubric,
                                        col_types = readr::cols(.default = "c")))
  auto_val <- suppressMessages(readr::read_csv(values_rubric,
                                        col_types = readr::cols(.default = "c")))
  allowed_fun$fix_mode <- as.logical(allowed_fun$fix_mode)
  allowed_fun$tol <- as.numeric(allowed_fun$tol)
  auto_val$fix_mode <- as.logical(auto_val$fix_mode)
  auto_val$tol <- as.numeric(auto_val$tol)

  raw_auto_fun <- pbapply::pblapply(bids, process_student_auto,
                         rmds = rmds,
                         auto_fun = auto_fun,
                         auto_rub_fun = auto_rub_fun,
                         allowed_fun = allowed_fun,
                         auto_val = auto_val,
                         my = my,
                         check_formals = check_formals,
                         chapter_level = chapter_level,
                         max_runtime = max_runtime,
                         debug = debug)
  auto_fun_res <- Reduce("rbind", raw_auto_fun)
  attr(auto_fun_res, "groups") <- NULL
  dplyr::as_tibble(auto_fun_res)
}
