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
process_student_auto <- function(bid, rmds, auto_fun, auto_rub_fun, allowed_fun, my, check_formals = FALSE, debug = FALSE) {
  # message(bid)
  this_student <- suppressMessages(inner_join(auto_fun, auto_rub_fun))
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
  # get rid of all non-functions in the environment
  sfs <- lsf.str(envir = student)
  sall <- ls(envir = student)
  to_remove <- setdiff(sall, sfs)
  # rm(list = to_remove, envir = student)

  fcalls <- auto_fun$fun
  fargs <- auto_fun$arguments

  all_functions <- unique(fcalls)
  fcalls <- gsub("\\w+::", "", fcalls)
  to_copy <- which(str_detect(all_functions, "::"))
  all_functions <- gsub("\\w+::", "", all_functions)
  for (tc in to_copy) {
    assign(all_functions[tc], get(all_functions[tc]), envir = student)
    assign(all_functions[tc], get(all_functions[tc]), envir = my)
  }
  student_objs <- lapply(ls(envir = student), get, envir = student)
  student_obj_classes <- sapply(student_objs, class)
  student_obj_names <- ls(envir = student)

  student_functions <- student_objs[student_obj_classes == "function"]
  names(student_functions) <- student_obj_names[student_obj_classes == "function"]


  # check missing functions
  missing_functions <- setdiff(all_functions, ls(student))
  found_functions <- intersect(names(student_functions), allowed_fun$fun)

  # check if any functions have illegal functions
  illegal_functions <- mapply(get_illegal_functions,
         allowed_fun$fun,
         allowed_fun$bad_fun,
         MoreArgs = list(envir = student)
  )
  # illegal_functions$my_rev <- c("rev", "seq")
  bad_funs <- sapply(illegal_functions, function(q){ifelse(length(q) == 0, 1, allowed_fun$bad_max)})

  # bad_funs <- names(bad_funs[!is.na(bad_funs)])
  # check if any functions use banned loops
  illegal_loops <- mapply(get_illegal_loops,
                          all_functions,
                          allowed_fun[match(all_functions, allowed_fun$fun),]$loops_allowed,
                          MoreArgs = list(envir = student)
  )
  bad_loops <- sapply(illegal_loops, function(q){ifelse(length(q) == 0, 1, allowed_fun$loop_max)})
  
  illegal_conditionals <- mapply(get_illegal_conditionals,
                          all_functions,
                          allowed_fun[match(all_functions, allowed_fun$fun),]$ifs_allowed,
                          MoreArgs = list(envir = student)
  )
  bad_ifs <- sapply(illegal_conditionals,
                    function(q){ifelse(length(q) == 0, 1, allowed_fun$if_max)})
  
  qdo.call <- purrr::quietly(do.call)
  function_results <- t(mapply(function(f, a){
    tryCatch({
      expected_out <- ""
      if (f == "identity") {
        b <- deparse(get(a, envir = my))
        a <- deparse(get(a, envir = student))
      } else {
        b <- a
      }
      sarglist <- paste0("list(", a, ")")
      marglist <- paste0("list(", b, ")")

      mres <- qdo.call(f, eval(rlang::parse_expr(marglist)), envir = my)[["result"]]
      expected_out <- paste("<code>", paste(capture.output(mres), collapse = "<br/>"), "</code>")
      expected_out <- gsub(" ", "&nbsp;", expected_out)
      feedback <- paste("<code>", f, "(", a, ")</code> must return:<br/>", expected_out, "<br/>&nbsp;<br/>", sep = "")
      sres <- R.utils::withTimeout(qdo.call(f, eval(rlang::parse_expr(sarglist)), envir = student)[["result"]], timeout = 3)
      if (is.numeric(sres)) {
        sres <- round(sres, 6)
      }
      if (is.numeric(mres)) {
        mres <- round(mres, 6)
      }
      pass <- close_enough(mres, sres, 1e-6) ||
        isTRUE(identical(sres, mres)) ||
        isTRUE(identical(as(mres, class(sres)), sres)) ||
        (identical(length(sres), length(mres)) &&
           (!is.recursive(sres) && ! is.recursive(mres)) &&
           isTRUE(all(sres == mres)))

      list(pass, ifelse(pass, "", feedback))
      },
      error = function(e){
        dbg("\nUID: ", bid, "\nError encountered: ", e)
        if (exists("sres")) {
          dbg("\nsres: ", typeof(sres), "\n", sres)
        }
        if (exists("mres")) {
          dbg("\nmres: ", typeof(mres), "\n", mres)
        }
        list(FALSE, "")
      }
    )
  },  f = fcalls, a = fargs))
  dimnames(function_results) <- NULL
  function_results <- tibble::as_tibble(plyr::alply(function_results, .margins = 2, unlist))
  this_student[, c("res", "feedback")] <- function_results

  # check formals
  flist <- unique(auto_fun[, 1:4])
  flist <- all_functions

  make_argstring <- function(f) {
    frml <- formals(f, my)
    fnames <- names(frml)
    fvals <- sapply(frml, format)
    joiner <- ifelse(nchar(fvals) == 0, "", " = ")
    paste0("function(", paste0(fnames, joiner, fvals, collapse = ", "), ")")
  }


  lapply(flist, function(f){
    if (check_formals) {
    test <- tryCatch(
      {mean(c(identical(formals(f, student), formals(f, my)), identical(names(formals(f, student)), names(formals(f, my)))))},
      error = function(e){
        FALSE
      }
    )
    } else {
      test <- 1
    }
    if (test) {
      tibble(score = ((1 + test) / 2)^(1 / 4), fb = ifelse(test < 1, paste0("Function arguments should be the following, in the following order, with the following default values: ", make_argstring(f)),""))
    } else {
      tibble(score = 0.75, fb = paste0("Function arguments should be the following, in the following order, with the following default values: ", make_argstring(f)))
    }
  }) -> w

  args_score <- Reduce("rbind", w)

  # if formals are wrong no more than 1/2 credit
  #update_no_credit <- unique(this_student[this_student$fun %in% no_credit,c("question", "part")])

this_student %>%
    group_by(question, part, subpart) %>%
    summarise(bid = bid,  score = mean(as.numeric(res)) * mean(as.numeric(points)), possible = mean(as.numeric(points)), fb = paste(ifelse(res, "", feedback), sep = "<br/>", collapse = "<br/>")) ->
    student_res
  student_res$fb <- gsub("^(&nbsp;)?( ?<br/>)*|(<br/>&nbsp;)?(<br/>)*?$", "", student_res$fb)

  # Check body for banned functions


  # if found function gets 0

  # TODO: change score to be a proportion of
  # available points from rubric (need to add points to both auto and regex rubrics)
  student_res$score <- ceiling(student_res$score *
                                 args_score$score *
                                 bad_funs *
                                 pmin(bad_ifs, bad_loops)
                               * 10) / 10
  student_res$fb <- ifelse(args_score$fb == "",
                           student_res$fb,
                           paste(student_res$fb, args_score$fb,
                                 sep = ifelse(student_res$fb == "", "", "<br/>")))
  student_res$fb <- ifelse(bad_funs == 1,
                           student_res$fb,
                           paste("Function used disallowed functions.<br/>",
                                 student_res$fb,
                                 sep = ifelse(student_res$fb == "", "", "<br/>")))
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
  student_res$fb <- gsub("^(<br/>)+", "", student_res$fb)
  did_remove_student <- file.remove(stdr)
  rm(student)
  student_res[c("bid", "question", "part", "subpart", "possible", "score", "fb")]
  student_res
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
                            sol_file,
                            bids,
                            rmds,
                            allowed_fun,
                            check_formals,
                            debug = FALSE){
  me <- knitr::purl(sol_file)
  my <- new.env()
  did_source_teacher <- source(me, my)
  # message(did_source_teacher)
  rubric <- suppressMessages(read_csv(rubric_file,
                                      col_types = cols(.default = "c")))
  auto_rub_fun <- rubric[rubric$type == "auto_fun", ]
  auto_fun <- suppressMessages(read_csv(function_rubric,
                                        col_types = cols(.default = "c")))

  raw_auto_fun <- pblapply(bids, process_student_auto,
                         # bids = bids,
                         rmds = rmds,
                         auto_fun = auto_fun,
                         auto_rub_fun = auto_rub_fun,
                         allowed_fun = allowed_fun,
                         my = my,
                         check_formals = check_formals,
                         debug = debug)
  auto_fun_res <- Reduce("rbind", raw_auto_fun)
  attr(auto_fun_res, "groups") <- NULL
  as_tibble(auto_fun_res)
}

