#' Find all the functions used INSIDE student written functions
#'
#' @param hw_dir
#' @param exclude
#' @param flist
#'
#' @return
#' @export
#'
#' @examples
find_all_functions <- function(hw_dir, exclude = NULL, flist = NULL) {
  bids <- dir(hw_dir)
  rmds <- dir(hw_dir, pattern = "Rmd$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  bid <- bids[1]
  process_one <- function(bid, exclude, flist){
    message(bid)
    get_student_functions <- function(f, envir = globalenv()) {
      sf <- tryCatch(get(f, envir = envir),
                     error = function(e){
                       NULL
                     })
      if (is.null(sf)){
        character(0)
      } else {
        bod <- body(sf)
        temp <- tempfile()
        writeLines(c("f <- function(){", as.character(bod)[-1], "}"), temp)
        # loops <- c("FOR", "WHILE", "REPEAT")
        pc <- lintr::get_source_expressions(temp)$expressions[[1]]$parsed_content
        fcall <- c("SYMBOL_FUNCTION_CALL", "SPECIAL")
        pc[pc$token %in% fcall, ]$text
      }
    }
    rmd <- rmds[grepl(bid, rmds)]
    stdr <- suppressMessages(knitr::purl(rmd, quiet = TRUE))
    student <- new.env()
    did_source_student <- robust_source(stdr, student, 2)

    # fcalls <- allowed_fun$fun
    # all_functions <- unique(fcalls)
    student_objs <- lapply(ls(envir = student), get, envir = student)
    student_obj_classes <- sapply(student_objs, class)
    student_obj_names <- ls(envir = student)

    student_functions <- student_objs[student_obj_classes == "function"]
    names(student_functions) <- student_obj_names[student_obj_classes == "function"]

    # check missing functions
    if (is.null(flist)) {
      flist <- setdiff(names(student_functions), exclude)
    }
    # check if any functions have illegal functions
    b <- lapply(flist, get_student_functions, envir = student)
    names(b) <- flist
    b
  }
  exclude <- NULL
  flist <- NULL
  res <- sapply(bids, process_one, exclude = exclude, flist = flist)
  # out <- sort(table(Reduce("c", res)))
  all_fn_names <- Reduce("union", sapply(res, names))
  q <- lapply(all_fn_names,
              function(fn) {
                sort(table(Reduce(`c`, sapply(res, function(s, fn) {
                  s[fn]
                }, fn))))
              })
  names(q) <- all_fn_names
  q
}

