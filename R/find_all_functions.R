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
#' @importFrom dplyr bind_rows bind_cols as_tibble group_by summarise n
#' @importFrom knitr purl
#' @importFrom lintr get_source_expressions
#' @importFrom pbapply pblapply
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr bind_rows bind_cols as_tibble group_by summarise n
#' @importFrom knitr purl
#' @importFrom lintr get_source_expressions
#' @importFrom pbapply pblapply
#' @importFrom tidyr pivot_longer
find_all_functions <- function(hw_dir, exclude = NULL, flist = NULL, combine = FALSE, support_dir = "support_files") {
  bids <- dir(hw_dir, include.dirs = TRUE)
  rmds <- dir(hw_dir, pattern = "Rmd$|rmd$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  process_one <- function(bid, exclude, flist){
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
        pc <- lintr::get_source_expressions(temp)$expressions[[1]]$parsed_content
        fcall <- c("SYMBOL_FUNCTION_CALL", "SPECIAL")
        pc[pc$token %in% fcall, ]$text
      }
    }
    rmd <- rmds[grepl(bid, rmds)]
    tryCatch({
      stdr <- suppressMessages(knitr::purl(rmd, quiet = TRUE))
      student <- new.env()
      did_source_student <- robust_source(stdr, student, 2)
      unlink(stdr)  
    }, error = function(e){
      return(NULL)
    })
    
    suppressWarnings(file.copy(support_dir, getwd(), recursive = TRUE))
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
  res <- pbapply::pblapply(bids, process_one, exclude = exclude, flist = flist)
  if (combine) {
    # finds how many students used each function internally.
    table(Reduce(`c`, lapply(res, function(r) {
      unique(Reduce(`c`, r))
    })))
  } else {
    # finds how many students used each function internally in each function.
    all_funs <- unique(Reduce(`c`, lapply(res, names)))
    conformed <- dplyr::bind_rows(lapply(res, function(x) {
      whereami <- parent.frame()$i
      tryCatch({
        x <- lapply(x, unique)
        lens <- sapply(x, length)
        n <- max(lens)
        x <- mapply(`c`, x, lapply(n - lens, character),SIMPLIFY = FALSE)
        to_add <- setdiff(all_funs, names(x))
        ta <- rep(list(character(n)), length(to_add))
        names(ta) <- to_add
        w <- dplyr::bind_cols(dplyr::as_tibble(x), dplyr::as_tibble(ta))
        w %>%
          tidyr::pivot_longer(
            cols = names(w),
            names_to = "student_function",
            values_to = "used_function") -> q
        q[q[, "used_function", drop = TRUE] != "",]
      }, error = function(e){
        message(e, "\n", length(x), "\n", whereami)
      })
    }))
    conformed %>%
      dplyr::group_by(student_function, used_function) %>%
      dplyr::summarise(n = dplyr::n())
  }
}

