check_function_structure <- function() {
  funs <- c("f", "g")# function list
  fs <- data.frame(fun = funs,
                   exists = FALSE,
                   proper_formals = FALSE,
                   preferred_methods = FALSE,
                   handles_errors = FALSE,
                   stringsAsFactors = FALSE)

  # get a list B of all of the functions the student has written
  sfuns <- c("g", "h")# student functions
  fs[fs$fun %in% sfuns, "exists"] <- TRUE

}
