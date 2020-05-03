#' Title
#'
#' @param f
#' @param bad_fun
#' @param envir
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom lintr get_source_expressions
get_illegal_functions <- function(f, bad_fun = character(0), envir = globalenv()) {
  sf <- tryCatch(get(f, envir = envir),
                 error = function(e) {
                   NULL
                 })
  if (is.null(sf)) {
    character(0)
  } else {
    bod <- body(sf)
    temp <- tempfile()
    writeLines(c("f <- function() {", as.character(bod)[-1], "}"), temp)
    fcall <- "SYMBOL_FUNCTION_CALL"
    pc <- lintr::get_source_expressions(temp)$expressions[[1]]$parsed_content

    illegal <- intersect(pc[pc$token %in% fcall, ]$text, bad_fun)
    illegal
  }
}

#' Title
#'
#' @param f
#' @param whitelist
#' @param envir
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom lintr get_source_expressions
check_whitelist <- function(sf, whitelist, envir = globalenv()) {
    bod <- body(sf)
    temp <- tempfile()
    writeLines(c("f <- function() {", as.character(bod)[-1], "}"), temp)
    fcall <- "SYMBOL_FUNCTION_CALL"
    pc <- lintr::get_source_expressions(temp)$expressions[[1]]$parsed_content
    setdiff(pc[pc$token %in% fcall, ]$text, whitelist)
  # }
}

#' Title
#'
#' @param f
#' @param loops_allowed
#' @param envir
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom lintr get_source_expressions
get_illegal_loops <- function(f, loops_allowed, envir = globalenv()) {
  if (isFALSE(loops_allowed)) {
    sf <- tryCatch(get(f, envir = envir),
                   error = function(e) {
                     NULL
                   })
    if (is.null(sf)) {
      integer(0)
    } else {
      bod <- body(sf)
      temp <- tempfile()
      writeLines(c("f <- function() {", as.character(bod)[-1], "}"), temp)
      loops <- c("FOR", "WHILE", "REPEAT")
      pc <- lintr::get_source_expressions(temp)$expressions[[1]]$parsed_content
      unique(pc[pc$token %in% loops, ]$text)
    }
  } else {
    character(0)
  }
}

#' Title
#'
#' @param f
#' @param if_allowed
#' @param envir
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom lintr get_source_expressions
get_illegal_conditionals <- function(f, if_allowed, envir = globalenv()) {
  if (isFALSE(if_allowed)) {
    sf <- tryCatch(get(f, envir = envir),
                   error = function(e) {
                     NULL
                   })
    if (is.null(sf)) {
      integer(0)
    } else {
      bod <- body(sf)
      temp <- tempfile()
      writeLines(c("f <- function() {", as.character(bod)[-1], "}"), temp)
      ifs <- c("IF")
      pc <- lintr::get_source_expressions(temp)$expressions[[1]]$parsed_content
      unique(pc[pc$token %in% ifs, ]$text)
    }
  } else {
    character(0)
  }
}
