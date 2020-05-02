is_allowed <- function(x) {
  allowable <- c("na.rm", "na.last", "useNA",
                 "MARGIN", "arr.ind", "as.factor", LETTERS, "`%^%`", "%^%", "`%m%`", "%m%")
  x %in% allowable
}

is_infix <- function(x) {
  grepl("^`%.+%`$", x)
}

#' @importFrom lintr ids_with_token with_id Lint
absolute_path_linter2 <- function(source_file, lax = TRUE) {
  lapply(lintr::ids_with_token(source_file, "STR_CONST"), function(id) {
    token <- lintr::with_id(source_file, id)
    path <- do.call("file.path",
                    args = as.list(
                      sapply(
                        strsplit(token[["text"]], "/"),
                        function(b) {
                          gsub("\"", "", b)
                        }
                      )
                    )
    )
    if (lintr:::is_absolute_path(path) &&
        lintr:::is_valid_long_path(path, lax)) {
      start <- token[["col1"]] + 1L
      end <- token[["col2"]] - 1L
      lintr::Lint(
        filename = source_file[["filename"]], line_number = token[["line1"]],
        column_number = start, type = "warning",
        message = "Do not use absolute paths.",
        line = source_file[["lines"]][[as.character(token[["line1"]])]],
        ranges = list(c(start, end)), "absolute_path_linter"
      )
    }
  })
}

#' @importFrom lintr ids_with_token with_id Lint
my_unneeded_concatenation_linter <- function(source_file) {
  tokens <- source_file[["parsed_content"]] <- filter_out_token_type(source_file[["parsed_content"]],
                                                                     "expr")
  msg_empty <- "Unneded concatenation without arguments. Replace the \"c\" call by NULL."
  msg_const <- "Unneded concatenation of a constant. Remove the \"c\" call."
  lapply(lintr::ids_with_token(source_file, "SYMBOL_FUNCTION_CALL"),
         function(token_num) {
           num_args <- get_num_concat_args(token_num, tokens)
           if (num_args == 0L || num_args == 1L) {
             token <- lintr::with_id(source_file, token_num)
             start_col_num <- token[["col1"]]
             end_col_num <- token[["col2"]]
             line_num <- token[["line1"]]
             line <- source_file[["lines"]][[as.character(line_num)]]
             lintr::Lint(filename = source_file[["filename"]], line_number = line_num,
                  column_number = start_col_num, type = "warn",
                  message = if (num_args) {
                    msg_const
                  }
                  else {
                    msg_empty
                  }, line = line, linter = "unneeded_concatenation_linter",
                  ranges = list(c(start_col_num, end_col_num)))
           }
         })
}


#' @importFrom rex re_matches rex re_substitutes
#' @importFrom xml2 xml_find_all xml_find_first
object_name_linter2 <- function(styles = "snake_case") {
  .or_string <- function(xs) {
    len <- length(xs)
    if (len <= 1) {
      return(xs)
    }
    comma_sepd_prefix <- paste(xs[-len], collapse = ", ")
    paste(comma_sepd_prefix, "or", xs[len])
  }
  style_regexes <- list(
    "^(?:\\.)?[[:upper:]](?:[[:alnum:]])*$", "^(?:\\.)?[[:lower:]](?:[[:alnum:]])*$",
    "^(?:\\.)?[[:lower:][:digit:]]+[_[:lower:][:digit:]]*$",
    "^(?:\\.)?(?:[[:lower:][:digit:]])+(?:\\.(?:[[:lower:][:digit:]])+)*$",
    "^(?:\\.)?(?:[[:lower:][:digit:]])+$", "^(?:\\.)?(?:[[:upper:][:digit:]])+$"
  )
  names(style_regexes) <- c("CamelCase", "camelCase", "snake_case", "dotted.case", "lowercase", "UPPERCASE")
  # print(styles)
  # message(names(style_regexes))

  # styles <- match.arg(styles, names(style_regexes), several.ok = TRUE)
  lint_msg <- paste0(
    "Variable and function name style should be ",
    .or_string(styles), "."
  )

  check_style <- function(nms, style, generics = character()) {
    conforming <- rex::re_matches(nms, style_regexes[[style]])
    conforming[!nzchar(nms) | is.na(conforming) | is_allowed(nms) | is_infix(nms)] <- TRUE

    # if (any(!conforming)) {
    #   possible_s3 <- 
    #     rex::re_matches(
    #       nms[!conforming],
    #       rex::rex(rex:::capture(name = "generic", something),
    #                ".",
    #                rex:::capture(name = "method", something)))
    #   if (any(!is.na(possible_s3))) {
    #     has_generic <- possible_s3$generic %in% generics
    #     conforming[!conforming][has_generic] <- TRUE
    #   }
    # }
    conforming
  }
  # source_file <- "../2019_4_fall/stats20/homework/style_checker/file_uploads/test.R"
  function(source_file) {
    allowable <- c("na.rm", "na.last", "useNA",
                                "MARGIN", "arr.ind", "as.factor", LETTERS, "`%^%`", "%^%")
    x <- lintr:::global_xml_parsed_content(source_file)
    if (is.null(x)) {
      return()
    }
    xpath <- paste0(
      "//expr[SYMBOL or STR_CONST][following-sibling::LEFT_ASSIGN or following-sibling::EQ_ASSIGN]/*",
      " | ", "//expr[SYMBOL or STR_CONST][preceding-sibling::RIGHT_ASSIGN]/*",
      " | ", "//SYMBOL_FORMALS"
    )
    assignments <- xml2::xml_find_all(x, xpath)
    strip_names <- function (x) {
      x <- rex::re_substitutes(x, rex::rex(start, some_of(".", quote, "$", "@")), "")
      x <- rex::re_substitutes(x, rex::rex(some_of(quote, "<", "-", "$", "@"), end), "")
      x
    }
    # nms <- lintr:::strip_names(as.character(xml2::xml_find_first(assignments, "./text()")))
    nms <- strip_names(as.character(xml2::xml_find_first(assignments, "./text()")))


    generics <- c(
      lintr:::declared_s3_generics(x), lintr:::namespace_imports()$fun,
      names(.knownS3Generics), .S3PrimitiveGenerics, ls(baseenv()), allowable
    )
    styles <- "snake_case"
    style_matches <- lapply(styles, function(x) {
      check_style(nms, x, generics)
    })
    matches_a_style <- Reduce(`|`, style_matches)
    lapply(
      assignments[!matches_a_style], lintr:::object_lint2,
      source_file, lint_msg, "object_name_linter"
    )
  }
}

#' Make the my_lintrs Object
#'
#' TODO: clean and generalize.
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom lintr with_defaults T_and_F_symbol_linter 
#'   semicolon_terminator_linter undesirable_function_linter
#'   undesirable_operator_linter unneeded_concatenation_linter default_linters
#'   default_undesirable_functions
make_my_lintrs <- function(){
  all_lintrs <- c(
    "T_and_F_symbol_linter", "assignment_linter",
    "closed_curly_linter", "commas_linter",
    "commented_code_linter", "todo_comment_linter",
    "cyclocomp_linter", "object_name_linter",
    "object_length_linter", "camel_case_linter",
    "equals_na_linter", "extraction_operator_linter",
    "function_left_parentheses_linter", "implicit_integer_linter",
    "infix_spaces_linter", "line_length_linter", "no_tab_linter",
    "object_usage_linter", "open_curly_linter",
    "paren_brace_linter", "absolute_path_linter",
    "nonportable_path_linter", "pipe_continuation_linter",
    "semicolon_terminator_linter", "seq_linter",
    "single_quotes_linter", "spaces_inside_linter",
    "spaces_left_parentheses_linter", "trailing_blank_lines_linter",
    "trailing_whitespace_linter", "undesirable_function_linter",
    "undesirable_operator_linter", "unneeded_concatenation_linter"
  )

  extra_lints <- c(
    "lintr::T_and_F_symbol_linter",
    "lintr::semicolon_terminator_linter",
    "lintr::undesirable_function_linter",
    "lintr::undesirable_operator_linter",
    "lintr::unneeded_concatenation_linter"
  )

  new_lintrs <- setdiff(all_lintrs, names(lintr::default_linters))
  new_lintrs <- paste0("lintr::", new_lintrs)
  # library(lintr)
  extra_lints <- sapply(c(new_lintrs[c(1, 8:11)]), function(l) eval(str2lang(l)))
  names(extra_lints) <- gsub("lintr::", "", names(extra_lints))
  # extra_lints <- sapply(c(new_lintrs[c(1, 8:11)]), function(l) get(l))
  my_lintrs <- do.call("with_defaults", args = extra_lints)
  my_lintrs[["object_name_linter"]] <- object_name_linter2(styles = "snake_case")
  my_lintrs[["cyclocomp_linter"]] <- NULL
  # my_lintrs[["cyclocomp_linter"]] <- cyclocomp_linter(complexity_limit = 10)
  my_lintrs[["no_tab_linter"]] <- NULL
  my_lintrs[["pipe_continuation_linter"]] <- NULL

  body(my_lintrs[["unneeded_concatenation_linter"]]) <- body(my_unneeded_concatenation_linter)
  undesireable_functions <- lintr::default_undesirable_functions
  undesireable_functions$print <- "use message() to output information to the console. print() should generally never be used to return a value."
  undesireable_functions$return <- "structure your code to return the last evaluated statement in the function body."
  # undesireable_functions$library <- NULL
  undesireable_functions$ifelse <- NULL
  undesireable_functions$par <- NULL
  # undesireable_functions$sapply <- NULL
  # undesireable_functions$source <- NULL
  undesireable_functions$setwd <- "do not change the working directory in your R Markdown Files."

  my_lintrs[["undesirable_function_linter"]] <- lintr::undesirable_function_linter(fun = undesireable_functions)
  # make outside functions lintr
  #outside_functions <-
  #my_lintrs[["outside_functions"]] <- undesirable_function_linter(fun = undesireable_functions)
  my_lintrs
}

