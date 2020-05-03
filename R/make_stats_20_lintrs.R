#' Make Governing Styleguide
#'
#' This function creates a data.frame object which lists all of the lintrs
#' used to grade the style component of the homework, what the penalty is,
#' and groups them into style "families" to avoid repeated penalties for
#' slight variations of transgressions.
#'
#' TODO: generalize inputs. At a minimum should take a score argument.
#' This could perhaps be put into a package data object.
#'
#' @return
#' @export
#'
#' @examples
make_stats_20_lintrs <- function() {
  stats20_lintrs <- c("assignment_linter",
                      "closed_curly_linter",
                      "commas_linter",
                      "commented_code_linter",
                      "equals_na_linter",
                      "function_left_parentheses_linter",
                      "infix_spaces_linter",
                      "line_length_linter",
                      "object_length_linter",
                      "object_name_linter",
                      "object_usage_linter",
                      "open_curly_linter",
                      "paren_brace_linter",
                      "seq_linter",
                      "single_quotes_linter",
                      "spaces_inside_linter",
                      "spaces_left_parentheses_linter",
                      "trailing_blank_lines_linter",
                      "trailing_whitespace_linter",
                      "T_and_F_symbol_linter",
                      "semicolon_terminator_linter",
                      "undesirable_function_linter",
                      "undesirable_operator_linter",
                      "unneeded_concatenation_linter")
  stats20_lintrs <- data.frame(linter = stats20_lintrs, stringsAsFactors = FALSE)
  stats20_lintrs$score <- -c(5, 1, 3, 1, 5, 1, 3, 2, 1, 5, 1, 1, 1, 3, 5, 1, 1, 1, 1, 5, 5, 5, 5, 1)
  stats20_lintrs$family <- c("Assignment", "Curly Braces", "Whtespace", "Comments", "== NA", "Whtespace",
                             "Whtespace", "Line Length", "Object Name", "Object Name", "Object Use", "Curly Braces",
                             "Whtespace", "Colon Operator", "Single Quotes", "Whtespace", "Whtespace",  "Whtespace",
                             "Whtespace", "Undesirables", "Semicolon", "Undesirables", "Undesirables", "Undesirables")
  stats20_lintrs
}
