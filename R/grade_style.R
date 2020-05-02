#' @importFrom lintr lint
grade_style <- function(rmd, my_lintrs, class_lintrs) {
  lints <- lintr::lint(rmd, linters = my_lintrs)
  ldf <- as.data.frame(lints)
  linter_names <- ldf$linter
  linter_names[!grepl(pattern = "_linter", x = linter_names)] <- paste0(linter_names[!grepl(pattern = "_linter", x = linter_names)], "_linter")
  ldf$linter <- linter_names

  ldf <- merge(ldf, class_lintrs, all.x = TRUE, all.y = FALSE)
  process_lints(ldf)
}

#' Evaluate a List of Files for Style Violations
#'
#' @param bid the Bruin ID of the student
#' @param rmds a vector of Rmd file paths
#' @param my_lintrs the style lintrs to use
#' @param stats20_lintrs a data frame defining lintr groups and deductions.
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr tibble
#' @importFrom knitr purl
grade_all_style <- function(bid, rmds, my_lintrs, stats20_lintrs) {
  # message(bid)
  rmd <- rmds[grepl(bid, rmds)]
  tryCatch({
    stdr <- suppressMessages(knitr::purl(rmd, quiet = TRUE, documentation = 0))
    style <- grade_style(stdr, my_lintrs, stats20_lintrs)
    unlink(stdr)
    if (length(style) == 3) {
      style <- cbind(family = "All", style)
    }
    style_score <- sum(as.numeric(style$score), na.rm = TRUE)
    dplyr::tibble(bid = bid, style_score = style_score, style_table = make_table(style))},
    error = function(e) {
      style_error <- dplyr::tibble(family = "Parsing Error", score = -100, message = as.character(e), where = "Unknown.")
      dplyr::tibble(bid = bid, style_score = -100, style_table = make_table(style_error))
    }
  )
}
