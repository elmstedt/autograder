#' Title
#'
#' @param lints
#'
#' @return
#' @export
#'
#' @examples
as.data.frame.lints <- function(lints) {
  lints <- lapply(lints, function(x) {
    lapply(x, function(y) {
      ifelse(is.null(y), "", y)
    })
  })
  class(lints) <- "list"
  lapply(lints, function(x) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    names(x) <- c(
      "filename", "line_number", "column_number",
      "type", "message", "line", "ranges", "linter"
    )
    x
  }) -> q
  Reduce(rbind, q)
}

#' Process a Single lintr() Report
#'
#' @param x
#'
#' @return a tibble to be used to generate style feedback.
#' @export
#'
#' @examples
#' @importFrom dplyr tibble group_by summarise mutate select
process_lints <- function(x) {
  q <- as.data.frame(x)
  if (nrow(q) == 0) {
    return(dplyr::tibble(message = "No style issues found.", score = 0, where = ""))
  }
  q <- q[, -2]
  q_idx <- !duplicated(q[, c("line", "column_number", "message")])
  qq <- q[q_idx, ]
  point_adj <- qq %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(score = min(score))

  p <- qq %>%
    dplyr::group_by(message, line) %>%
    dplyr::mutate(columns = paste(column_number, collapse = ", "))

  p_idx <- !duplicated(p[, c("line", "columns", "message")])
  pp <- p[p_idx, ]
  r <- pp %>%
    dplyr::mutate(line_where = paste("Line ",
                              line_number,
                              ": ",
                              columns,
                              sep = ""
    )) %>%
    dplyr::group_by(message, line) %>%
    dplyr::mutate(where = paste(line_where, collapse = "</br>"))
  rr <- unique(r[, c("family", "score", "message", "line", "where")])
  rr$line_where <- paste(rr$where, "</br>", rr$line, sep = "")
  s <- rr %>%
    dplyr::group_by(message) %>%
    dplyr::mutate(where = paste(line_where,
                         sep = "</br>",
                         collapse = "</br></br>"
    )) %>%
    dplyr::select(c("family", "score", "message", "where")) %>%
    unique()
  s <- s[order(s$family),]
  dup_idx <- duplicated(s$family)
  class(s$score) <- "character"
  s[dup_idx, c("family", "score")] <- ""
  s
}
