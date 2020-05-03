#' Title
#'
#' @param ch
#' @param envir
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom utils str
get_whitelist <- function(ch = 0) {
  ch1 <- c("getwd", "setwd", "q", "rm", "sqrt", "exp", "log", "objects", "ls")
  ch2 <- c("c", "length", "mode", "seq", "seq_along", "seq_len", "rep", "numeric",
           "character", "logical", "integer", "vapply", "isTRUE", "isFALSE",
           "sum", "prod", "mean", "sd", "var", "median", "IQR", "min", "max",
           "range", "diff", "cumsum", "cumprod",  "sort", "fivenum", "summary",
           "typeof", "dir", "rev", "as.character", "as.integer", "as.numeric",
           "as.logical", "sin", "cos", "tan", "asin", "acos", "atan", "rev")
  ch3 <- c("any", "all", "identical", "is.na", "is.nan", "is.null", "is.finite",
           "is.infinite", "which", "ifelse", "round", "floor", "ceiling",
           "trunc", "signif")
  ch4 <- c("stop", "warning", "message")
  ch5 <- c("matrix", "cbind", "rbind", "dim", "nrow", "ncol", "attributes",
           "list", "rownames", "colnames", "dimnames", "diag", "solve", "apply",
           "rowMeans", "colMeans", "t", "row", "col", "cor", "order")
  ch6 <- c("factor", "as.factor", "levels", "nlevels", "tapply")
  df <- as.data.frame(rbind(cbind(ch1, 1),
  cbind(ch2, 2),
  cbind(ch3, 3),
  cbind(ch4, 4),
  cbind(ch5, 5),
  cbind(ch6, 6)))
  names(df) <- c("fun", "ch")
  whitelist <- df
  whitelist$fun <- as.character(whitelist$fun)
  whitelist$ch <- as.integer(whitelist$ch)
  whitelist[whitelist["ch"] <= ch, "fun"]
}

#' Title
#'
#' @param ch
#'
#' @return
#' @export
#'
#' @examples
show_whitelist <- function(ch) {
  whitelist[whitelist[, "ch"] <= ch, "fun"]
}
