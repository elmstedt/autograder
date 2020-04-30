#' Title
#'
#' @param x
#' @param expr
#'
#' @return
#' @export
#'
#' @examples
scoring_function <- function(x, expr = scoring_expr){
  eval(as.expression(expr))
}

#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr tibble
#' @importFrom stringr str_remove_all
make_data_row <- function(a) {
  b <- a
  o <- order(b$question, b$part, b$subpart, b$possible, na.last = TRUE) #order the data so the questions and parts are in order and put the quibbles last.
  b <- b[o,]
  b$possible <- as.character(b$possible)
  duplicated_parts <- duplicated(b[c("question", "part")])
  if(any(duplicated_parts)){
    b[duplicated_parts, ]$part <- NA
  }
  if (any(duplicated(b$question))) {
    b[duplicated(b$question), ]$question <- NA
  }
  # b[is.na(b)] <- "&#8942;"
  if (any(is.na(b))) {
    b[is.na(b)] <- ""
  }
  # b[b == ""] <- "&#8942;"

  fmt <- "<tr>
  <td align=\"left\" valign=\"top\">
    %s
  </td>
  <td align=\"left\" valign=\"top\">
    %s
  </td>
  <td align=\"left\" valign=\"top\">
    %s
  </td>
  <td align=\"right\" valign=\"top\">
    %s
  </td>
  <td align=\"right\" valign=\"top\">
    %s
  </td>
  <td></td>
  <td align=\"left\" valign=\"top\">
    %s
  </td>
</tr>"
  args <- c(str_replace_all(fmt, "\\t", ""), b[-1])

  d <- do.call(what = "sprintf", args = args)
  dplyr::tibble(feedback = paste0(stringr::str_remove_all(string = d, pattern = "\n|\t"), collapse = ""))
}

#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom stringr str_replace_all str_remove_all
make_style_row <- function(a) {
  fmt <- "<tr><td align=\"left\" valign=\"top\" colspan=\"2\">%s</td><td align=\"right\" valign=\"top\">%s</td><td align=\"right\" valign=\"top\">%s</td><td valign=\"top\"><pre>%s</pre></td></tr>"
  args <- c(stringr::str_replace_all(fmt, "[\\t\\n]", ""), a)
  d <- do.call(what = "sprintf", args = args)
  paste0(stringr::str_remove_all(string = d, pattern = "\n|\t"), collapse = "")
  paste0(d, collapse = "")
}

#' Title
#'
#' @param style
#'
#' @return
#' @export
#'
#' @examples
make_table <- function(style) {
  paste0("<table>
            <tr>
              <th align=\"left\" colspan=\"2\">
              Issue Family
            </th>
            <th align=\"right\">Deduction</th>
            <th align=\"right\">Remedy</th>
            <th align=\"right\">Location(s) of Issue</th>
            <th></th></tr>", make_style_row(style), "</table>")
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
make_head_row <- function() {
  "<tr>
  <th align=\"left\">
    Question
  </th>
  <th align=\"left\">
    Part
  </th>
  <th align=\"left\">
    Sub-Part
  </th>
  <th align=\"left\">
    Available
  </th>
  <th align=\"left\">
    Received
  </th>
<th></th>
  <th align=\"left\">
    Notes
  </th>
</tr>"
}

#' Title
#'
#' @param a
#' @param r
#'
#' @return
#' @export
#'
#' @examples
make_raw_row <- function(a, r) {
  paste0(
    "<tr>
  <th align=\"left\">
    Raw Score
  </th>
  <th align=\"left\">

  </th>
    <th align=\"left\">


  <th align=\"right\"><b>",
    a,
    "</b></th>
  <th align=\"right\"><b>",
    r,
    "</b></th>
  <th></th>
  <th align=\"left\">
  </th>
</tr>"
  )
}

#' Title
#'
#' @param r
#' @param s
#' @param l
#'
#' @return
#' @export
#'
#' @examples
make_final_row <- function(r, s = 0, l = 0) {
  paste0("<tr><th align=\"left\" colspan=\"4\">Final Score</th><th align=\"right\"><b>", scoring_function(r) + s + l, "</b></th><th></th></tr>")
}

#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
make_style_adj <- function(a) {
  paste0("<tr><th  align=\"left\" colspan=\"4\">Style adjustment</th><th align=\"right\"><b>", a, "</b></th><th></th></tr>")
}

#' Title
#'
#' @param a
#'
#' @return
#' @export
#'
#' @examples
make_late_adj <- function(a) {
  paste0("<tr><th  align=\"left\" colspan=\"4\">Late Penalty</th><th align=\"right\"><b>", a, "</b></th><th></th></tr>")
}

#' Title
#'
#' @param expr
#' @param a
#' @param r
#'
#' @return
#' @export
#'
#' @examples
make_total_row <- function(expr, a, r) {
  paste0(
    "<tr>
  <th align=\"left\" colspan=\"3\">
    Total Score
  </th>
  <th align=\"right\"><b>",
    100,#scoring_function(x = as.numeric(a)),
    "</b></th>
  <th align=\"right\"><b>",
    scoring_function(x = as.numeric(r)),
    "</b></th>
  <th></th>
  <th align=\"left\">",
    format(scoring_expr),
    "</th>
</tr>"
  )
}

#' Make Feedback Reports for Students
#'
#' @param f feedback on problems
#' @param a the number of raw points available
#' @param r a vector of raw points received
#' @param s a vector of style deductions
#' @param scoring_expr the scaling function used to standardize the scores
#'
#' @return
#' @export
#'
#' @examples
make_report <- function(f, a, r, s, scoring_expr) {
  paste0("<table>", make_head_row(), f, make_raw_row(a, r), make_total_row(scoring_expr, a, r), make_style_adj(s), make_final_row(r, s), "</table>")
}
