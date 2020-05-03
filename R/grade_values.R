# grade_value <- function(val, envirs) {
grade_value <- function(val, group, my, student) {
  with(val, {
    possible <- as.numeric(possible)
    obj_name <- gsub("\\*", "", object_name)
    check <- anything_equal(my[[obj_name]], student, fix_mode, tol)
    expected_val <- paste("<code>",
                          paste(utils::capture.output(my[[obj_name]]),
                                collapse = "<br/>"),
                          "</code>")
    fb <- ""
    score <- if (check) {
      if (grepl("\\*", object_name)) {
        possible
      } else {
        if (obj_name %in% attr(check, "objects")) {
          possible
        } else {
          fb <- paste(
            "Expected value:<br/>",
            expected_val,
            "<br/>in an object named <code>",
            obj_name,
            "</code> but found it in an object named <code>",
            attr(check, "objects")[[1]],
            "</code>.",
            sep = "")
          possible / 2
        }
      }
    } else {
      fb <- paste(
        "Expected value:<br/>",
        expected_val,
        "<br/>in an object named <code>",
        obj_name,
        "</code> but found after sourcing your file, it was not found.",
        sep = "")
      0
    }

    dplyr::tibble(
      question,
      part,
      subpart,
      possible,
      score,
      fb)
  })
}

