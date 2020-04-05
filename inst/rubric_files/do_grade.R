library(autograder)
scoring_expr <- expr(3 * x + 25)
grades <- compile_grades(results_file = "grades.csv",
                         check_style = TRUE,
                         check_formals = FALSE)
grades <- read_csv("grades.csv")
review_grading(grades)
