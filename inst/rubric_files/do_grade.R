detach("package:autograder", unload = TRUE)
library(autograder)
scoring_expr <- expression(2 * x + 10)
grades <- compile_grades(submission_dir = "subs",
                         results_file = "grades.csv",
                         grade_style = TRUE,
                         check_formals = TRUE,
                         max_runtime = 5,
                         debug = TRUE,
                         chapter_level = 4)
grades <- read.csv("grades.csv", stringsAsFactors = FALSE)
quantile(grades$grade)
review_grading(grades)
plot_grade_distribution(grades$grade, "Homework 3")
hist(grades$grade, breaks = seq(0, 100, 10))
