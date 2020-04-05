#' Title
#'
#' @param sub_dir
#' @param support_dir
#'
#' @return
#' @export
#'
#' @examples
knit_submissions <- function(sub_dir, support_dir) {
  support_files <- dir(support_dir, full.names = TRUE)
  loads <- support_files[grepl("Rdata", support_files, ignore.case = TRUE)]
  csvs <- support_files[grepl("csv", support_files, ignore.case = TRUE)]
  for (this_load in loads) {
    load(this_load)
  }
  for (csv in csvs) {
    readr::read_csv(csv)
  }

  # knit each file
  bids <- dir(sub_dir)
  bid <- bids[1]
  for (bid in bids) {
    this_dir <- file.path(sub_dir, bid)
    file.copy(support_files, this_dir, recursive = TRUE)

    rmd <- dir(this_dir, pattern = "Rmd", ignore.case = TRUE, full.names = TRUE)
    rmarkdown::render(rmd,
                      output_format = "html_document")
    setdiff(dir(this_dir, full.names = TRUE),
            dir(this_dir,
                pattern = "\\.rmd$|\\.html$|\\.r$",
                ignore.case = TRUE,
                full.names = TRUE)) -> to_drop
    unlink(to_drop)
  }
}
