#' Title
#'
#' @param scores
#' @param item
#' @param binwidth
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom graphics hist par plot.new plot.window rect axis title mtext
#' @importFrom stats median sd
plot_grade_distribution <- function(scores, item = "", binwidth = 10) {
  med <- stats::median(scores)
  s <- stats::sd(scores)
  pos <- med + c(-4:4) * s
  lb <- max(pos[pos < min(scores)])
  ub <- min(pos[pos > max(scores)])

  low <- binwidth * floor(lb / binwidth)
  bot <- binwidth * floor(min(scores) / binwidth)
  high <- binwidth * ceiling(ub / binwidth)

  keeps <- pos > low & pos < high
  pos <- pmax(pmin(pos, 100), bot)
  scores[scores == min(scores)] <- scores[scores == min(scores)] + 0.01
  hs <- graphics::hist(scores,
             breaks = seq(bot, 100, binwidth),
             plot = FALSE)
  top <- max(hs$density)
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(opar))
  graphics::par(mar = c(5, 4, 7, 2) + 0.1)
  marks <- unique(pos)
  sds <- seq_along(marks) - which(marks == med)
  cols <- c("grey20", "grey40", "red", "orange",
            "darkgreen", "darkblue", "blue", "pink")[keeps]
  graphics::par(lwd = 3)
  graphics::plot.new()

  graphics::plot.window(xlim = range(pos), ylim = c(0, top))

  for (i in seq_along(marks[-1])) {
    graphics::rect(marks[i], 0,
                   marks[i + 1], top,
                   border = NA, col = cols[i], density = 100)
  }
  graphics::hist(scores,
       breaks = seq(bot, 100, binwidth),
       freq = FALSE, include.lowest = TRUE,
       add = TRUE)
  graphics::axis(1)
  graphics::title(main = c("Grade Distribution", item), xlab = "Score")

  sds <- sds[marks < 100][-1]
  cols <- cols[marks < 100][-1]
  marks <- marks[marks < 100][-1]

  graphics::mtext(c(paste(sds[sds < 0], "sd"),
                    "Median",
                    paste0("+", sds[sds > 0], " sd"), round(marks)),
        3,
        line = rep(1:0, each = length(marks)),
        at = marks,
        cex = rep(c(1.5, 1), each = length(marks)),
        col = cols)
}
