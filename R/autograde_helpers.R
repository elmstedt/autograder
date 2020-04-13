dbg <- function(...) {
  if (parent.frame(7)$debug) {
    zz <- file("debug.log", open = "a+")
    sink(zz, append = TRUE)
    sink(zz, type = "message", append = TRUE)
    message(...)
    sink(type = "message")
    sink()
    closeAllConnections()
  }
}

sort_attr <- function(x) {
  ax <- attributes(x)
  if (!is.null(ax)) {
    ax <- ax[order(names(ax))]
    attributes(x) <- ax
  }
  x
}

`%=%` <- function(x, y) {
  round(x, 5) == round(y, 5)
}

round_char <- function(x, tol) {
  if (is.numeric(x)) {
    as.character(round(x, abs(log10(tol))))
  } else {
    as.character(x)
  }
}

close_enough <- function(x, y, tol = 1e-6) {
    if (identical(class(x), class(y))) {
      if (is.list(x)) {
        x_ <- rapply(x, round_char, how = "replace", tol = tol)
        y_ <- rapply(y, round_char, how = "replace", tol = tol)
        all(mapply(close_enough, x_, y_, tol))
      } else {
        identical(round_char(x, tol), round_char(y, tol))
      }
    } else {
      identical(round_char(x, tol), round_char(y, tol))
    }
  }


is_equivalent <- function(x, y, tol = 1e-12) {
  if (is.null(x) || is.null(y)) {
    return(FALSE)
  }
  x <- sort_attr(x)
  y <- sort_attr(y)

  if (identical(x, y)) {
    return(TRUE)
  }


  check_values <- mapply(close_enough, x, y, tol)
  if (!all(check_values)) {
    return(FALSE)
  }
  TRUE
}

equal <- function(x, y) {
  all(x == y)
}

approx_equal <- function(x, y, tol = 1e-10) {
  all(abs(x - y) < tol)
}

sequal <- function(x, y) {
  all(sort(x) == sort(y))
}

sapprox_equal <- function(x, y, tol = 1e-10) {
  all(abs(sort(x) - sort(y)) < tol)
}

same_length <- function(x, y) {
  length(x) == length(y)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
format.name <- function(x) {
  as.character(x)
}
