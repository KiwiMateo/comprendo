#' Append to a vector
#'
#' @param x a vector
#' @param y object
#'
#' @return NULL. Modifies the input vector.
#'
#' @examples
#'   x <- vector()
#'   for (i in 1:10) append(x, i)
#'   x
append <- function(x, y) {
  x <- substitute(x)
  y <- substitute(y)
  e <- parent.frame()

  eval(substitute(x[[length(x) + 1]] <- y,
                  list(x = x, y = y)),
       envir = e)
}


#' Zip vectors
#'
#' @param ... vectors of the same length
#'
#' @return A list.
#' @export
#'
#' @examples
#'   zip(letters, 1:26, 26:1)
zip <- function(...) {
  x <- list(...)
  if (any(lengths(x[-1]) != length(x[[1]])))
    stop("all elements must have the same length")
  lapply(seq_along(x[[1]]),
         function(i) lapply(x, `[[`, i))
}
