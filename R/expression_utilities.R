#' Split expression on `&`
#'
#' @param expr an expression
#'
#' @return A list of expressions.
separate_into_conjuncts <- function(expr) {
  if (expr[[1]] == as.symbol("&")) c(Recall(expr[[2]]), expr[[3]])
  else list(expr)
}


#' Test if expression contain a symbol
#'
#' @param expr an expression
#' @param sym a symbol
#'
#' @return Boolean.
contains <- function(expr, sym) {# TODO?: early exit
  if (is.symbol(expr) && expr == sym) TRUE
  else if (length(expr) == 1) FALSE
  else any(vapply(expr, contains, logical(1), sym = sym, USE.NAMES = FALSE))
}


#' Join expressions with `&`
#'
#' @param exprs a list of expressions
#'
#' @return An expression.
conjunction <- function(exprs)
  Reduce(function(x, y) call("&", x, y), exprs)


#' Construct a loop call
#'
#' @param range expression of the form `<variable> %in% <range>`
#' @param filters a list of expressions
#' @param body an expression
#'
#' @return A loop expression.
construct_loop <- function(range,
                           filters,
                           body)
  call("for", range[[2]],
       if (length(filters) > 0) call("{", call("<-", range[[2]], range[[3]]),
                                     call("[", range[[2]], conjunction(filters)))
       else range[[3]],
       body)


#' Remove named argument from call
#'
#' @param expr a call expression
#' @param x character
#'
#' @return Call expression `expr` without argument `x`.
drop <- function(expr, x) {
  i <- which(names(expr) == x)
  if (length(i) > 0) expr[-i] else expr
}
