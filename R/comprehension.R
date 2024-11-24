#' Construct comprehension loop
#'
#' @param components output of `parse_comprehension`
#' @param var character, giving name of one comprehension variable
#' @param body expression, body of the comprehension
#'
#' @return A loop expression.
loop <- function(components, var, body)
  split(construct_loop(components[["ranges"]][[var]],
                       components[["filters"]][[var]],
                       body),
        as.symbol(var))


#' Deal with joined variables
#'
#'   Converts a comprehension loop with range over more than one variable (like
#'   `c(x, y) %in% zip(a, b)`) into one over a single variable.
#'
#' @param loop_expr a loop expression from `loop`
#' @param new_var character, giving name for new loop variable
#'
#' @return A loop expression.
split <- function(loop_expr,
                  new_var) {
  if (is.symbol(loop_expr[[2]])) loop_expr
  else if (loop_expr[[2]][[1]] == as.symbol("c")) {
    vars <- as.list(loop_expr[[2]][-1])
    loop_expr[[2]] <- new_var
    do.call("substitute", list(loop_expr,
                               lapply(structure(seq_along(vars),
                                                names = as.character(vars)),
                                      function(i) call("[[", new_var, i))))
  } else
    stop("invalid range")
}


#' Convert comprehension call into loop code
#'
#' @param comprehension_call expression, a comprehension call
#'
#' @return An expression.
make_comprehension_code <- function(comprehension_call) {
  components <- parse_comprehension(comprehension_call)
  vars <- rev(components[["vars"]])
  cll  <- loop(components, vars[1],
               call("append", as.symbol("out"), components[["calculation"]]))
  for (var in vars[-1])
    cll <- loop(components, var, cll)

  cll
}


#' Comprehension
#'
#' @param comprehension_call expression, a comprehension call
#' @param type character, giving the type of the output
#' @param e environment for execution
#'
#' @return A vector of mode `type`.
comprehend <- function(comprehension_call,
                       type,
                       e) {
  env <- new.env(parent = e)
  env$out <- vector(type)
  eval(make_comprehension_code(comprehension_call), envir = env)
  env$out
}


#' Comprehension
#'
#'   A Python like list comprehension; `.` has a type argument for the output
#'   (with default value "list"), `l` returns a list and `v` returns a
#'   homogeneous vector.
#'
#' @param comprehension a comprehension expression of the form
#'     `<calculation> | <range 1> [& <range2> ...]`
#'   where each `<range>` has the form
#'     `<variable> %in% <vector>`
#' @param ... filter conditions
#' @param type character, giving the type of the output vector
#'
#' @return A vector of mode `type`, possibly coerced to a more general type.
#' @export
#'
#' @examples
#'   .(x^2 | x %in% 1:10)
#'   .(x^2 | x %in% 1:10, x %% 3 == 0, type = "numeric")
#'   .(c(x, y, z) | x %in% 1:20 & y %in% x:20 & z %in% y:20,
#'     x^2 + y^2 == z^2)
#'   .(x + y + z | c(x, y, z) %in% zip(1:10, 10:1, c(-5:-1, 1:5)))
#'
#'   l(c(x, y, z) | x %in% 1:20 & y %in% x:20 & z %in% y:20,
#'     x^2 + y^2 == z^2)
#'   v(x^2 | x %in% 1:10, x %% 3 == 0)
`.` <- function(comprehension,
                ...,
                type = "list") {
  e <- parent.frame()
  comprehend(drop(match.call(), "type"), type, e)
}

#' @rdname .
#' @export
l <- function(comprehension, ...) {
  e <- parent.frame()
  comprehend(match.call(), "list", e)
}

#' @rdname .
#' @export
v <- function(comprehension, ...) {
  e <- parent.frame()
  comprehend(match.call(), "logical", e)
}
