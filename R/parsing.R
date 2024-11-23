#' Check comprehension is well formed
#'
#' @param expr a call to comprehension
#'
#' @return NULL. Throws an error if there's an issue.
check_comprehension <- function(expr) {
  if (expr[[1]] != as.symbol("|"))
    stop("comprehension needs a \"|\"")
  expr <- expr[[3]]
  while(TRUE) {
    if (expr[[1]] == as.symbol("%in%")) break
    if (expr[[1]] != as.symbol("&") || expr[[3]][[1]] != as.symbol("%in%"))
      stop("range not well defined")
    expr <- expr[[2]]
  }
}


#' Group filter conditions by when they can be applied
#'
#' @param filters list of expressions
#' @param vars character vector of variables used in filters
#'
#' @return A named list with filters that can only be applied once the named
#' variable is introduced.
organise_filters <- function(filters,
                             vars) {
  if (length(filters) == 0) return(list())

  rev_vars <- rev(vars)
  rank <- vapply(filters,
                 function(expr) 1 + length(vars) - which.max(vapply(rev_vars,
                                                                    contains,
                                                                    logical(1),
                                                                    expr = expr,
                                                                    USE.NAMES = FALSE)),
                 numeric(1), USE.NAMES = FALSE)
  tapply(filters, vars[rank], list)
}


#' Parse a comprehension call
#'
#' @param call expression, giving a comprehension call
#'
#' @return List with values calculation, vars, ranges, and filters; vars is a
#'   character vector, the other values are expressions (calculation) or lists
#'   of expressions (ranges, filters); elements of the ranges list have the form
#'   `<variable> %in% <range>`.
parse_comprehension <- function(call) {
  expr <- call[[2]]
  check_comprehension(expr)

  filters <- as.list(call[-(1:2)])
  ranges  <- separate_into_conjuncts(expr[[3]])
  vars    <- as.character(lapply(ranges, `[[`, 2))

  list(calculation = expr[[2]],
       vars        = vars,
       ranges      = structure(ranges, names = vars),
       filters     = organise_filters(filters, vars))
}
