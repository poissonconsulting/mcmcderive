# Inspiration from Advanced R
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

iteration_removal <- function(x, iteration_var) {
  switch_expr(
    x,
    # Base cases
    constant = x,
    symbol = {
      x
    },
    # Recursive cases
    call = {
      # go into [ to remove i's or add cbind
      if (x[[1]] == "[") {
        # remove the iteration variable i
        if (x[[3]] == iteration_var) {
          return(x[[2]])
        }
        # cbind switch for [ with multiple arguments
        if (length(x) > 3) {
          args1 <- purrr::map2(
            as.list(x)[c(3, 4)],
            rlang::as_string(iteration_var),
            iteration_removal
          )
          fun1 <- rlang::call2(rlang::expr(cbind), !!!args1)
          return(rlang::call2(x[[1]], x[[2]], fun1))
        }
      }
      args <- purrr::map2(
        as.list(x)[-1],
        rlang::as_string(iteration_var),
        iteration_removal
      )
      rlang::call2(x[[1]], !!!args)
    },
    pairlist = {
      x
    }
  )
}

#' Convert New Expression
#'
#' Takes an expression and removes the for loop and adds `cbind` for arrays.
#'
#' @param x An expression
#'
#' @return An expression
#' @export
#'
#' @examples
#' expression_convert(rlang::expr(for(i in 1:nObs) {eCount[i] <- b0}))
#' expression_convert(rlang::expr(for(i in 1:length(LogLength)) {eWeightLength[i] <- b0 + bDayte * Dayte[i]}))
#' expression_convert(rlang::expr(for(i in 1:nObs) {eAnnual[i] <- bAnn[Ann[i]] + bSA[Site[i], Ann[i]]}))
expression_convert <- function(x) {
  if (x[[1]] == "for") {
    out <- iteration_removal(x = x[[4]], iteration_var = x[[2]])

    if (length(out) == 2 && out[[1]] == "{") {
      out <- out[[2]]
    }
    out
  } else if (x[[1]] == "{") {
    args <- purrr::map(as.list(x)[-1], expression_convert)
    rlang::call2(x[[1]], !!!args)
  } else {
    x
  }
}
