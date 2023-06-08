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
            as.list(x)[-(1:2)],
            rlang::as_string(iteration_var),
            iteration_removal
          )
          fun1 <- rlang::call2(rlang::expr(cbind), !!!args1)
          return(rlang::call2(x[[1]], x[[2]], fun1))
        }
      } else if (x[[1]] == "for" || x[[1]] == "sum") {
        rlang::abort("Not translating", class = "mcmcderive_unhandled_expr")
      }

      # Use base R variant because map2() catches all errors
      args <- mapply(
        as.list(x)[-1],
        rlang::as_string(iteration_var),
        FUN = iteration_removal
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
#' expression_vectorize(rlang::expr(for(i in 1:nObs) {eCount[i] <- b0}))
#' expression_vectorize(rlang::expr(for(i in 1:length(LogLength)) {eWeightLength[i] <- b0 + bDayte * Dayte[i]}))
#' expression_vectorize(rlang::expr(for(i in 1:nObs) {eAnnual[i] <- bAnn[Ann[i]] + bSA[Site[i], Ann[i]]}))
expression_vectorize <- function(x) {
  if (x[[1]] == "for") {
    out <- tryCatch(
      iteration_removal(x = x[[4]], iteration_var = x[[2]]),
      mcmcderive_unhandled_expr = function(e) {
        x
      }
    )

    if (length(out) == 2 && out[[1]] == "{") {
      out <- out[[2]]
    }

    out
  } else if (x[[1]] == "{") {
    args <- purrr::map(as.list(x)[-1], expression_vectorize)
    rlang::call2(x[[1]], !!!args)
  } else {
    x
  }
}
