#' MCMC Derive
#'
#' Generate an mcmcr object of derived parameter(s) from an original MCMC object.
#'
#' @param object The original MCMC object (which is converted to an mcmcr object using \code{\link[mcmcr]{as.mcmcr}()}).
#' @param expr A string of the R code defining the values of the derived parameter(s) with respect to the parameters in object.
#' @param values A named list of additional R objects to evaluate in the R expression.
#' @param monitor A regular expression specifying the derived parameter(s) in expr.
#' @param silent A flag specifying whether to suppress warnings.
#' @param ... Unused.
#' @return An \code{\link[mcmcr]{mcmcr}} object of the derived parameter(s).
#' @export
#' @examples
#' mcmc_derive(mcmcr::mcmcr_example, "prediction <- (alpha + beta) / sigma")
#' 
#' expr <- "
#'  log(alpha2) <- alpha
#'  gamma <- sum(alpha) * sigma"
#'  
#' mcmc_derive(mcmcr::mcmcr_example, expr)
#' 
#' mcmc_derive(mcmcr::mcmcr_example, expr, monitor = "gamma")
#' 
mcmc_derive <- function(object, ...) {
  UseMethod("mcmc_derive")
}

#' @describeIn mcmc_derive MCMC Derive for an object that can be coerced to an mcmcr object
#' @export
mcmc_derive.default <- function(object, expr, values = list(), monitor = ".*", silent = FALSE, ...) {
  check_unused(...)
  mcmc_derive(as.mcmcr(object), expr = expr, values = values, monitor = monitor, silent = silent) 
}

#' @describeIn mcmc_derive MCMC Derive for an mcmcr object
#' @export
mcmc_derive.mcmcr <- function(object, expr, values = list(), monitor = ".*", silent = FALSE, ...) {
  check_mcmcr(object)
  check_string(expr)
  check_list(values)
  check_string(monitor)
  check_flag(silent)
  check_unused(...)
  
  if (length(values)) {
    check_named(values, unique = TRUE)
    object <- drop_overridden_parameters(object, values, silent = silent)
    values <- drop_absent_values(values, expr, silent = silent)
  }
  object <- drop_absent_parameters(object, expr, silent = silent)
  values <- add_new_variables(values, object, expr, silent = silent)
  monitor <- monitor_variables(monitor, values)
  
  derive(object, expr, values, monitor)
}
