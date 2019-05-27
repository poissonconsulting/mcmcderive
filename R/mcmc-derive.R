#' MCMC Derive
#'
#' Calculate derived parameters for an MCMC object.
#' 
#' In the case of an mcmc_data object, values includes the vectors 
#' in the associated data frame. 
#' The mcmcr parameter is named estimate (and this is the default variable to monitor).
#'
#' @param object The MCMC object.
#' @param expr A string of the R expression to evaluate.
#' @param values A named list of additional values to evaluate in the R expression.
#' @param monitor A regular expression specifying the new variables to monitor.
#' @param parallel A flag indicating whether to derive samples in parallel using foreach backend.
#' @param ... Unused.
#' @return An \code{\link[mcmcr]{mcmcr}} object of the derived parameter(s).
#' @export
#' @examples
#' mcmc_derive(mcmcr::mcmcr_example, "prediction <- (alpha + beta) / sigma")
mcmc_derive <- function(object, ...) {
  UseMethod("mcmc_derive")
}

#' @export
mcmc_derive.default <- function(object, expr, values = list(), monitor = ".*", parallel = FALSE, ...) {
  check_unused(...)
  mcmc_derive(as.mcmcr(object), expr = expr, values = values, monitor = monitor, parallel = parallel) 
}

#' @describeIn mcmc_derive MCMC Derive for an mcmcr object
#' @export
mcmc_derive.mcmcr <- function(object, expr, values = list(), monitor = ".*", parallel = FALSE, ...) {
  check_string(expr)
  check_list(values)
  check_string(monitor)
  check_flag(parallel)
  check_unused(...)
  
  values <- convert_values(values)

  parameters <- parameters(object)
  names_values <- names(values)
  variables_expr <- all.vars(parse(text = expr))

  if (length(values)) {
    if (is.null(names_values)) err("values must be named")
    if (anyDuplicated(names_values)) err("values names must be unique")

    parameters <- parameters[!parameters %in% names_values]

    values <- values[intersect(names_values, variables_expr)]
  }

  parameters <- intersect(parameters, variables_expr)

  if (!length(parameters)) err("expr must include at least one object parameter")

  object <- subset(object, parameters = parameters)

  variables_expr <- setdiff(variables_expr, parameters)
  variables_expr <- setdiff(variables_expr, names(values))

  if (!length(variables_expr)) err("expr must include at least one new variable")

  values[variables_expr] <- NA

  if (!length(variables_expr[grepl(monitor, variables_expr)]))
    err("monitor '", monitor, "' must match at least one new variable in expr\n", expr)

  monitor <- variables_expr[grepl(monitor, variables_expr)]

  monitor <- sort(monitor)

  object <- plyr::llply(1:nchains(object), derive_chain, object = object,
                     .parallel = parallel, expr = parse(text = expr),
                  values = values, monitor = monitor)

  object <- Reduce(bind_chains, object)

  if (anyNA(object))
    err("monitor '", monitor, "' must not include missing values in expr\n", expr)
  object
}

#' @describeIn mcmc_derive MCMC Derive for an mcmc_data object
#' @export
mcmc_derive.mcmc_data <- function(object, expr = "prediction <- estimate", 
                                  values = list(), 
                                  monitor = "prediction", parallel = FALSE, ...) {
  check_string(expr)
  check_string(monitor)
  check_flag(parallel)
  check_unused(...)
  
  data <- as.data.frame(object)
  mcmc <- as.mcmcr(object)
  parameters(mcmc) <- "estimate"
  values <- c(values, as.list(data))
  mcmc <- mcmc_derive(mcmc, expr = expr, monitor = monitor, values = values, 
                      parallel = parallel)
  mcmc_data(mcmc, data)
}
