
subset_mcmcarray <- function(x, chains = NULL, iterations = NULL) {			
  if (!is.null(chains)) x <- abind::asub(x, chains, 1L, drop = FALSE)			
  if (!is.null(iterations)) x <- abind::asub(x, iterations, 2L, drop = FALSE)			
  class(x) <- "mcmcarray"			
  x			
}

subset_mcmcr <- function(x, chains = NULL, iterations = NULL) {			
  x <- lapply(x, subset_mcmcarray, chains = chains, iterations = iterations)			
  class(x) <- "mcmcr"			
  x			
}

subset_mcmcr_parameters <- function(x, parameters = NULL) {
  x <- x[parameters]
  class(x) <- "mcmcr"
  x
}

estimates_mcmcarray <- function(object, fun = stats::median) {			
  apply(object, 3:ndims(object), FUN = .estimates, fun = fun)			
}			

estimates_mcmcr <- function(object, fun = stats::median) {			
  lapply(object, estimates_mcmcarray, fun = fun)			
}
