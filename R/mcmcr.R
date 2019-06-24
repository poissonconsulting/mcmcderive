subset_mcmcarray <- function(x, chains = NULL, iterations = NULL) {			
  if (!is.null(chains)) x <- abind::asub(x, chains, 1L, drop = FALSE)			
  if (!is.null(iterations)) x <- abind::asub(x, iterations, 2L, drop = FALSE)			
  set_class(x, "mcmcarray")
}

subset_mcmcr <- function(x, chains = NULL, iterations = NULL) {			
  x <- lapply(x, subset_mcmcarray, chains = chains, iterations = iterations)	  
  set_class(x, "mcmcr")
}

subset_mcmcr_parameters <- function(x, parameters = NULL) {
  x <- x[parameters]
  set_class(x, "mcmcr")
}

estimates_mcmcarray <- function(object) {			
  apply(object, 3:ndims(object), FUN = identity)			
}			

estimates_mcmcr <- function(object) {			
  lapply(object, estimates_mcmcarray)			
}

bind_iterations_mcmcr <- function(x, x2, ...) {			
  x <- mapply(x, x2, FUN = bind_iterations, SIMPLIFY = FALSE)			
  set_class(x, "mcmcr")			
}
