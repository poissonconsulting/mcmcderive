abind <- function(x, x2, along, dimnames = TRUE) {
  x <- abind::abind(x, x2, along = along)
  if(!isTRUE(dimnames)) dimnames(x) <- NULL
  x
}

subset_mcmcarray_chains <- function(x, chains) {			
 x <- abind::asub(x, chains, 1L, drop = FALSE)			
  set_class(x, "mcmcarray")
}

subset_mcmcarray_iterations <- function(x, iterations) {			
  x <- abind::asub(x, iterations, 2L, drop = FALSE)	
  x <- apply(x, 3:ndims(x), FUN = identity)
  set_class(x, "mcmcarray")
}

subset_mcmcr_iterations <- function(x, iterations) {			
  x <- lapply(x, subset_mcmcarray_iterations, iterations = iterations)	  
  set_class(x, "mcmcr")
}

subset_mcmcr_chains <- function(x, chains) {			
  x <- lapply(x, subset_mcmcarray_chains, chains = chains)	  
  set_class(x, "mcmcr")
}

subset_mcmcr_parameters <- function(x, parameters) {
  x <- x[parameters]
  set_class(x, "mcmcr")
}

bind_iterations_mcmcarray <- function(x, x2, ...) {		
  x <- abind::abind(x, x2, along = 2)
  dimnames(x) <- NULL		
  set_class(x, "mcmcarray")			
}			


bind_iterations_mcmcr <- function(x, x2, ...) {			
  x <- mapply(x, x2, FUN = bind_iterations_mcmcarray, SIMPLIFY = FALSE)			
  set_class(x, "mcmcr")			
}
