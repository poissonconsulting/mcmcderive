split_apply_combine_sample <- function(i, object, expr, values, monitor) {
  object <- subset_mcmcr(object, iterations = i)
  object <- estimates_mcmcr(object)
  object <- c(object, values)
  object <- within(object, eval(expr))
  object <- object[monitor]
  object <- lapply(object, function(x) { dim(x) <- c(1L, 1L, dims(x)); class(x) <- "mcmcarray"; x})
  
  class(object) <- "mcmcr"
  
  object
}

split_apply_combine_chain <- function(i, object, expr, values, monitor) {
  object <- subset_mcmcr(object, chains = i)
  
  object <- lapply(1:niters(object), FUN = split_apply_combine_sample, object = object,
                   expr = expr, values = values, monitor = monitor)
  object <- Reduce(bind_iterations, object)
  object
}

split_apply_combine <- function(object, expr, values, monitor, parallel) {
  
  object <- plyr::llply(1:nchains(object), split_apply_combine_chain, 
                        object = object,
                        .parallel = parallel, expr = parse(text = expr),
                        values = values, monitor = monitor)

  object <- Reduce(bind_chains, object)
  
  check_no_missing_values(object)
}
