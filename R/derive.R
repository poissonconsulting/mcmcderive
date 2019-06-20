derive_sample <- function(i, object, expr, values, monitor) {
  object <- subset(object, iterations = i)
  object <- estimates(object)
  object <- c(object, values)
  object <- within(object, eval(expr))
  object <- object[monitor]
  object <- lapply(object, function(x) { dim(x) <- c(1L, 1L, dims(x)); class(x) <- "mcmcarray"; x})
  
  class(object) <- "mcmcr"
  
  object
}

derive_chain <- function(i, object, expr, values, monitor) {
  object <- subset(object, chains = i)
  
  object <- lapply(1:niters(object), FUN = derive_sample, object = object,
                   expr = expr, values = values, monitor = monitor)
  object <- Reduce(bind_iterations, object)
  object
}
