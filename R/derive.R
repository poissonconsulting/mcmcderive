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

derive <- function(object, expr, values, monitor) {
  # need to optimize this part...
  object <- lapply(1:nchains(object), derive_chain, object = object,
                   expr = parse(text = expr),
                   values = values, monitor = monitor)
  
  object <- Reduce(bind_chains, object)
  
  check_no_missing_values(object)
}

derive2 <- function(object, expr, values, monitor) {

  object <- lapply(object, unclass)
  values <- c(object, values)
  print(values)
  print(expr)
  object <- within(values, eval(parse(text = expr)))
  print(object)
  object <- object[monitor]

  # missing values
  object <- lapply(object, set_class, "mcmcarray")
  object <- as.mcmcr(object)
  check_mcmcr(object)
  object
}
