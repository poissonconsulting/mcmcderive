
variables <- function(expr) {
  all.vars(parse(text = expr))
}

is_NA <- function(x) {
  identical(x, NA)
}

set_class <- function(x, class) {
  class(x) <- class
  x
}


derive2 <- function(object, expr, values, monitor) {

  object <- lapply(object, as.mcarray)
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

