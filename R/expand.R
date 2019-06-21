expand_indexed_parameter <- function(expr, parameter) {
  pattern <- p0("(?<=\\b", parameter, ")\\s*\\[([^\\]]+)")
  gsub(pattern, "[\\1,,", expr, perl = TRUE)
}

expand_indexed_parameters <- function(expr, object, monitor) {
  parameters <- c(parameters(object), monitor)
  
  for(parameter in parameters)
    expr <- expand_indexed_parameter(expr, parameter)
  expr
}
