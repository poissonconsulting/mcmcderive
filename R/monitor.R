monitor_variables <- function(monitor, values) {
  variables <- names(values[vapply(values, is.na, TRUE)])
  
  match <- variables[grepl(monitor, variables)]
  if(!length(match)) {
    err(co_or(match, p0(
      "monitor '", monitor, 
      "' must match at least one of the following variable%s in expr: %c")))
  }
  sort(match)
}