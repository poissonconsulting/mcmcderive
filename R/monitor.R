monitor_variables <- function(monitor, values) {
  variables <- names(values[vapply(values, is_NA, TRUE)])
  
  match <- variables[grepl(monitor, variables)]
  if(!length(match)) {
    err("`monitor` '", monitor, 
        "' must match at least one of the following variables in expr: ", 
        cc(variables, " or "))
  }
  sort(match)
}
