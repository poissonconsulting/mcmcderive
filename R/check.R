check_no_missing_values <- function(object) {
  missing <- vapply(object, anyNA, TRUE)
  
  if(any(missing))
    err("the following derived parameters include missing values: ", 
        cc(names(missing[missing]), " and "))
  TRUE
}
