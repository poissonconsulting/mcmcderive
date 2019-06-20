factors_to_integers <- function(values, warn = FALSE) {
  is_factor <- vapply(values, is.factor, TRUE)
  values[is_factor] <- lapply(values[is_factor], as.integer)
  values
}

convert_values <- function(values) {
  check_named(values, unique = TRUE)
  values <- factors_to_integers(values)
  values
}