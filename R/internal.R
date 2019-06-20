
variables <- function(expr) {
  all.vars(parse(text = expr))
}

is_NA <- function(x) {
  identical(x, NA)
}