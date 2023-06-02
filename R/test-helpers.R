save_txt <- function(x) {
  path <- tempfile(fileext = ".text")
  writeLines(x, path)
  path
}

expect_snapshot_expression <- function(x, name) {
  testthat::expect_identical(mode(x), "call")
  
  x <- deparse(x)
  path <- save_txt(x)
  testthat::expect_snapshot_file(path, paste0(name, ".txt"))
}