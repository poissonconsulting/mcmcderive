context("expand")

test_that("expand",{
  expect_identical(expand_indexed_parameter("par[1] <- par[2]", "par"),
                   "par[,,1] <- par[,,2]")
  
  expect_identical(expand_indexed_parameter("par[] <- par[,]", "par"),
                   "par[] <- par[,,,]")
  expect_identical(expand_indexed_parameter("par[] <- par[,]", "par2"),
                   "par[] <- par[,]")
  expect_identical(expand_indexed_parameter("ppar[3] <- par[,]", "par"),
                   "ppar[3] <- par[,,,]")
})
