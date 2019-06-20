context("derive")

test_that("derive.mcmcarray", {
  mcmc_list <- coda::as.mcmc.list(subset(mcmcr::mcmcr_example, 1:2, 1:10))
  
  expect_warning(mcmc_derive(mcmc_list, "gamma <- alpha + beta"), 
                 "the following parameter was not in expr and so was dropped from object: 'sigma'")
  derived <- mcmc_derive(mcmc_list, "gamma <- alpha + beta", silent = TRUE)
  expect_identical(parameters(derived), "gamma")
  expect_identical(nchains(derived), 2L)
  expect_identical(niters(derived), 10L)
  expect_identical(nterms(derived), 4L)
})

test_that("derive.mcmcr simple", {
  mcmcr <- subset(mcmcr::mcmcr_example, 1:2, 1:10)

  expect_warning(mcmc_derive(mcmcr, "gamma <- alpha + beta"), 
                 "the following parameter was not in expr and so was dropped from object: 'sigma'")
  
  derived <- mcmc_derive(mcmcr, "gamma <- alpha + beta", silent = TRUE)
  expect_identical(parameters(derived), "gamma")
  expect_identical(nchains(derived), 2L)
  expect_identical(niters(derived), 10L)
  expect_identical(nterms(derived), 4L)
})

test_that("derive.mcmcr more complex", {
  
  mcmcr <- subset(mcmcr::mcmcr_example, 1:2, 1:10)
  
  expr <- "
    gamma <- alpha + beta
  alpha2 <- alpha * 2
  znot <- alpha * 2
  for(i in seq_along(x)) {
    alpha3[i] <- alpha[1] * x[i]
  }
  "
  
  values <- list(x = 2:10)
  
  derived <- mcmc_derive(mcmcr, expr, values = values, monitor = "^g|^a", silent = TRUE)
  
  expect_identical(parameters(derived), c("alpha2", "alpha3", "gamma"))
  expect_identical(nchains(derived), 2L)
  expect_identical(niters(derived), 10L)
  expect_identical(nterms(derived), 15L)
  
  expect_error(mcmc_derive(mcmcr, expr, values = values, monitor = "something",
                           silent = TRUE), 
               "monitor 'something' must match at least one of the following variables in expr: 'gamma', 'alpha2', 'znot', 'i' or 'alpha3'")
})

test_that("derive.mcmcr matrix", {
  mcmcr <- subset( mcmcr::mcmcr_example, 1:2, 1:10, parameters = "beta")
  
  expr <- "
    x <- Z
    for(i in 1:nrow(beta)) {
      for(j in 1:ncol(beta)) {
        x[i,j] <- beta[i,j]
      }
    }
  "
  
  Z <- matrix(0, 2, 2)
  
  values <- list(Z = Z)
  
  derived <- mcmc_derive(mcmcr, expr, values = values, monitor = "x")
  
  names(derived) <- "beta"
  expect_identical(derived, mcmcr)
})

test_that("derive.mcmcr problems", {
  
  #  expect_error(mcmc_derive(mcmcr_example, expr, values = list(x = NA), monitor = "alpha3"), paste0("monitor 'alpha3' must not include missing values in expr\n\n    gamma "))

})

