context("derive")

test_that("derive", {

  mcmcr_example <- mcmcr::mcmcr_example
  mcmcr <- subset(mcmcr_example, 1:2, 1:10)

  derived <- mcmc_derive(mcmcr, "gamma <- alpha + beta")
  expect_identical(parameters(derived), "gamma")
  expect_identical(nchains(derived), 2L)
  expect_identical(niters(derived), 10L)
  expect_identical(nterms(derived), 4L)
})

test_that("derive", {

  mcmcr_example <- mcmcr::mcmcr_example
  mcmcr <- subset(mcmcr_example, 1:2, 1:10)

  expr <- "
    gamma <- alpha + beta
  alpha2 <- alpha * 2
  znot <- alpha * 2
  for(i in seq_along(x)) {
    alpha3[i] <- alpha[1] * x[i]
  }
  "

  values <- list(x = 2:10)

  derived <- mcmc_derive(mcmcr, expr, values = values, monitor = "^g|^a")

  expect_identical(parameters(derived), c("alpha2", "alpha3", "gamma"))
  expect_identical(nchains(derived), 2L)
  expect_identical(niters(derived), 10L)
  expect_identical(nterms(derived), 15L)

  expect_error(mcmc_derive(mcmcr_example, expr, values = values, monitor = "something"), paste0("monitor 'something' must match at least one new variable in expr\n\n    gamma "))

    expect_error(mcmc_derive(mcmcr_example, expr, values = list(x = NA), monitor = "alpha3"), paste0("monitor 'alpha3' must not include missing values in expr\n\n    gamma "))
})

test_that("derive matrix", {

  mcmcr_example <- mcmcr::mcmcr_example
  mcmcr <- subset(mcmcr_example, 1:2, 1:10, parameters = "beta")

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

test_that("derive.mcmc_data", {
  
  mcmc_data <- mcmcdata::mcmc_data_example
  mcmc_data$data$count <- 1:2
  derived <- mcmc_derive(mcmc_data, expr = "prediction <- estimate * count")
  expect_is(derived, "mcmc_data")
  
  mcmc_data <- coef(mcmc_data)
  derived <- coef(derived)
  
  expect_identical(mcmc_data$estimate * 1:2, derived$estimate)
})
