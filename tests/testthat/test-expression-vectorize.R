test_that("simple expr with iteration var replaced and for loop removed", {
  expect_snapshot(
    expression_vectorize(rlang::expr(
      for(i in 1:nObs) {log(eCount[i]) <- b0}
    ))
  )
})

test_that("avoiding double braces (#21)", {
  expect_snapshot(
    expression_vectorize(rlang::expr({
      for (i in 1:10) {
        x[i] <- 1
        y[i] <- 2
      }
    }))
  )
})

test_that("iteration var replaced with squared term", {
  expect_snapshot(
    expression_vectorize(rlang::expr(
      for(i in 1:length(LogLength)) {
        eWeightLength[i] <- bWeightLength + bDayte * Dayte[i] + bDayte2 * Dayte[i]^2
      }
    ))
  )
})

test_that("iteration var replaced with prediction, fit and residual term ", {
  expect_snapshot(
    expression_vectorize(rlang::expr(
      for(i in 1:nObs) {
        log(prediction[i]) <-  bWeight + eWeightLength[i] * LogLength[i]
        fit[i] <- log(prediction[i])
        residual[i] <- res_lnorm(Weight[i], fit[i], sWeight)
      }
    ))
  )
})

test_that("iteration var replaced and cbind added to arrays", {
  expect_snapshot(
    expression_vectorize(rlang::expr(
      for(i in 1:nObs) {
        log(eCount[i]) <- b0 + bYear * Year[i] + bAnnual[Annual[i]] + bSiteAnnual[Site[i], Annual[i]]
        fit[i] <- eCount[i]
        residual[i] <- res_gamma_pois(Count[i], fit[i], sSiteAnnualQuadrat)
      }
    ))
  )
})

test_that("expr with mutli lines", {
  expect_snapshot(
    expression_vectorize(rlang::expr(
      for(i in 1:nObs) {
        log(eCount[i]) <- b0 + bYear * Year[i] + bKelpLine * KelpLine[i] + bSite[Site[i]] + bAnnual[Annual[i]] + bSiteAnnual[Site[i], Annual[i]]
        log(eCountKelpline[i]) <- b0 + bKelpLine + bYear * Year[i] + bAnnual[Annual[i]] + bSite[Site[i]] + bSiteAnnual[Site[i],Annual[i]]
        log(eCountBarren[i]) <- b0 + bYear * Year[i] + bAnnual[Annual[i]] + bSite[Site[i]] + bSiteAnnual[Site[i],Annual[i]]
      }
    ))
  )
})

test_that("expr with non iteration", {
  expect_snapshot(
    expression_vectorize(rlang::expr(
      {max_age <- round(bA_max)
      age <- 1:max_age
      length <- bL_inf * (1 - exp(-bk * (age - ba0)))
      fecundity <- 10^(-5 + 3 * log(fl2tl(length), base = 2))
      survival <- c(1, rep(bS_J, 2), rep(bS_A, max_age - 1))
      survivorship <- cumprod(survival)
      maturity <- age^14 / (bAs^14 + age^14)
      eggs <- survivorship * maturity * fecundity * 0.50
      prediction <- 1/sum(eggs)}
    ))
  )
})

test_that("sum() inside the expression leaves the for loop unchanged", {
  expect_snapshot(
    expression_vectorize(rlang::expr(
      for (i in 1:length(Year)) {
        eGrowth[i] <- max(0, (bLinf - LengthAtRelease[i]) * (1 - exp(-sum(eK[Year[i]:(Year[i] + dYears[i] - 1)]))))
      }
    ))
  )
})

test_that("cbind with var and constant", {
  expect_snapshot(
    expression_vectorize(rlang::expr(
      for(i in 1:nObs) {
        eDensity[i] <- bDensity[Island[i],Day[i]]
        ePopn[i] <- bPopn[Island[i],1]
      }
    ))
  )
})

test_that("code before for loop", {
  expect_snapshot(
    expression_vectorize(rlang::expr(
      {
        b0 <- 2
        for(i in 1:nObs) {log(eCount[i]) <- b0}
      }
    ))
  )
})

test_that("more than two dimensions", {
  expect_snapshot(
    expression_vectorize(rlang::expr(
      for(i in 1:nObs) {
        log(eCount[i]) <- b0 + bKelpLine * KelpLine[i] + bYear * Year[i] + bSite[Site[i]] + bSiteAnnual[Site[i], Annual[i]] +  bAnnual[Annual[i]]
        dpois(eCount[i] * bSiteAnnualQuadrat[Site[i], Annual[i], Quadrat[i]])
      }
    ))
  )
})
